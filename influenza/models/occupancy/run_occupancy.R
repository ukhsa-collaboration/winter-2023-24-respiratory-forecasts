# Script to run influenza occupancy

#### DESCRIPTION ####
#
# Take influenza admissions model and convert to occupancy model
#
# Method
#   Take forecasted samples for admissions in the next two weeks
#   Take posterior samples for fixed discharge probability
#   Combine forecast samples and posterior samples to calculate occupancy levels
#


# ENVIRONMENT SET UP
wd <- system("echo $(git rev-parse --show-toplevel)", intern = TRUE)
setwd(wd)
Sys.setenv("R_BOX_PATH" = fs::path(rprojroot::find_root(rprojroot::is_git_root), "src", "R"))
# install all required packages for the modelling
source(paste0(wd, "/influenza/models/src/depends.R"))
source(paste0(wd, "/influenza/models/occupancy/models/discharge_region.R"))
source(paste0(wd, "/influenza/models/occupancy/models/admissions_to_occupancy.R"))

# SET GLOBAL SEED for reproducibility
set.seed(8675309)

config_path <- "./influenza/models/occupancy/occupancy_config.yaml"
config <- yaml::read_yaml(config_path)
current_time <- gsub(" ", "_", glue::glue("{Sys.time()}_{lubridate::wday(Sys.time(), label=T)}"))

output_path <- glue::glue("{wd}/influenza/outputs/occupancy/{current_time}/")
dir.create(output_path, recursive = T)

# get forecasts path
admissions_forecast_path <- s3$find_latest_file(uri = config$files$admissions_forecast_data,
  pattern = config$files$admissions_forecast_pattern)

# get raw training data for occupancy path
pipeline_path <- s3$find_latest_file(uri = config$files$pipeline_data,
  pattern = config$files$pipeline_pattern)

overall_params <- config$overall_params

#### LOAD DATA ####
admissions_forecast <- aws.s3::s3read_using(
  vroom::vroom,
  object = admissions_forecast_path,
  show_col_types = FALSE
) |>
  dplyr::filter(age_group == "all", location_level == "region")

# check how recent the data is
print(noquote(paste(
  "The difference between the most recent training data and today is:",
  Sys.Date() - max(admissions_forecast$date), "days.")))
print(noquote("[Expect a negative number, ~ -11, if you're running this live]"))
user_check$user_check(lead_message = glue::glue("Is this ok?"))


# some of our overall params are defined upstream from the admissions model
overall_params$forecast_horizon <- unique(admissions_forecast$forecast_horizon)
overall_params$n_lookbacks <- length(unique(na.omit(admissions_forecast$prediction_start_date)))
overall_params$max_lookback <- max(unique(na.omit(admissions_forecast$prediction_start_date)))

# Load in the most recent training data
training_data <- aws.s3::s3read_using(
  vroom::vroom,
  object = pipeline_path,
  show_col_types = FALSE
) |>
  dplyr::mutate(date = lubridate::ymd(date)) |>
  # keep only recent data
  dplyr::filter(date > "2023-01-01") |>
  dplyr::mutate(total_beds = rowSums(dplyr::across(dplyr::starts_with("total_")), na.rm = T),
    occupancy = rowSums(dplyr::across(dplyr::starts_with("occupancy_")), na.rm = T)) |>
  dplyr::select(
    date, icb_name, nhs_region_name, population, total_beds, admissions, occupancy
  ) |>
  dplyr::filter(date < overall_params$max_lookback) |>
  # aggregate to ICBs
  dplyr::group_by(date, icb_name, nhs_region_name) |>
  dplyr::summarise(
    population = ifelse(all(is.na(admissions)), NA, sum(population[!is.na(admissions)], na.rm = TRUE)),
    dplyr::across(-c("population"),
      ~ ifelse(all(is.na(.)), NA, sum(., na.rm = TRUE))),
    .groups = "keep") |>
  dplyr::ungroup() |>
  # TODO temporary: doesnt like occupancy being zero too much
  dplyr::mutate(occupancy = case_when(occupancy == 0 ~ 0.001,
    T ~ occupancy))


training_data <- training_data |>
  mutate(admissions = ifelse(icb_name == "NHS Frimley Integrated Care Board", 0, admissions))
training_data <- training_data |>
  mutate(occupancy = ifelse(icb_name == "NHS Frimley Integrated Care Board", 0, occupancy))

# check how recent the data is
print(noquote(paste(
  "The difference between the most recent training data and today is:",
  Sys.Date() - max(training_data$date), "days.")))
user_check$user_check(lead_message = glue::glue("Is this ok?"))

#### DISCHARGE MODEL ####

discharge_results <- run_discharge_region(training_data, overall_params)


#### DISCHARGE MODEL QA ####

# Code to sense check the model outputs
# lognormal_mu and lognormal_sigma define the discharge probability
# alpha is the conversion multiplier between arrival admissions and admissions
# beta is the error term on the gamma distribution
# the `y_hat` variables are the forecasted values historically to check fit

# check values
discharge_results$model$summary()

# check convergence
bayesplot::mcmc_trace(discharge_results$model$draws(), pars = c("lognormal_sigma"))
bayesplot::mcmc_trace(discharge_results$model$draws(), regex_pars = c("lognormal_mu"))
bayesplot::mcmc_trace(discharge_results$model$draws(), regex_pars = c("beta"))

##### REGIONAL #####

# show how the model fit to the data
discharge_results$predictions %>%
  ggplot() +
  geom_line(aes(x = date, y = occupancy, col = "occupancy")) +
  geom_line(aes(x = date, y = admissions, col = "admissions")) +
  geom_line(aes(x = date, y = pi_50, col = "fitted occupancy")) +
  geom_ribbon(aes(x = date, ymin = pi_5, ymax = pi_95), fill = "darkgreen", alpha = 0.3) +
  facet_wrap(~nhs_region_name) +
  scale_colour_manual("",
    breaks = c("occupancy", "fitted occupancy", "admissions"),
    values = c("maroon", "darkgreen", "orange"))


# explore posterior parameters
los_regional_samples <- expand.grid(t = seq(0, overall_params$max_lag, 0.1),
  nhs_region_name = unique(discharge_results$training_data$nhs_region_name)) |>
  dplyr::left_join(discharge_results$parameters, by = "nhs_region_name") |>
  # probability density
  dplyr::mutate(p = dlnorm(t, lognormal_mu, lognormal_sigma)) |>
  # cumulative probability density
  dplyr::mutate(cd = plnorm(t, lognormal_mu, lognormal_sigma))

los_regional_summary <- los_regional_samples |>
  dplyr::group_by(t, nhs_region_name) |>
  dplyr::summarise(
    p_50 = quantile(p, 0.5),
    p_5 = quantile(p, 0.05),
    p_95 = quantile(p, 0.95),
    c_50 = quantile(cd, 0.5),
    c_5 = quantile(cd, 0.05),
    c_95 = quantile(cd, 0.95)
  ) |>
  dplyr::ungroup()


# Some discharge distribution parameters do depend on the region, so we expect variation
# should be a right skewed distribution, ideally in a nice /‾\_ shape
# There is some pooling, but mostly each region will be independently fit and differ in distribution for los
print(outputs$los_plotter(
  los_regional_summary,
  discharge_results,
  geography = "region",
  output_path = output_path,
  disease = overall_params$disease
))

narratives$los_narrative_txt_output(
  data = los_regional_samples,
  geography = "region",
  disease = "influenza",
  output_path = output_path
)

# should be monatonically increasing, reaching 1 near the end of the max_lag
print(los_regional_summary %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = t, y = c_50)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = t, ymin = c_5, ymax = c_95),
    alpha = 0.4, fill = "darkgreen") +
  ggplot2::ylab("cumulative p discharge") +
  ggplot2::xlab("days since admission") +
  ggplot2::ggtitle("Likelihood of discharge over time") +
  ggplot2::ylim(0, 1) +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~nhs_region_name))

# plot the fit parameters by region
# are the ones we expect to vary varying? At the moment, beta and lognormal_mu are varying (05/10/2023)
print(discharge_results$parameters |>
  tidyr::pivot_longer(cols = c("lognormal_mu", "lognormal_sigma", "beta"),
    names_to = "parameter_name", values_to = "parameter_value") |>
  dplyr::group_by(nhs_region_name, parameter_name) |>
  dplyr::summarise(cri_50 = quantile(parameter_value, 0.5),
    cri_95 = quantile(parameter_value, 0.95),
    cri_05 = quantile(parameter_value, 0.05)) |>
  dplyr::ungroup() |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(y = parameter_name, x = cri_50, color = parameter_name)) +
  ggplot2::geom_linerange(ggplot2::aes(xmin = cri_05, xmax = cri_95, y = parameter_name, color = parameter_name)) +
  ggplot2::facet_wrap(~nhs_region_name, ncol = 1) +
  ggplot2::xlim(c(0, NA)) +
  ggplot2::xlab("parameter value") +
  ggplot2::ylab("") +
  ggplot2::theme_bw())

##### NATIONAL #####

# For each region:
# find mean population across training period
# calculate number of samples proportional to this to use in national average
regional_n_samples <- training_data |>
  dplyr::filter(date <= max(discharge_results$predictions$date),
    date >= min(discharge_results$predictions$date)) |>
  dplyr::filter(!is.na(occupancy) & !is.na(total_beds) & !is.na(admissions)) |>
  dplyr::group_by(nhs_region_name) |>
  dplyr::summarise(
    population = median(population)) |>
  dplyr::mutate(
    n_samples = round(population / max(population) * overall_params$n_pi_sample)
  )

los_national_samples <- expand.grid(t = seq(0, overall_params$max_lag, 0.1),
  nhs_region_name = unique(discharge_results$training_data$nhs_region_name)) |>
  dplyr::left_join(discharge_results$parameters, by = "nhs_region_name") |>
  # number of samples weighted by region
  dplyr::left_join(regional_n_samples, by = "nhs_region_name") |>
  dplyr::filter(.draw <= n_samples) |>
  # probability density
  dplyr::mutate(p = dlnorm(t, lognormal_mu, lognormal_sigma)) |>
  # cumulative probability density
  dplyr::mutate(cd = plnorm(t, lognormal_mu, lognormal_sigma))

los_national_summary <- los_national_samples |>
  dplyr::group_by(t) |>
  dplyr::summarise(
    p_50 = quantile(p, 0.5),
    p_5 = quantile(p, 0.05),
    p_95 = quantile(p, 0.95),
    c_50 = quantile(cd, 0.5),
    c_5 = quantile(cd, 0.05),
    c_95 = quantile(cd, 0.95)
  ) |>
  dplyr::ungroup()

# Plot
print(outputs$los_plotter(
  los_national_summary,
  discharge_results,
  geography = "nation",
  output_path = output_path,
  disease = overall_params$disease
))

# Narrative
narratives$los_narrative_txt_output(
  data = los_national_samples,
  geography = "nation",
  disease = "influenza",
  output_path = output_path
)

# enforce a user check on the stan model
user_check$user_check(lead_message = glue::glue(
  "Do the QA plots (in the plots pane) for the fit stan model look sensible?"))


#### OCCUPANCY MODEL ####
occupancy_results <- run_occupancy_region(
  admissions_data = admissions_forecast,
  discharge_data = discharge_results,
  overall_params = overall_params)



#### FORMAT ####

# national samples
occupancy_samples <- discharge_results$training_data |>
  dplyr::select(-admissions) |>
  dplyr::full_join(occupancy_results$forecast_occupancy, by = c("date", "nhs_region_name")) |>
  dplyr::rename(.value = .pred,
    target = occupancy,
    population = total_beds) |>
  dplyr::group_by(nhs_region_name) |>
  dplyr::arrange(date) |>
  dplyr::select(-r) |>
  tidyr::fill(population) |>
  dplyr::ungroup()

# national summary
nation_occupancy_formatted_summary <- occupancy_samples |>
  ensemble$ensemble_from_samples(
    remove_identifiers = c("nhs_region_name"),
    overall_params = overall_params,
    model_name = overall_params$model_name
  ) |>
  # convert to a percentage rate
  dplyr::mutate(
    dplyr::across(dplyr::starts_with("pi_"), ~ 100 * . / population),
    target = 100 * target / population
  ) |>
  dplyr::rename(target_value = target) |>
  dplyr::mutate(
    location = "England",
    location_level = "nation",
    age_group = "all",
    model = overall_params$model_name,
    age_group_granularity = "none",
    target_name = "influenza_bed_occupancy_rate",
    forecast_horizon = overall_params$forecast_horizon)




# regional summary
region_occupancy_formatted_summary <- occupancy_samples |>
  ensemble$ensemble_from_samples(
    remove_identifiers = c(),
    overall_params = overall_params,
    model_name = overall_params$model_name
  ) |>
  # convert to a percentage rate
  dplyr::mutate(
    dplyr::across(dplyr::starts_with("pi_"), ~ 100 * . / population),
    target = 100 * target / population
  ) |>
  dplyr::rename(target_value = target,
    location = nhs_region_name) |>
  dplyr::mutate(
    location_level = "region",
    age_group = "all",
    model = overall_params$model_name,
    age_group_granularity = "none",
    target_name = "influenza_bed_occupancy_rate",
    forecast_horizon = overall_params$forecast_horizon)

occupancy_formatted_summary <- dplyr::bind_rows(
  nation_occupancy_formatted_summary,
  region_occupancy_formatted_summary
)

checks$check_forcast_format_summary(occupancy_formatted_summary)


# regional samples
occupancy_formatted_samples <- occupancy_samples |>
  dplyr::rename(target_value = target,
    location = nhs_region_name) |>
  dplyr::mutate(
    location_level = "region",
    age_group = "all",
    model = overall_params$model_name,
    age_group_granularity = "none",
    target_name = "influenza_bed_occupancy_rate",
    forecast_horizon = overall_params$forecast_horizon)

# takes a while as checking each
checks$check_forcast_format_sample(occupancy_formatted_samples)

#### PLOTTING ####

# nation & regions
outputs$projections_plotter(
  plots_include = list(c("lookbacks", "rag")),
  data = occupancy_formatted_summary,
  target_name = "bed_occupancy_rate_(%)",
  model_name = overall_params$model_name,
  geography = c("nation", "region"),
  output_path = output_path,
  disease = config$overall_params$disease,
  peaks_data = config$overall_params$show_peaks
)


outputs$rag_plotter(
  data = occupancy_formatted_summary,
  target_name = "bed_occupancy_rate_(%)",
  model_name = overall_params$model_name,
  geography = "nation",
  output_path = output_path,
  disease = config$overall_params$disease
)

#### NARRATIVE ####
narrative_output_path <- glue::glue("{output_path}/narrative")
dir.create(narrative_output_path, recursive = T)

narratives$narrative_txt_output(occupancy_formatted_summary,
  geography = "nation",
  age_granularity = "none",
  target_name = "bed_occupancy_rate",
  model_name = overall_params$model_name,
  disease = "influenza",
  rounding_level = 1,
  is_percent = T,
  output_path = narrative_output_path
)



#### SCORING ####
scoring_output_path <- glue::glue("{output_path}scoring")
dir.create(scoring_output_path, recursive = T)

# National scoring
scoring$score(
  data = occupancy_formatted_summary,
  geography = "nation",
  age_cohort_level = "none",
  model_names = overall_params$model_name,
  output_path = scoring_output_path
)

# Regional scoring
scoring$score(
  data = occupancy_formatted_summary,
  geography = "region",
  age_cohort_level = "none",
  model_names = overall_params$model_name,
  output_path = scoring_output_path
)

#### SAVE OUTPUT ####


data_output_path <- glue::glue("{output_path}/data")
dir.create(data_output_path, recursive = T)


file_name_summary <- glue::glue("all_models_predictions_summary_{current_time}.csv.gz")

s3$write_to_s3(
  data = occupancy_formatted_summary,
  s3_uri = glue::glue("PATH REMOVED/influenza/{overall_params$target_name}/{file_name_summary}"),
  local_path = glue::glue("{data_output_path}/{file_name_summary}")
)

file_name_samples <- glue::glue("all_models_predictions_samples_{current_time}.csv.gz")

s3$write_to_s3(
  data = occupancy_formatted_samples,
  s3_uri = glue::glue("PATH REMOVED/influenza/{overall_params$target_name}/{file_name_samples}"),
  local_path = glue::glue("{data_output_path}/{file_name_samples}")
)
