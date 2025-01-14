# Script to run RSV univariate models ensemble version1

#### DESCRIPTION ####
#
# RSV test model, running and output creation
#
#

# TODO
# clean up data processing?


#### ENVIRONMENT SET UP ####
wd <- system("echo $(git rev-parse --show-toplevel)", intern = TRUE)
setwd(wd)

# SET GLOBAL SEED for reproducibility
set.seed(8675309)

# options(dplyr.summarise.inform = FALSE)
# In case box is playing up:
# Sys.setenv("R_BOX_PATH" = fs::path(
#   rprojroot::find_root(rprojroot::is_git_root),
#   "src", "R"))

source(paste0(wd, "/rsv/models/src/depends.R"))
# may need to run again if gratia is installed
source(paste0(wd, "/rsv/models/src/mrf_structures.R"))
source(paste0(wd, "/rsv/models/src/population.R"))

box::use(
  box / redshift,
  box / s3,
  prj / checks,
  prj / splines,
  prj / intervals,
  prj / parallel,
  prj / ensemble,
  prj / write,
  prj / extract_objects,
  prj / narratives,
  prj / scoring,
  prj / user_check,
  prj / outputs,
  prj / run_model
)


#### CONFIGURATION ####

config_path <- "./rsv/models/admissions/rsv_admissions_config.yaml"
config <- yaml::read_yaml(config_path)


# Set up data and output paths
training_data_path <- s3$find_latest_file(uri = config$file_path$uri,
  pattern = config$file_path$pattern)

current_time <- gsub(" ", "_", glue::glue(
  "{Sys.time()}_{lubridate::wday(Sys.time(), label=T)}"))

output_path <- glue::glue("/rsv/outputs/admissions/{current_time}/")
output_path <- paste0(wd, output_path)

dir.create(output_path, recursive = T)

# add MRF structures to hyperparameters
config$hyperparams$hospital_cases$nb$age_group <- age_nb
config$hyperparams$hospital_cases$nb$nhs_region_name <- nhs_nb

# to convert to generic target name
target_name_sym <- dplyr::sym(config$overall_params$target_name)

#### DATA PREPROCESSING ####

# access lookups
rs <- redshift$data_model()

# bring in RSV pipeline
rsv_raw <- aws.s3::s3read_using(
  vroom::vroom,
  object = training_data_path
)
user_check$user_check(lead_message = glue::glue(
  "The difference between the most recent raw training data and today is: {Sys.Date() - max(rsv_raw$date)} days.\n Is this ok?"))



# define max lookback from the data
config$overall_params$max_lookback <- max(rsv_raw$date) + 1

sgss_clean <- rsv_raw |>
  tidyr::pivot_longer(cols = dplyr::starts_with("n_age_"),
    names_to = "age", values_to = "count") |>
  dplyr::mutate(age = as.integer(stringr::str_remove(age, "n_age_"))) |>
  # we cannot bin missing ages
  dplyr::filter(!is.na(age)) |>
  # age_breakdowns defined in mrf_structures.R; N.B. some combined after model
  dplyr::mutate(age_group = cut(age, age_breakdowns, right = FALSE)) |>
  dplyr::filter(!is.na(age_group)) |>
  dplyr::group_by(date, trust_code, icb_name, nhs_region_name, age_group) |>
  dplyr::summarise(count = sum(count)) |>
  dplyr::ungroup() |>
  dplyr::left_join(rsv_raw |> # re-add paediatric bed availability
    dplyr::select(date, trust_code, total_paediatric_beds_general_acute,
      total_paediatric_beds_critical_care),
  by = c("date", "trust_code")) |>
  dplyr::filter(date <= max(date),
    date < config$overall_params$max_lookback) |>
  dplyr::left_join(trust_age_population,
    by = c("trust_code", "age_group", "nhs_region_name"))

# generate all combinations of date, location, and age;
# which we need to predict with
regional_spine <- expand.grid(
  date = seq(min(sgss_clean$date), max(sgss_clean$date), by = "day"),
  nhs_region_name = unique(sgss_clean$nhs_region_name),
  age_group = unique(sgss_clean$age_group)
)

training_data <- sgss_clean |>
  # remove impact of trusts with only zero counts
  # first assume all missing are zero
  dplyr::mutate(count = dplyr::coalesce(count, 0)) |>
  # calculate the rolling sum by age group and trust (to work out if zero)
  dplyr::group_by(trust_code, age_group) |>
  dplyr::arrange(date) |>
  dplyr::mutate(trust_age_level_rolling_sum_count = zoo::rollapplyr(
    count, 90, sum, partial = TRUE, align = "right")) |>
  dplyr::ungroup() |>
  # get overall trust counts,
  # given we think misreporting is at a trust not trust:age level
  dplyr::group_by(trust_code, date) |>
  dplyr::mutate(trust_level_rolling_sum_count = sum(
    trust_age_level_rolling_sum_count, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  # lets remove any counts for trusts that don't meet our criteria at
  # a point in time so they don't contribute to the regional numerator.
  # NOTE: NAs and zeros are treated the same in this approach
  dplyr::mutate(count = dplyr::case_when(
    trust_level_rolling_sum_count == 0 ~ NA_real_,
    TRUE ~ count)
  ) |>
  # and make their populations low (but not zero) so they don't contribute to
  # the regional denominator
  dplyr::mutate(population = dplyr::case_when(
    trust_level_rolling_sum_count == 0 ~ 1, # can't put 0 as it messes offset up
    TRUE ~ population)
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(date, nhs_region_name, age_group) |>
  dplyr::summarise(target = sum(count, na.rm = T),
    population = sum(population, na.rm = T)) |>
  # TODO: work out how to handle missing trusts?
  dplyr::ungroup() |>
  dplyr::left_join(regional_spine, .,
    by = c("date",
      "nhs_region_name",
      "age_group")) |>
  dplyr::mutate(
    target = dplyr::coalesce(target, 0), # fill in missing (ie no tests) with 0
  )



# define lookback start dates
start_dates <- seq(from = as.Date(config$overall_params$max_lookback) -
  (config$overall_params$n_lookbacks) *
    config$overall_params$lookback_step_length,
to = as.Date(config$overall_params$max_lookback),
by = glue::glue("{config$overall_params$lookback_step_length} days"))

#### MODELLING ####
hospital_cases_outputs <- run_model$run_scripted_model(
  wd,
  model_name = "hospital_cases",
  training_data = training_data,
  overall_params = config$overall_params,
  start_dates = start_dates,
  required_covariates = config$required_covariates,
  model_hyperparams = config$hyperparams$hospital_cases)

hospital_cases_sample_predictions <- extract_objects$extract_from_list(
  hospital_cases_outputs)$sample_predictions |>
  dplyr::full_join(
    training_data |>
      dplyr::filter(date >= "2022-09-01") |>
      dplyr::select(date, nhs_region_name, age_group, target, population),
    by = c("nhs_region_name", "age_group", "date", "population")) |>
  # we are converting from the estimated rate to estimated counts using the true
  # population; bring in the true (total) denominator, not reported denominator.
  dplyr::left_join(
    trust_age_population |>
      dplyr::summarize(total_population = sum(population),
        .by = c("nhs_region_name", "age_group")),
    by = c("nhs_region_name", "age_group")) |>
  # adjust up the predictions
  dplyr::mutate(.value = round(total_population * (.value / population))) |>
  # adjust up the true values for the plots
  dplyr::mutate(target = round(total_population * (target / population))) |>
  dplyr::select(-total_population)

hospital_cases_models <- extract_objects$extract_from_list(
  hospital_cases_outputs)$models


hospital_cases_national_preds <- intervals$samples_to_quantiles(
  .sample_predictions = hospital_cases_sample_predictions,
  remove_identifiers = c("age_group", "nhs_region_name"),
  overall_params = config$overall_params,
  method = "quantile") |>
  dplyr::mutate(
    age_group = "all",
    age_group_granularity = "none",
    model = "hospital_cases",
    location = "England",
    location_level = "nation")

hospital_cases_regional_preds <- intervals$samples_to_quantiles(
  .sample_predictions = hospital_cases_sample_predictions,
  remove_identifiers = c("age_group"),
  overall_params = config$overall_params,
  method = "quantile") |>
  dplyr::mutate(
    age_group = "all",
    age_group_granularity = "none",
    model = "hospital_cases",
    location_level = "region") |>
  dplyr::rename(location = nhs_region_name)

hospital_cases_age_preds <- intervals$samples_to_quantiles(
  .sample_predictions = hospital_cases_sample_predictions |>
    dplyr::mutate(age_group = dplyr::case_when(
      age_group %in% c("[0,1)", "[1,2)") ~ "[0,2)",
      age_group %in% c("[2,5)") ~ "[2,5)",
      age_group %in% c("[5,18)") ~ "[5,18)",
      age_group %in% c("[18,65)") ~ "[18,65)",
      age_group %in% c("[65,75)") ~ "[65,75)",
      age_group %in% c("[75,85)", "[85,120)") ~ "[75,120)",
    )),
  remove_identifiers = c("nhs_region_name"),
  overall_params = config$overall_params,
  method = "quantile") |>
  dplyr::mutate(
    model = "hospital_cases",
    location = "England",
    location_level = "nation",
    age_group_granularity = "fine")


# we don't plot with coarse breakdowns,
# but will want to use the [0,5) for occupancy models
# NOTE: this takes a few mins which is odd, perhaps use filter?
hospital_cases_age_coarse_region_preds <- intervals$samples_to_quantiles(
  .sample_predictions = hospital_cases_sample_predictions |>
    dplyr::mutate(age_group = dplyr::case_when(
      age_group %in% c("[0,1)", "[1,2)", "[2,5)") ~ "[0,5)",
      age_group %in% c("[5,18)") ~ "[5,18)",
      age_group %in% c("[18,65)") ~ "[18,65)",
      age_group %in% c("[65,75)", "[75,85)", "[85,120)") ~ "[65,120)",
    ),
    )
  #|> dplyr::filter(age_group == "[0,5)") # just what's needed for occupancy?
  ,
  remove_identifiers = NULL,
  overall_params = config$overall_params,
  method = "quantile") |>
  dplyr::rename(location = nhs_region_name) |>
  dplyr::mutate(
    model = "hospital_cases",
    location_level = "region",
    age_group_granularity = "coarse")

hospital_cases_formatted <- dplyr::bind_rows(
  hospital_cases_national_preds,
  hospital_cases_regional_preds,
  hospital_cases_age_preds,
  hospital_cases_age_coarse_region_preds
) |>
  dplyr::mutate(
    forecast_horizon = config$overall_params$forecast_horizon,
    target_name = "rsv_hospital_cases"
  ) |>
  dplyr::rename(
    target_value = target
  )

checks$check_forcast_format_summary(hospital_cases_formatted)

#### PLOTTING ####
##### NATIONAL & REGIONAL - ALL AGES #####
# use c("lookbacks", "rag") below to show coloured projections
outputs$projections_plotter(
  plots_include = list("multiple_cis", c("lookbacks")),
  data = hospital_cases_formatted,
  target_name = "estimated admissions",
  model_name = "hospital_cases",
  days_plotted = config$overall_params$forecast_horizon +
    config$hyperparams$hospital_cases$training_length,
  geography = c("nation", "region"),
  age_granularity = "none", # the default
  y_limit = c(400, 150),
  peaks_data = config$overall_params$show_peaks,
  output_path = output_path,
  disease = config$overall_params$disease)

##### NATIONAL - AGE BREAKDOWN #####
outputs$projections_plotter(
  plots_include = list("multiple_cis", c("lookbacks")),
  data = hospital_cases_formatted,
  target_name = "estimated admissions",
  model_name = "hospital_cases",
  days_plotted = config$overall_params$forecast_horizon +
    config$hyperparams$hospital_cases$training_length,
  geography = "nation",
  age_granularity = "fine", # not turning this into a loop for a one time use.
  y_limit = 325, # accounting for this and geo at the same time would be hell.
  peaks_data = config$overall_params$show_peaks,
  output_path = output_path,
  disease = config$overall_params$disease)


#### NARRATIVE ####
narrative_output_path <- glue::glue("{output_path}/narrative")
dir.create(narrative_output_path, recursive = T)

# National narrative
narratives$narrative_txt_output(data = hospital_cases_formatted,
  target_name = "estimated admissions",
  model_name = "hospital_cases",
  geography = "nation",
  age_granularity = "none",
  disease = "RSV",
  output_path = narrative_output_path)


#### WRITE ####

file_name <- glue::glue("hospital_cases_predictions_summary_{current_time}.csv.gz")

data_output_path <- glue::glue("{output_path}/data")
dir.create(data_output_path, recursive = T)

s3$write_to_s3(
  data = hospital_cases_formatted,
  s3_uri = glue::glue(
    "PATH REMOVED/rsv/{config$overall_params$target_name}/{file_name}"),
  local_path = glue::glue("{data_output_path}/{file_name}")
)



#### SCORING ####
scoring_output_path <- glue::glue("{output_path}/scoring")
dir.create(scoring_output_path, recursive = T)

scoring$score(hospital_cases_formatted,
  geography = "nation",
  age_cohort_level = "fine",
  model_names = "hospital_cases",
  output_path = scoring_output_path)

scoring$score(hospital_cases_formatted,
  geography = "nation",
  age_cohort_level = "none",
  model_names = "hospital_cases",
  output_path = scoring_output_path)
