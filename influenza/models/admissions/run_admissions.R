# # # # # # # # # # #
#### DESCRIPTION ####
# # # # # # # # # # #
# Flu ensemble models, running and output creation

# TODO
# generate intervals via gratia
# scale data and predictions by true population sizes for all products (but not scoring)
# univariate model: tuning for spline k parameters, training length
# univariate model: test variations
# 111 online model: improve
# free scales option for regional plots?
# check if sp is being used (for nbobj??) and replace with sf (due to warning about sp being deprecated)

# ENVIRONMENT SET UP
wd <- system("echo $(git rev-parse --show-toplevel)", intern = TRUE)
setwd(wd)
Sys.setenv("R_BOX_PATH" = fs::path(rprojroot::find_root(rprojroot::is_git_root), "src", "R"))
# install all required packages and functions
source(paste0(wd, "/influenza/models/src/depends.R"))
source(paste0(wd, "/influenza/models/src/format.R"))

rs <- redshift$data_model()

# SET GLOBAL SEED for reproducibility
set.seed(8675309)


# # # # # # # # # # # #
#### CONFIGURATION ####
# # # # # # # # # # # #

config_path <- "./influenza/models/admissions/influenza_admissions_config.yaml"
config <- yaml::read_yaml(config_path)

training_data_path <- s3$find_latest_file(
  uri = config$files$pipeline_path,
  pattern = config$files$pipeline_pattern)

current_time <- gsub(" ", "_", glue::glue("{Sys.time()}_{lubridate::wday(Sys.time(), label=T)}"))
output_path <- glue::glue("{wd}/influenza/outputs/admissions/{current_time}/")
dir.create(output_path, recursive = TRUE)

# define config from yaml file
overall_params <- config$overall_params

overall_params$threshold_rates <- list(
  upper_rate = config$thresholds[[overall_params$target_name]],
  lower_rate = -config$thresholds[[overall_params$target_name]]
)

models_include <- overall_params$models_include
ensemble_include <- ifelse(length(models_include) > 1, TRUE, FALSE)


# # # # # # # # # #
#### LOAD DATA ####
# # # # # # # # # #

target_name_sym <- rlang::sym(overall_params$target_name)

training_data <- aws.s3::s3read_using(
  vroom::vroom,
  object = training_data_path,
  show_col_types = FALSE
) |>
  dplyr::mutate(date = lubridate::ymd(date)) |>
  # keep only recent data
  dplyr::filter(date > "2022-10-01") |>
  dplyr::mutate(target = {{ target_name_sym }}) |>
  # aggregate to ICBs
  dplyr::select(-trust_code, -trust_name) |>
  dplyr::group_by(date, icb_name, nhs_region_name, across(starts_with("prov_"))) |>
  # reporting population consists of trusts that report on a given day, or NA if no trusts
  dplyr::summarise(
    # TODO true_population column not currently used or saved in output
    true_population = sum(population, na.rm = TRUE),
    population = ifelse(all(is.na(target)), NA, sum(population[!is.na(target)], na.rm = TRUE)),
    dplyr::across(-c("population"),
      ~ ifelse(all(is.na(.)), NA, sum(., na.rm = TRUE))),
    .groups = "keep") |>
  dplyr::ungroup()

training_data <- training_data |>
  mutate(target = ifelse(icb_name == "NHS Frimley Integrated Care Board", 0, admissions))
training_data <- training_data |>
  mutate(occupancy_general_acute = ifelse(icb_name == "NHS Frimley Integrated Care Board", 0, occupancy_general_acute))
training_data <- training_data |>
  mutate(occupancy_critical_care = ifelse(icb_name == "NHS Frimley Integrated Care Board", 0, occupancy_critical_care))


# check how recent the data is
user_check$user_check(lead_message = glue::glue(
  "The difference between the most recent data and today is: {Sys.Date() - max(training_data$date)} days.\n Is this ok?"))

overall_params$max_lookback <- max(training_data$date) + 1

# define lookback start dates
start_dates <- seq(from = as.Date(overall_params$max_lookback) - (overall_params$n_lookbacks - 1) * overall_params$lookback_step_length,
  to = as.Date(overall_params$max_lookback),
  by = glue::glue("{overall_params$lookback_step_length} days"))

# get spatial network object
nbobj <- aws.s3::s3read_using(
  readRDS,
  object = config$files$spatial_network_path
)

# # # # # # # # # # #
#### UNIVARIATE ####
# # # # # # # # # #

if ("univariate" %in% models_include) {

  univariate_outputs <- run_model$run_scripted_model(wd,
    model_name = "univariate",
    training_data = training_data,
    overall_params = overall_params,
    start_dates = start_dates,
    required_covariates = config$required_covariates,
    model_hyperparams = config$hyperparams$univariate)
  # parallel::stopCluster(cl)


  univariate_sample_predictions <- extract_objects$extract_from_list(univariate_outputs)$sample_predictions |>
    dplyr::full_join(training_data |>
      dplyr::select(date, icb_name, nhs_region_name, target),
    dplyr::join_by("date", "icb_name", "nhs_region_name"))

  univariate_models <- extract_objects$extract_from_list(univariate_outputs)$models

  univariate_national_preds <- intervals$samples_to_quantiles(
    .sample_predictions = univariate_sample_predictions,
    remove_identifiers = c("nhs_region_name", "icb_name"),
    overall_params = overall_params) |>
    dplyr::mutate(
      model = "univariate",
      location = "England",
      location_level = "nation")

  univariate_regional_preds <- intervals$samples_to_quantiles(
    .sample_predictions = univariate_sample_predictions,
    remove_identifiers = c("icb_name"),
    overall_params = overall_params) |>
    dplyr::mutate(model = "univariate",
      location_level = "region") |>
    dplyr::rename(location = nhs_region_name)

  univariate_icb_preds <- intervals$samples_to_quantiles(
    .sample_predictions = univariate_sample_predictions,
    remove_identifiers = c("nhs_region_name"),
    overall_params = overall_params) |>
    dplyr::mutate(model = "univariate",
      location_level = "icb") |>
    dplyr::rename(location = icb_name)

  univariate_formatted <- dplyr::bind_rows(
    univariate_national_preds,
    univariate_regional_preds,
    univariate_icb_preds
  ) |>
    format_influenza_outputs()

  checks$check_forcast_format_summary(univariate_formatted)

  rm(univariate_outputs,
    univariate_models,
    univariate_national_preds,
    univariate_regional_preds,
    univariate_icb_preds)
  gc()
}



# # # # # # # # # # # #
####  111 ONLINE  ####
# # # # # # # # # # #

if ("online" %in% models_include) {

  online_outputs <- run_model$run_scripted_model(
    wd,
    model_name = "online",
    training_data = training_data,
    overall_params = overall_params,
    start_dates = start_dates,
    required_covariates = config$required_covariates,
    model_hyperparams = config$hyperparams$online)
  # parallel::stopCluster(cl)


  online_sample_predictions <- extract_objects$extract_from_list(online_outputs)$sample_predictions |>
    dplyr::full_join(training_data |>
      dplyr::filter(date >= "2023-01-01") |>
      dplyr::select(date, nhs_region_name, icb_name, target, population),
    by = c("nhs_region_name", "icb_name", "date", "population"))
  online_models <- extract_objects$extract_from_list(online_outputs)$models

  online_national_preds <- intervals$samples_to_quantiles(
    .sample_predictions = online_sample_predictions,
    remove_identifiers = c("icb_name", "nhs_region_name"),
    overall_params = overall_params) |>
    dplyr::mutate(
      model = "online",
      location = "England",
      location_level = "nation")

  online_regional_preds <- intervals$samples_to_quantiles(
    .sample_predictions = online_sample_predictions,
    remove_identifiers = c("icb_name"),
    overall_params = overall_params) |>
    dplyr::mutate(model = "online",
      location_level = "region") |>
    dplyr::rename(location = nhs_region_name)

  online_icb_preds <- intervals$samples_to_quantiles(
    .sample_predictions = online_sample_predictions,
    remove_identifiers = c("nhs_region_name"),
    overall_params = overall_params) |>
    dplyr::mutate(model = "online",
      location_level = "icb") |>
    dplyr::rename(location = icb_name)

  online_formatted <- dplyr::bind_rows(
    online_national_preds,
    online_regional_preds,
    online_icb_preds
  ) |>
    format_influenza_outputs()

  checks$check_forcast_format_summary(online_formatted)

  rm(online_outputs,
    online_models,
    online_national_preds,
    online_regional_preds,
    online_icb_preds)
  gc()
}

# # # # # # # # # # # #
#### GOOGLE HEALTH ####
# # # # # # # # # # # #

if ("gtrends" %in% models_include) {

  gtrends_outputs <- run_model$run_scripted_model(
    wd,
    model_name = "gtrends",
    training_data = training_data,
    overall_params = overall_params,
    start_dates = start_dates,
    required_covariates = config$required_covariates,
    model_hyperparams = config$hyperparams$gtrends)
  # parallel::stopCluster(cl)


  gtrends_sample_predictions <- extract_objects$extract_from_list(gtrends_outputs)$sample_predictions |>
    dplyr::full_join(training_data |>
      dplyr::filter(date >= "2023-01-01") |>
      dplyr::select(date, nhs_region_name, icb_name, target, population),
    by = c("nhs_region_name", "icb_name", "date", "population"))
  gtrends_models <- extract_objects$extract_from_list(gtrends_outputs)$models

  gtrends_national_preds <- intervals$samples_to_quantiles(
    .sample_predictions = gtrends_sample_predictions,
    remove_identifiers = c("icb_name", "nhs_region_name"),
    overall_params = overall_params) |>
    dplyr::mutate(
      model = "gtrends",
      location = "England",
      location_level = "nation")

  gtrends_regional_preds <- intervals$samples_to_quantiles(
    .sample_predictions = gtrends_sample_predictions,
    remove_identifiers = c("icb_name"),
    overall_params = overall_params) |>
    dplyr::mutate(model = "gtrends",
      location_level = "region") |>
    dplyr::rename(location = nhs_region_name)

  gtrends_icb_preds <- intervals$samples_to_quantiles(
    .sample_predictions = gtrends_sample_predictions,
    remove_identifiers = c("nhs_region_name"),
    overall_params = overall_params) |>
    dplyr::mutate(model = "gtrends",
      location_level = "icb") |>
    dplyr::rename(location = icb_name)

  gtrends_formatted <- dplyr::bind_rows(
    gtrends_national_preds,
    gtrends_regional_preds,
    gtrends_icb_preds
  ) |>
    format_influenza_outputs()

  checks$check_forcast_format_summary(gtrends_formatted)

  rm(gtrends_outputs,
    gtrends_models,
    gtrends_national_preds,
    gtrends_regional_preds,
    gtrends_icb_preds)
  gc()
}

# # # # # # # # # # #
#### ETS  ####
# # # # # # # # # #

if ("ets" %in% models_include) {

  ets_outputs <- run_model$run_scripted_model(
    wd,
    model_name = "ets",
    training_data = training_data,
    overall_params = config$overall_params,
    start_dates = start_dates,
    required_covariates = config$required_covariates,
    model_hyperparams = config$hyperparams$ets)


  ets_sample_predictions <- extract_objects$extract_from_list(ets_outputs)$sample_predictions |>
    dplyr::full_join(
      training_data %>%
        dplyr::filter(date >= "2023-01-01") %>%
        dplyr::select(date, nhs_region_name, icb_name, target, population),
      by = c("nhs_region_name", "icb_name", "date", "population")
    )

  # we need to do this or we end up adding rates, rather than getting an average when we aggregate samples


  ets_national_preds <- intervals$samples_to_quantiles(
    .sample_predictions = ets_sample_predictions,
    remove_identifiers = c("icb_name", "nhs_region_name"),
    overall_params = overall_params) |>
    # and convert back to rates, then percent
    dplyr::mutate(
      age_group = "all",
      age_group_granularity = "none",
      model = "ets",
      location = "England",
      location_level = "nation")

  ets_regional_preds <- intervals$samples_to_quantiles(
    .sample_predictions = ets_sample_predictions,
    remove_identifiers = c("icb_name"),
    overall_params = overall_params) |>
    dplyr::mutate(
      age_group = "all",
      age_group_granularity = "none",
      model = "ets",
      location_level = "region") |>
    dplyr::rename(location = nhs_region_name)

  ets_icb_preds <- intervals$samples_to_quantiles(
    .sample_predictions = ets_sample_predictions,
    remove_identifiers = c("nhs_region_name"),
    overall_params = overall_params) |>
    dplyr::mutate(
      age_group = "all",
      age_group_granularity = "none",
      model = "ets",
      location_level = "icb") |>
    dplyr::rename(location = icb_name)

  ets_formatted <- dplyr::bind_rows(
    ets_national_preds,
    ets_regional_preds,
    ets_icb_preds
  ) |>
    dplyr::mutate(
      forecast_horizon = config$overall_params$forecast_horizon,
      target_name = "admission"
    ) %>%
    dplyr::rename(
      target_value = target
    )

  checks$check_forcast_format_summary(ets_formatted)

  rm(
    ets_outputs,
    ets_national_preds,
    ets_regional_preds,
    ets_icb_preds
  )
  gc()

}

# # # # # # # # # #
#### ENSEMBLE ####
# # # # # # # # #
# bring all prediction frames together and generate ensemble predictions

if (ensemble_include) {

  individual_model_samples <- dplyr::bind_rows(lapply(paste0(models_include, "_sample_predictions"), function(x) get(x))) |>
    # NAs from the training data targets are duplicate when binding model outputs
    dplyr::distinct()

  rm(online_sample_predictions,
    gtrends_sample_predictions,
    univariate_sample_predictions,
    ets_sample_predictions)

  print("Ensembling predictions")
  ensemble_national_preds <- individual_model_samples |>
    ensemble$ensemble_from_samples(.sample_predictions = _,
      remove_identifiers = c("icb_name", "nhs_region_name"),
      method = "mellor",
      model_name = paste(models_include, collapse = "_"),
      overall_params = overall_params) |>
    dplyr::mutate(
      location = "England",
      location_level = "nation")

  ensemble_regional_preds <- individual_model_samples |>
    ensemble$ensemble_from_samples(.sample_predictions = _,
      remove_identifiers = c("icb_name"),
      method = "mellor",
      model_name = paste(models_include, collapse = "_"),
      overall_params = overall_params) |>
    dplyr::mutate(location_level = "region") |>
    dplyr::rename(location = nhs_region_name)

  ensemble_icb_preds <- individual_model_samples |>
    ensemble$ensemble_from_samples(
      .sample_predictions = _,
      remove_identifiers = c(),
      method = "mellor",
      model_name = paste(models_include, collapse = "_"),
      overall_params = overall_params
    ) |>
    dplyr::mutate(location_level = "icb") |>
    dplyr::rename(location = icb_name) |>
    dplyr::select(-nhs_region_name)

  ensemble_formatted <- dplyr::bind_rows(
    ensemble_national_preds,
    ensemble_regional_preds,
    ensemble_icb_preds
  ) |>
    format_influenza_outputs()

  checks$check_forcast_format_summary(ensemble_formatted)
  print("Done")


  # Samples output
  # For the ensemble (all models) we want to be able to persist the samples
  # for forward models
  print("Ensembling samples")

  ensemble_national_preds_samples <- individual_model_samples |>
    intervals$aggregate_samples(remove_identifiers = c("nhs_region_name", "icb_name")) |>
    dplyr::mutate(
      location = "England",
      location_level = "nation"
    )

  ensemble_regional_preds_samples <- individual_model_samples |>
    intervals$aggregate_samples(remove_identifiers = c("icb_name")) |>
    dplyr::mutate(
      location_level = "region"
    ) |>
    dplyr::rename(location = nhs_region_name)

  ensemble_icb_preds_samples <- individual_model_samples |>
    intervals$aggregate_samples(remove_identifiers = c("nhs_region_name")) |>
    dplyr::mutate(
      location_level = "icb"
    ) |>
    dplyr::rename(location = icb_name)

  all_formatted_samples <- dplyr::bind_rows(
    ensemble_national_preds_samples,
    ensemble_regional_preds_samples,
    ensemble_icb_preds_samples
  ) |>
    format_influenza_outputs()

  checks$check_forcast_format_sample(all_formatted_samples)
  print("Done")

}


# # # # # # # # # # # # #
####  FORMAT OUTPUTS  ####
# # # # # # # # # # # # #

data_output_path <- glue::glue("{output_path}/data")
dir.create(data_output_path, recursive = T)

# get predictions across models in appropriate format for saving
all_formatted_summary <- dplyr::bind_rows(lapply(paste0(models_include, "_formatted"), function(x) get(x)))
if (ensemble_include) {
  all_formatted_summary <- dplyr::bind_rows(all_formatted_summary, ensemble_formatted)

  file_name_samples <- glue::glue("all_models_predictions_samples_{current_time}.csv.gz")

  s3$write_to_s3(
    data = all_formatted_samples,
    s3_uri = glue::glue("PATH REMOVED/influenza/{overall_params$target_name}/{file_name_samples}"),
    overwrite = TRUE,
    local_path = glue::glue("{data_output_path}/{file_name_samples}")
  )
}

checks$check_forcast_format_summary(all_formatted_summary)
model_names <- unique(all_formatted_summary$model[!is.na(all_formatted_summary$model)])
file_name_summary <- glue::glue("all_models_predictions_summary_{current_time}.csv.gz")

s3$write_to_s3(
  data = all_formatted_summary,
  s3_uri = glue::glue("PATH REMOVED/influenza/{overall_params$target_name}/{file_name_summary}"),
  overwrite = TRUE,
  local_path = glue::glue("{data_output_path}/{file_name_summary}")
)



# # # # # # # # # # # #
####  PLOTTING    ####
# # # # # # # # # # # #

# National & Regional plots
for (model_name in model_names) {
  outputs$projections_plotter(
    plots_include = list(c("lookbacks", "rag")),
    data = all_formatted_summary,
    target_name = target_name_sym,
    model_name = model_name,
    geography = c("nation", "region"),
    output_path = output_path,
    disease = config$overall_params$disease,
    y_limit = c("nation" = 800, "region" = 150),
    peaks_data = config$overall_params$show_peaks
  )
}

# ICB plots - Only for ensemble

if (ensemble_include) {

  icb_region_lookup <- rs$lookups.nhs_trusts |>
    dplyr::select(icb23nm, nhser23nm) |>
    dplyr::collect() |>
    dplyr::mutate(icb23nm = tolower(icb23nm))

  # Add on region as additional column
  icb_plotting <- all_formatted_summary |>
    dplyr::filter(location_level == "icb") |>
    dplyr::mutate(location = tolower(location)) |>
    dplyr::left_join(icb_region_lookup, by = c("location" = "icb23nm"), relationship = "many-to-many")

  region_list <- unique(icb_plotting$nhser23nm[!is.na(icb_plotting$nhser23nm)])


  for (region in region_list) {

    one_region <- icb_plotting |>
      dplyr::filter(nhser23nm == region)

    outputs$projections_plotter(
      plots_include = list(c("rag")), # only rag to keep it simple
      data = one_region,
      target_name = target_name_sym,
      model_name = model_names[grep("ensemble", model_names)], # selects only ensemble
      geography = "icb",
      output_path = output_path,
      disease = config$overall_params$disease,
      peaks_data = NULL, # Don't add peaks
      y_limit = NA
    )
  }
}

# RAG national trend probability & maps

for (model_name in model_names) {
  outputs$rag_plotter(
    data = all_formatted_summary,
    target_name = target_name_sym,
    model_name = model_name,
    geography = c("nation", "region", "icb"),
    output_path = output_path,
    disease = config$overall_params$disease
  )
}


# # # # # # # # # # # #
####  NARRATIVE   ####
# # # # # # # # # # # #

narrative_output_path <- glue::glue("{output_path}/narrative")
dir.create(narrative_output_path, recursive = T)

# National narrative
for (model_name in model_names) {
  narratives$narrative_txt_output(
    data = all_formatted_summary,
    target_name = target_name_sym,
    model_name = model_name,
    geography = "nation",
    disease = "influenza",
    output_path = narrative_output_path)
}

# ICB narrative
for (model_name in model_names) {
  narratives$map_narrative_txt_output(
    data = all_formatted_summary,
    target_name = target_name_sym,
    model_name = model_name,
    geography = "icb",
    disease = "influenza",
    output_path = narrative_output_path
  )
}

# # # # # # # # # # # # # # #
####       SCORING       ####
# # # # # # # # # # # # # # #

scoring_output_path <- glue::glue("{output_path}/scoring")
dir.create(scoring_output_path, recursive = T)

# National scoring
scoring$score(
  data = all_formatted_summary,
  geography = "nation",
  age_cohort_level = "none",
  model_names = model_names,
  output_path = scoring_output_path)

# Regional scoring
scoring$score(
  data = all_formatted_summary,
  geography = "region",
  age_cohort_level = "none",
  model_names = model_names,
  output_path = scoring_output_path)
