# Script to run COVID ensemble (version 2)

# # # # # # # # # # #
#### DESCRIPTION ####
# # # # # # # # # # #
#
# COVID Ensemble models, running and output creation
#
#

# TODO model ideas
# can we do offsets properly for COVID
# explore weighting samples for training
# explore hdi (rob idea) for combining forecasts
# explore improved smoothing (check for right alignment)
# move to gratia for posterior samples
# single spine within each models data processing


# ENVIRONMENT SET UP
# setwd(system("echo $(git rev-parse --show-toplevel)", intern = TRUE))
# options(dplyr.summarise.inform = FALSE)
# Sys.setenv( # in case box isn't working:
#   "R_BOX_PATH" = fs::path(rprojroot::find_root(rprojroot::is_git_root),
#                           "src", "R"))
# install all required packages for the modelling:
# source("./covid/models/src/depends.R") # should be covered in deps_$need()
# source("./covid/models/src/format.R")

# SET GLOBAL SEED for reproducibility
set.seed(8675309)

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
  prj / projection_plots,
  prj / scoring,
  prj / user_check,
  prj / run_model,
  prj / outputs,
  prj / format # TODO: Turn into generic box module
)

# box::use(. / covid / models / src / format)

rs <- redshift$data_model("lookups")

# # # # # # # # # # # #
#### CONFIGURATION ####
# # # # # # # # # # # #

config <- yaml::read_yaml("./covid/models/admissions/covid_admissions_config.yaml")
current_time <- format(Sys.time(), "%Y-%m-%d_%H:%M:%S_%a")

training_data_path <- config$files$pipeline_data

output_path <- rprojroot::find_rstudio_root_file() |>
  fs::path("covid", "outputs", "admissions", current_time) |>
  fs::dir_create()


# define config from yaml file
overall_params <- config$overall_params

overall_params$threshold_rates <- list(
  upper_rate = config$thresholds[[overall_params$target_name]],
  lower_rate = -config$thresholds[[overall_params$target_name]]
)

models_include <- overall_params$models_include



# # # # # # # # # #
#### LOAD DATA ####
# # # # # # # # # #

target_name_sym <- rlang::sym(overall_params$target_name)

# Check whether we're using most recent or specific data file:
if (training_data_path %in% c("", "latest", "new", "newest", "most recent") ||
  is.null(training_data_path)) {
  training_data_path <- s3$find_latest_file(
    uri = config$files$pipeline_path,
    pattern = config$files$pipeline_pattern)
}

# These names will need to be updated with newer data pipeline inputs
training_data <- aws.s3::s3read_using(
  vroom::vroom,
  object = training_data_path
) |>
  dplyr::mutate(date = lubridate::ymd(date)) |>
  dplyr::mutate("target" = {{ target_name_sym }}) |>
  # we only have models defined for acute trusts
  dplyr::filter(is_acute) |>
  # something to keep only recent data
  dplyr::filter(date >= "2023-01-01")

overall_params$max_lookback <- max(training_data$date) + 1

# check how recent the data is
user_check$user_check(lead_message = glue::glue(
  "The difference between the most recent data and today is: {Sys.Date() - max(training_data$date)} days.\n Is this ok?"))

# define lookback start dates
start_dates <- seq(
  from = as.Date(overall_params$max_lookback) - (overall_params$n_lookbacks - 1) * overall_params$lookback_step_length,
  to = as.Date(overall_params$max_lookback),
  by = glue::glue("{overall_params$lookback_step_length} days")
)


# For each requested model, fit & then get predictions for:
# * Samples
# * Health geographies
predictions <- models_include |>
  purrr::set_names() |>
  purrr::map(function(model) {

    sample_preds <- run_model$run_scripted_model(
      wd = rprojroot::find_rstudio_root_file(),
      model_name = model,
      training_data = training_data,
      overall_params = overall_params,
      start_dates = start_dates,
      required_covariates = config$required_covariates,
      model_hyperparams = config$hyperparams[[model]]
    ) |>
      purrr::map("sample_predictions") |>
      purrr::list_rbind()


    # Aggregate to lowest health-geography available to all components, which is ICB
    if ("trust_code" %in% colnames(sample_preds)) {
      sample_preds <- sample_preds |>
        # TODO dummy $target, because that column is named in intervals$aggregate_samples() - tweak that function?
        dplyr::mutate("target" = 0) |>
        intervals$aggregate_samples(remove_identifiers = "trust_code") |>
        dplyr::select(!target)
    }

    # Attach ICB-level target values
    sample_preds <- sample_preds |>
      dplyr::left_join(
        training_data |>
          dplyr::filter(date >= "2023-01-01") |>
          dplyr::summarise(
            target = sum(target, na.rm = TRUE),
            .by = c(date, icb_name)
          ),
        dplyr::join_by(date, icb_name)
      )

    list(

      "sample_preds" = sample_preds,

      "geo_preds" = list(
        "nation" = intervals$samples_to_quantiles(
          .sample_predictions = sample_preds,
          remove_identifiers = c("icb_name", "nhs_region_name"),
          overall_params = overall_params
        ) |>
          dplyr::mutate("location" = "England"),

        "region" = intervals$samples_to_quantiles(
          .sample_predictions = sample_preds,
          remove_identifiers = "icb_name",
          overall_params = overall_params
        ) |>
          dplyr::rename("location" = nhs_region_name),

        "icb" = intervals$samples_to_quantiles(
          .sample_predictions = sample_preds,
          remove_identifiers = "nhs_region_name",
          overall_params = overall_params
        ) |>
          dplyr::rename("location" = icb_name)

      ) |>
        purrr::list_rbind(names_to = "location_level")
    )

  })


# # # # # # # # # #
#### ENSEMBLE ####
# # # # # # # # #
# bring all prediction frames together and generate ensemble predictions
print("Now ensembling model samples")



individual_model_samples <- predictions |>
  purrr::map("sample_preds") |>
  purrr::list_rbind(names_to = "model")


predictions[["ensemble"]] <- list(

  "sample_preds" = individual_model_samples,

  "geo_preds" = list(
    "nation" = ensemble$ensemble_from_samples(
      .sample_predictions = individual_model_samples,
      remove_identifiers = c("nhs_region_name", "icb_name"),
      method = "mellor",
      model_name = paste(models_include, collapse = "_"),
      overall_params = overall_params
    ) |>
      dplyr::mutate(location = "England"),

    "region" = ensemble$ensemble_from_samples(
      .sample_predictions = individual_model_samples,
      remove_identifiers = "icb_name",
      method = "mellor",
      model_name = paste(models_include, collapse = "_"),
      overall_params = overall_params
    ) |>
      dplyr::rename("location" = nhs_region_name),

    "icb" = ensemble$ensemble_from_samples(
      .sample_predictions = individual_model_samples,
      remove_identifiers = "nhs_region_name",
      method = "mellor",
      model_name = paste(models_include, collapse = "_"),
      overall_params = overall_params
    ) |>
      dplyr::rename("location" = icb_name)
  ) |>
    purrr::list_rbind(names_to = "location_level")
)



# Samples output
# For the ensemble (all models) we want to be able to persist the samples
# for forward models

all_formatted_samples <- list(
  "nation" = intervals$aggregate_samples(
    individual_model_samples,
    remove_identifiers = c("nhs_region_name", "icb_name")
  ) |>
    dplyr::mutate(location = "England"),

  "region" = intervals$aggregate_samples(
    individual_model_samples,
    remove_identifiers = "icb_name"
  ) |>
    dplyr::rename("location" = nhs_region_name)
) |>
  purrr::list_rbind(names_to = "location_level") |>
  format$format_covid_outputs(overall_params$target_name, overall_params$forecast_horizon)

checks$check_forcast_format_sample(all_formatted_samples)


# # # # # # # # # # # # #
####  FORMAT OUTPUTS ####
# # # # # # # # # # # # #
print("Now formatting data and writing to S3")

# get predictions across models in appropriate format for saving
all_formatted_summary <- predictions |>
  purrr::map("geo_preds") |>
  purrr::list_rbind(names_to = "model") |>
  format$format_covid_outputs(overall_params$target_name, overall_params$forecast_horizon)

checks$check_forcast_format_summary(all_formatted_summary)

model_names <- names(predictions)

data_output_path <- fs::dir_create(output_path, "data")

file_name_summary <- glue::glue("all_models_predictions_summary_{current_time}.csv.gz")

s3$write_to_s3(
  data = all_formatted_summary,
  s3_uri = glue::glue("REMOVED PATH/covid-19/{overall_params$target_name}/{file_name_summary}"),
  local_path = glue::glue("{data_output_path}/{file_name_summary}")
)

file_name_samples <- glue::glue("all_models_predictions_samples_{current_time}.csv.gz")

s3$write_to_s3(
  data = all_formatted_samples,
  s3_uri = glue::glue("REMOVED PATH/covid-19/{overall_params$target_name}/{file_name_samples}"),
  local_path = glue::glue("{data_output_path}/{file_name_samples}")
)


# # # # # # # # # #
#### PLOTTING ####
# # # # # # # # #
print("Now plotting outputs")

# National & regional plots
for (model_name in model_names) {
  outputs$projections_plotter(
    plots_include = list(c("lookbacks", "rag")),
    data = all_formatted_summary,
    target_name = target_name_sym,
    model_name = model_name,
    geography = c("nation", "region"),
    output_path = output_path,
    y_limit = c(NA, 250),
    disease = config$overall_params$disease,
    peaks_data = config$overall_params$show_peaks
  )
}

# ICB plots - Only for ensemble

nhs_trust <- rs$nhs_trusts |>
  dplyr::select(icb23nm, nhser23nm) |>
  dplyr::collect() |>
  dplyr::mutate(icb23nm = tolower(icb23nm))


# Add on region as additional column
icb_plotting <- all_formatted_summary |>
  dplyr::filter(location_level == "icb") |>
  dplyr::mutate(location = tolower(location)) |>
  dplyr::left_join(nhs_trust, by = c("location" = "icb23nm"), relationship = "many-to-many")

region_list <- unique(icb_plotting$nhser23nm[!is.na(icb_plotting$nhser23nm)])


for (region in region_list) {

  one_region <- icb_plotting |>
    dplyr::filter(nhser23nm == region)

  outputs$projections_plotter(
    plots_include = list(c("rag")), # only rag to keep it simple
    data = one_region,
    target_name = target_name_sym,
    model_name = grep("ensemble", model_names, value = TRUE), # selects ensemble
    geography = "icb",
    output_path = output_path,
    disease = config$overall_params$disease,
    peaks_data = NULL, # Don't add peaks
    y_limit = NA)
}

# RAG probabilities

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

# # # # # # # # # #
#### NARRATIVE ####
# # # # # # # # # #
print("Now writing narratives")

narrative_output_path <- fs::dir_create(fs::path(output_path, "narrative"))

# National narrative
for (model_name in model_names) {
  narratives$narrative_txt_output(
    data = all_formatted_summary,
    target_name = target_name_sym,
    model_name = model_name,
    geography = "nation",
    disease = "COVID-19",
    output_path = narrative_output_path
  )
}

# ICB narrative
for (model_name in model_names) {
  narratives$map_narrative_txt_output(
    data = all_formatted_summary,
    target_name = target_name_sym,
    model_name = model_name,
    geography = "icb",
    disease = "COVID-19",
    output_path = narrative_output_path
  )
}


# # # # # # # # #
#### SCORING ####
# # # # # # # # #
print("Now scoring models")

scoring_output_path <- fs::dir_create(fs::path(output_path, "scoring"))

# National scoring
scoring$score(
  data = all_formatted_summary,
  geography = "nation",
  age_cohort_level = "none",
  model_names = model_names,
  output_path = scoring_output_path
)

# Regional scoring
scoring$score(
  data = all_formatted_summary,
  geography = "region",
  age_cohort_level = "none",
  model_names = model_names,
  output_path = scoring_output_path
)

check_lon_icb <- individual_model_samples |>
  dplyr::filter(icb_name == "NHS North East London Integrated Care Board")
check_lon_icb <- check_lon_icb |>
  dplyr::filter(.sample == 1)
