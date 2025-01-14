# This function is an example model structure, which can be adapted to
# create new models. It contains the required basic transformations used
# to produce the outputs required for model post-processing.
# It will need to be adapted for the needs of any new model

run_example <- function(.data,
                        forecast_horizon = 14,
                        n_pi_samples = 500,
                        prediction_start_date,
                        output_variables,
                        hyperparams) {
  # do data imputation, smoothing, feature engineering
  transformed_data <- .data

  # subset the data for training the model
  train_data <- transformed_data |>
    dplyr::filter(date < prediction_start_date,
      date >= as.Date(prediction_start_date) - lubridate::days(hyperparams$training_length)) |>
    dplyr::mutate(prediction_start_date = prediction_start_date,
      wday_ = as.factor(lubridate::wday(date)),
      t_ = as.integer(date - min(date)),
      trust_code = as.factor(trust_code),
      nhs_region_name = as.factor(nhs_region_name)
    )

  # data used to forecast with (extends into future)
  train_test_data <- tidyr::expand_grid(
    date = seq(min(train_data$date), max(train_data$date) + forecast_horizon, by = "day"),
    # spatial identifier
    trust_code = as.factor(unique(train_data$trust_code))) |>
    # bring in leading indicator covariates
    dplyr::left_join(transformed_data, by = c("trust_code", "date")) |>
    dplyr::mutate(prediction_start_date = prediction_start_date,
      wday_ = as.factor(lubridate::wday(date)),
      t_ = as.integer(date - min(date)),
      trust_code = as.factor(trust_code),
      nhs_region_name = as.factor(nhs_region_name)
    )


  # pass as an argument?
  f <- as.formula("target ~ nhs_region_name")

  # pass as an argument?
  gamma <- 1

  # run model
  model <- mgcv::bam(
    formula = f,
    data = train_data,
    family = "nb",
    discrete = T,
    method = "fREML",
    # paraPen = list(),
    gamma = gamma
  )

  fits_out <- intervals$generate_samples(
    .model = model,
    .data = train_test_data,
    .n_pi_samples = n_pi_samples,
    model_rate = FALSE
  )

  # tidy samples
  output_data_samples <- fits_out |>
    dplyr::mutate(model = "example") |>
    dplyr::select(dplyr::any_of(output_variables))



  return(
    list(
      sample_predictions = output_data_samples,
      model = model
  ))

}
