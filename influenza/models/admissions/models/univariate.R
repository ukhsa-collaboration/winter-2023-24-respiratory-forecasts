# TODO
# use true population of ICBs for products (but not scoring)

run_univariate <- function(.data,
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
      icb_name = as.factor(icb_name),
      nhs_region_name = as.factor(nhs_region_name))

  # data used to forecast with (extends into future)
  train_test_data <- tidyr::expand_grid(
    date = seq(min(train_data$date), max(train_data$date) + forecast_horizon, by = "day"),
    # spatial identifier
    icb_name = as.factor(unique(train_data$icb_name))) |>
    dplyr::mutate(
      # need the same factors for the test set
      wday_ = as.factor(lubridate::wday(date)),
      t_ = as.integer(date - min(date)),
      prediction_start_date = prediction_start_date
    ) |>
    # bring in covariates
    dplyr::left_join(transformed_data, by = c("icb_name", "date")) |>
    dplyr::group_by(icb_name) |>
    dplyr::arrange(date) |>
    tidyr::fill(nhs_region_name) |>
    tidyr::fill(population) |>
    dplyr::ungroup() |>
    # need factor
    dplyr::mutate(nhs_region_name = as.factor(nhs_region_name))

  # define full formula for GAM

  f <- as.formula(
    '
            target ~
            s(wday_, icb_name, nhs_region_name, bs = "re") +
            s(icb_name, bs = "mrf", xt = list(nb = nbobj)) +
            s(nhs_region_name, bs = "re") +
            s(t_,
              k = splines$every_k(hyperparams$t_q, hyperparams$training_length),
              bs = "cr",
              m = 2) +
            s(t_,
              icb_name,
              bs = "fs",
              k = splines$every_k(hyperparams$icb_name_q, hyperparams$training_length),
              m = 2,
              xt = list(bs = "cr")) +
            offset(log(population))
    ')

  # run model
  model <- mgcv::bam(
    formula = f,
    data = train_data,
    family = "nb",
    discrete = T,
    method = "fREML"
  )

  # Generate samples from model fit coefficients
  fits_out <- intervals$generate_samples(
    .model = model,
    .data = train_test_data,
    .n_pi_samples = n_pi_samples
  )

  # tidy samples
  output_data_samples <- fits_out |>
    dplyr::mutate(model = "univariate") |>
    dplyr::select(dplyr::any_of(output_variables))



  return(
    list(
      sample_predictions = output_data_samples,
      model = model
  ))

}
