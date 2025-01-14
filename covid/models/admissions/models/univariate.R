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
      trust_code = as.factor(trust_code),
      nhs_region_name = as.factor(nhs_region_name)
    )

  # data used to forecast with (extends into future)
  train_test_data <- tidyr::expand_grid(
    date = seq(min(train_data$date), max(train_data$date) + forecast_horizon, by = "day"),
    # spatial identifier
    trust_code = as.factor(unique(train_data$trust_code))) |>
    dplyr::mutate(
      # need the same factors for the test set
      wday_ = as.factor(lubridate::wday(date)),
      t_ = as.integer(date - min(date)),
      prediction_start_date = prediction_start_date
    ) |>
    # bring in leading indicator covariates
    dplyr::left_join(transformed_data, by = c("trust_code", "date")) |>
    dplyr::group_by(trust_code) |>
    dplyr::arrange(date) |>
    tidyr::fill(nhs_region_name) |>
    tidyr::fill(icb_name) |>
    tidyr::fill(population) |>
    dplyr::ungroup() |>
    # need factor
    dplyr::mutate(nhs_region_name = as.factor(nhs_region_name))

  # s(t_,
  #   bs='cr',
  #   k=splines$every_k(hyperparams$national_q, hyperparams$training_length))

  # define full formula for GAM

  f <- as.formula(
    "
            target ~
            s(trust_code, bs = 're') +
            s(wday_, bs = 're') +
            offset(log(population)) +
            s(t_,
              nhs_region_name,
              bs='fs',
              m = 1,
              xt=list('cr'),
              k = splines$every_k(hyperparams$nhs_region_name_q, hyperparams$training_length))+
            s(t_,
              by=trust_code,
              k = splines$every_k(hyperparams$trust_code_q, hyperparams$training_length),
              bs = 'cr')



            ")

  # run model
  model <- mgcv::bam(
    formula = f,
    data = train_data,
    family = "nb",
    discrete = T,
    method = "fREML" # ,
    # paraPen = penalty,
    # gamma = hyperparams$gamma
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
