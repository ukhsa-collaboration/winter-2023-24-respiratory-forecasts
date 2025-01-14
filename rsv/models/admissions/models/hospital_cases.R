run_hospital_cases <- function(.data,
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
      wday_ = lubridate::wday(date),
      t_ = as.integer(date - min(date)),
      age_group = as.factor(age_group),
      nhs_region_name = as.factor(nhs_region_name)
    )

  # data used to forecast with (extends into future)
  train_test_data <- tidyr::expand_grid(
    date = seq(min(train_data$date), max(train_data$date) + forecast_horizon, by = "day"),
    # spatial identifier
    nhs_region_name = as.factor(unique(train_data$nhs_region_name)),
    # age identifier
    age_group = as.factor(unique(train_data$age_group))) |>
    dplyr::mutate(
      # need the same factors for the test set
      wday_ = lubridate::wday(date),
      t_ = as.integer(date - min(date)),
      prediction_start_date = prediction_start_date
    ) |>
    # bring in leading indicator covariates
    dplyr::left_join(transformed_data, by = c("nhs_region_name", "age_group", "date")) |>
    dplyr::group_by(nhs_region_name, age_group) |>
    dplyr::arrange(date) |>
    tidyr::fill(population) |>
    dplyr::ungroup() |>
    # need factor
    dplyr::mutate(nhs_region_name = as.factor(nhs_region_name))

  # define full formula for GAM

  # note: we haven't tuned k's a lot, which should be done with scoring properly
  # rather than eyeballing
  f <- as.formula(
    "target ~
    s(t_, bs='tp', k=splines$every_k(11, hyperparams$training_length))+
    te(t_, age_group,
      bs = c('tp', 'mrf'),
      xt = list(t_ = NULL,
                age_group = list(nb = hyperparams$nb$age_group)),
      k=c(splines$every_k(11, hyperparams$training_length), 3))+
    te(t_, nhs_region_name,
      bs = c('tp', 'mrf'),
      xt = list(t_ = NULL,
                nhs_region_name = list(nb = hyperparams$nb$nhs_region_name)),
      k=c(4,4))+
    s(nhs_region_name, bs='re') +
    s(age_group,bs='mrf',xt=list(nb=hyperparams$nb$age_group))+
    s(wday_, nhs_region_name, bs='fs',xt=list('cc'),k=4)+
    offset(log(population))
    "
  )

  # run model
  model <- mgcv::gam(
    formula = f,
    data = train_data,
    family = "nb"
  )


  # Generate samples from model fit coefficients
  fits_out <- intervals$generate_samples(
    .model = model,
    .data = train_test_data,
    .n_pi_samples = n_pi_samples
  )

  # tidy samples
  output_data_samples <- fits_out |>
    dplyr::mutate(model = "cases") |>
    dplyr::select(dplyr::any_of(output_variables))



  return(
    list(
      sample_predictions = output_data_samples,
      model = model
  ))

}
