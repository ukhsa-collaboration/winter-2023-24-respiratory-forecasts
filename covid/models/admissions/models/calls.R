run_calls <- function(.data,
                      forecast_horizon = 14,
                      n_pi_samples = 500,
                      prediction_start_date,
                      output_variables,
                      hyperparams) {
  # lags of indicators
  lags <- seq(1, hyperparams$max_lag, 2)

  # do data imputation, smoothing, feature engineering
  transformed_data <- .data |>
    dplyr::mutate(
      wday = lubridate::wday(date)
    ) |>
    # impute using most recent wday
    dplyr::group_by(trust_code, wday) |> # group by trust and day of the week
    dplyr::arrange(date) |>
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("calls_"),
      ~ zoo::na.locf(., na.rm = FALSE)
    )) |> # forward the last NA value forward
    dplyr::ungroup() |>
    dplyr::group_by(trust_code) |>
    dplyr::arrange(date) |>
    # move the target forward by the forecast horizon length
    dplyr::mutate(target = dplyr::lead(target, forecast_horizon)) |>
    # Transforming and scaling the leading vars
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("calls_"),
      ~ log(. + 1)
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("calls_"),
      ~ zoo::rollmean(., k = 7, na.pad = TRUE, align = "right")
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("calls_"),
      ~ scales::rescale(.)
    )) |>
    dplyr::arrange(date) |>
    # add lags
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with(c("calls_")),
        purrr::map(lags, ~ function(v) dplyr::lag(v, .x)) |>
          purrr::set_names(paste0("lag_", lags))
      )
    ) |>
    dplyr::filter(date > (min(date) + hyperparams$max_lag)) |>
    dplyr::ungroup()

  # subset the data for training the model
  train_data <- transformed_data |>
    # remove the last two weeks of data (because we have moved the target)
    dplyr::filter(
      date < prediction_start_date - forecast_horizon,
      date >= as.Date(prediction_start_date) - lubridate::days(hyperparams$training_length) + 1 - forecast_horizon
    ) |>
    dplyr::mutate(
      prediction_start_date = prediction_start_date, # make the variable a column
      # create factors for mgcv
      wday_ = as.factor(lubridate::wday(date)),
      t_ = as.integer(date - min(date)),
      trust_code = as.factor(trust_code),
      nhs_region_name = as.factor(nhs_region_name)
    )

  # View(train_data)

  # data used to forecast with (extends into future)
  train_test_data <- tidyr::expand_grid(
    date = seq(min(train_data$date), max(train_data$date) + forecast_horizon, by = "day"),
    # spatial identifier
    trust_code = as.factor(unique(train_data$trust_code))
  ) |>
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


  indicators <- c(
    "calls_clinic_0_19",
    "calls_clinic_20_59",
    "calls_clinic_60_plus",
    "calls_ambo_0_19",
    "calls_ambo_20_59",
    "calls_ambo_60_plus",
    # "calls_self_care_0_19",
    "calls_self_care_20_59",
    "calls_self_care_60_plus"
  )



  time_series_variables_pen <- c(
    "-1",
    indicators,
    paste0(indicators, "_lag_7"),
    paste0(indicators, "_lag_3"),
    paste0(indicators, ":nhs_region_name"),
    paste0(indicators, "_lag_7:nhs_region_name"),
    paste0(indicators, "_lag_3:nhs_region_name")
  )

  penalise_variables <- model.matrix(reformulate(time_series_variables_pen), data = train_data)
  penalise_variables_test <- model.matrix(reformulate(time_series_variables_pen), data = train_test_data)
  # print(penalise_variables)
  # run model
  model <- mgcv::bam(
    formula = target ~ -1 + penalise_variables +
      s(nhs_region_name, bs = "re") +
      s(wday_, bs = "re") +
      offset(log(population)),
    data = train_data,
    family = "nb",
    discrete = T,
    method = "fREML",
    paraPen = list("penalise_variables" = list(rank = dim(penalise_variables)[2], diag(dim(penalise_variables)[2]))),
    gamma = hyperparams$gamma
  )



  # Generate samples from model fit coefficients
  fits_out <- intervals$generate_samples(
    .model = model,
    .data = dplyr::mutate(train_test_data, "penalise_variables" = penalise_variables_test),
    .n_pi_samples = n_pi_samples
  )

  # tidy samples
  output_data_samples <- fits_out |>
    dplyr::mutate(model = "calls") |>
    dplyr::select(dplyr::any_of(output_variables)) |>
    # correct the ::lead forward time of the target
    dplyr::mutate(date = date + forecast_horizon)


  return(
    list(
      sample_predictions = output_data_samples,
      model = model
    )
  )
}
