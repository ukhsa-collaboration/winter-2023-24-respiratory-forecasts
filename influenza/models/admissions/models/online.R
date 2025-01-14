run_online <- function(.data,
                       forecast_horizon = 14,
                       n_pi_samples = 500,
                       prediction_start_date,
                       output_variables,
                       hyperparams) {
  # define indicator lags
  lags <- seq(1, hyperparams$max_lag, 2)

  # do data imputation, smoothing, feature engineering
  transformed_data <- .data |>
    dplyr::mutate(
      wday = lubridate::wday(date)) |>
    # impute using most recent wday
    dplyr::group_by(icb_name, wday) |>
    dplyr::arrange(date) |>
    # forward the last NA value forward:
    dplyr::mutate(dplyr::across(dplyr::starts_with("online_"),
                                ~ zoo::na.locf(., na.rm = FALSE))) |>
    dplyr::ungroup() |>
    dplyr::group_by(icb_name) |>
    dplyr::arrange(date) |>
    # move the target and time-varying reporting population size forward,
    # by the forecast horizon length:
    dplyr::mutate(target = dplyr::lead(target, forecast_horizon),
      population = dplyr::lead(population, forecast_horizon)) |>
    # Transforming and scaling the leading vars
    dplyr::mutate(dplyr::across(dplyr::starts_with("online_"),
      ~ log(. + 1))) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("online_"),
      ~ zoo::rollmean(., k = 7, na.pad = TRUE, align = "right"))) |>
    # TODO difference indicator data: additive or multiplicative
    # dplyr::mutate(dplyr::across(dplyr::starts_with("online_"),
    #   ~ . - dplyr::lag(.))) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("online_"),
      ~ . / dplyr::lag(.))) |>

    # add new variables for ratio
    # dplyr::mutate(
    #   dplyr::across(
    #     .cols = dplyr::starts_with("online_"),
    #     .fns = ~./dplyr::lag(.),
    #     .names = "{.col}_ratio"
    #   )
    # ) |>

    # TODO scaling options
    # dplyr::mutate(dplyr::across(dplyr::starts_with("online_"), ~ . / true_population)) |> # population scaling instead
    dplyr::mutate(dplyr::across(dplyr::starts_with("online_"),
                                ~ scales::rescale(.))) |> # min max scaling
    # dplyr::mutate(dplyr::across(dplyr::starts_with("online_"), ~ scale(.))) |> # mean 0 var 1
    # add lags
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with(c("online_")),
        purrr::map(lags, ~ function(v) dplyr::lag(v, .x)) |>
          purrr::set_names(paste0("lag_", lags))
      )
    ) |>
    dplyr::filter(date > (min(date) + hyperparams$max_lag)) |>
    dplyr::ungroup()

  # subset the data for training the model
  train_data <- transformed_data |>
    # remove the last two weeks of data (because we have moved the target)
    dplyr::filter(date < as.Date(prediction_start_date) - forecast_horizon,
      date >= as.Date(prediction_start_date) -
        lubridate::days(hyperparams$training_length) + 1 - forecast_horizon) |>
    dplyr::mutate(
      prediction_start_date = prediction_start_date, # make variable a column
      # create factors for mgcv
      wday_ = as.factor(lubridate::wday(date)),
      t_ = as.integer(date - min(date)),
      icb_name = as.factor(icb_name),
      nhs_region_name = as.factor(nhs_region_name)
    )

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
    # bring in leading indicator covariates
    dplyr::left_join(transformed_data, by = c("icb_name", "date")) |>
    dplyr::group_by(icb_name) |>
    dplyr::arrange(date) |>
    tidyr::fill(nhs_region_name) |>
    tidyr::fill(population) |>
    dplyr::ungroup() |>
    # need factor
    dplyr::mutate(nhs_region_name = as.factor(nhs_region_name))


  indicators <- c(
    # "online_na_05_14_self_care",
    # "online_na_05_14_primary_care",
    # "online_na_05_14_secondary_care",
    # "online_na_05_09_self_care",
    # "online_na_05_09_primary_care",
    # "online_na_05_09_secondary_care",
    # "online_na_10_14_self_care",
    # "online_na_10_14_primary_care",
    # "online_na_10_14_secondary_care",
    # "online_na_15_19_self_care",
    # "online_na_15_19_primary_care",
    # "online_na_15_19_secondary_care", # ,

    "online_cold_and_flu_symptoms_05_09_na",
    "online_cold_and_flu_symptoms_10_14_na",
    "online_cold_and_flu_symptoms_15_19_na",
    "online_cold_and_flu_symptoms_05_14_na",
    "online_breathing_problems_05_09_na",
    "online_breathing_problems_10_14_na",
    "online_breathing_problems_15_19_na",
    "online_breathing_problems_05_14_na",
    "online_headache_05_09_na",
    "online_headache_10_14_na",
    "online_headache_15_19_na",
    "online_headache_05_14_na",
    "online_fever_05_09_na",
    "online_fever_10_14_na",
    "online_fever_15_19_na",
    "online_fever_05_14_na",
    "online_sore_throat_05_09_na",
    "online_sore_throat_10_14_na",
    "online_sore_throat_15_19_na",
    "online_sore_throat_05_14_na",
    "online_cough_05_09_na",
    "online_cough_10_14_na",
    "online_cough_15_19_na",
    "online_cough_05_14_na",
    "online_gi_problems_05_09_na",
    "online_gi_problems_10_14_na",
    "online_gi_problems_15_19_na",
    "online_gi_problems_05_14_na"
  )

  time_series_variables_pen <- c(
    indicators,
    paste0(indicators, "_lag_7"),
    paste0(indicators, "_lag_3"),
    paste0(indicators, "_lag_7:nhs_region_name"),
    paste0(indicators, "_lag_3:nhs_region_name"),
    paste0(indicators, ":nhs_region_name") # ,

    # paste0(indicators, "_ratio", "_lag_7"),
    # paste0(indicators, "_ratio", "_lag_3"),
    # paste0(indicators, "_ratio", "_lag_7:nhs_region_name"),
    # paste0(indicators, "_ratio", "_lag_3:nhs_region_name"),
    # paste0(indicators, "_ratio", ":nhs_region_name")
  )


  penalise_variables <- model.matrix(reformulate(time_series_variables_pen), data = train_data)
  penalise_variables_test <- model.matrix(reformulate(time_series_variables_pen), data = train_test_data)

  # run model
  model <- mgcv::bam(
    formula = target ~ -1 + penalise_variables +
      s(nhs_region_name, bs = "re") +
      s(wday_, icb_name, nhs_region_name, bs = "re") +
      # s(icb_name, bs = "re") +
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
    dplyr::mutate(model = "online") |>
    dplyr::select(dplyr::any_of(output_variables)) |>
    # correct the ::lead forward time of the target and population
    dplyr::mutate(date = date + forecast_horizon)



  return(
    list(
      sample_predictions = output_data_samples,
      model = model
  ))

}
