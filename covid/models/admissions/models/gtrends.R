run_gtrends <- function(
    .data,
    forecast_horizon = 14,
    n_pi_samples = 500,
    prediction_start_date,
    output_variables,
    hyperparams
    ) {
  # lags of indicators
  lags <- seq(1, hyperparams$max_lag, 2)

  # do data imputation, smoothing, feature engineering
  transformed_data <- .data |>

    # impute
    dplyr::group_by(trust_code) |>
    dplyr::arrange(date) |>
    tidyr::fill(dplyr::starts_with("gt")) |>
    # If anything is still NA (i.e. run of consecutive NAs from start of column,
    # possibly all the way to end of column), set it to 0
    dplyr::mutate(dplyr::across(dplyr::starts_with("gt_"), \(.) tidyr::replace_na(., 0))) |>

    # move the target forward by the forecast horizon length
    dplyr::mutate(target = dplyr::lead(target, forecast_horizon)) |>
    # Transforming and scaling the leading vars
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("gt_"),
        \(.) data.table::frollmean(., 7, na.rm = TRUE) |>
          scales::rescale()
      )
    ) |>
    # add lags
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("gt_"),
        # Apply one function per lag, to give columns like `gt_[symptom]_lag_[n]"
        lags |>
          purrr::set_names() |>
          purrr::set_names(\(.) paste0("lag_", .)) |>
          purrr::map(\(lag) function(v) dplyr::lag(v, lag))
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(date > (min(date) + hyperparams$max_lag))

  # subset the data for training the model
  train_data <- transformed_data |>
    # remove the last two weeks of data (because we have moved the target)
    dplyr::filter(
      date < prediction_start_date - forecast_horizon,
      date >= as.Date(prediction_start_date) - lubridate::days(hyperparams$training_length) + 1 - forecast_horizon
    ) |>
    dplyr::mutate(
      "t" = as.integer(date - min(date)), # TODO unused?
      # must be factors for mgcv
      "wday" = as.factor(lubridate::wday(date)),
      trust_code = as.factor(trust_code),
      nhs_region_name = as.factor(nhs_region_name)
    )

  # data used to forecast with (extends into future)
  train_test_data <- tidyr::expand_grid(
    "date" = seq(min(train_data$date), max(train_data$date) + forecast_horizon, by = "day"),
    # spatial identifier
    "trust_code" = as.factor(unique(train_data$trust_code))
  ) |>
    dplyr::mutate(
      # need the same factors for the test set
      "t" = as.integer(date - min(date)),
      "wday" = as.factor(lubridate::wday(date))
    ) |>
    # bring in leading indicator covariates
    dplyr::left_join(transformed_data, dplyr::join_by(trust_code, date)) |>
    dplyr::group_by(trust_code) |>
    dplyr::arrange(date) |>
    tidyr::fill(nhs_region_name, icb_name, population) |>
    dplyr::ungroup() |>
    # need factor
    dplyr::mutate(nhs_region_name = as.factor(nhs_region_name))

  # Select all included terms:
  indicators <- c(
    # Group 1: terms that seem like reasonable indicators (at national level) and dont seem to be mainly flu signals
    "gt_chills",
    "gt_cough", # maybe flu
    "gt_headache",
    "gt_shortness_of_breath",
    "gt_throat_irritation",
    "gt_diarrhea",
    "gt_migraine",
    "gt_nausea",
    "gt_pleurisy",
    "gt_rhinorrhea",
    "gt_ageusia", # low counts, maybe flu
    "gt_arthralgia", # not sure if good indicator
    "gt_hypoxia", # not that great indicator
    "gt_shivering" # ,

    # # Group 2: terms that don't look like good indicators (at national level) but dont seem to be mainly flu
    # "gt_clouding of consciousness",
    # "gt_fatigue",
    # "gt_muscle_weakness",
    # "gt_myalgia",
    # "gt_pulmonary_edema",
    # "gt_shallow breathing", # very low counts
    #
    # # Group 3: terms that seem like mainly flu signals
    # "gt_sore_throat",
    # "gt_anosmia",
    # "gt_fever", # has slight bumps for covid
    # "gt_low_grade_fever",
    # "gt_pneumonia", # bigger bumps for covid
    # "gt_viral_pneumonia" # low counts
  )

  time_series_variables_pen <- c(
    indicators,
    paste0(indicators, "_lag_3"),
    paste0(indicators, "_lag_3:nhs_region_name"),
    paste0(indicators, "_lag_5"),
    paste0(indicators, "_lag_5:nhs_region_name"),
    paste0(indicators, "_lag_7"),
    paste0(indicators, "_lag_7:nhs_region_name"),
    paste0(indicators, ":nhs_region_name")
  )

  penalise_variables <- model.matrix(reformulate(time_series_variables_pen), data = train_data)
  penalise_variables_test <- model.matrix(reformulate(time_series_variables_pen), data = train_test_data)

  # run model
  model <- mgcv::bam(
    formula = target ~ -1 + penalise_variables +
      s(nhs_region_name, bs = "re") +
      s(wday, bs = "re") +
      offset(log(population)),
    data = train_data,
    family = "nb",
    discrete = TRUE,
    method = "fREML",
    paraPen = list(
      "penalise_variables" = list(rank = dim(penalise_variables)[2],
                                  diag(dim(penalise_variables)[2]))),
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
    dplyr::mutate(
      "prediction_start_date" = prediction_start_date,
      "model" = "gtrends"
    ) |>
    dplyr::select(dplyr::any_of(output_variables)) |>
    # correct the ::lead forward time of the target
    dplyr::mutate(date = date + forecast_horizon)


  return(
    list(
      sample_predictions = output_data_samples,
      model = model
  ))
}
