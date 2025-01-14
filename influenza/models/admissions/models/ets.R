run_ets <- function(.data,
                    forecast_horizon = 14,
                    n_pi_samples = 500,
                    prediction_start_date,
                    output_variables,
                    hyperparams) {

  ets_training_data <- .data

  transformed_data <- tidyr::expand_grid(
    date = seq(min(ets_training_data$date, na.rm = TRUE),
      as.Date(prediction_start_date) + (forecast_horizon - 1),
      by = "day"
    ),
    icb_name = unique(na.omit(ets_training_data$icb_name))
  ) |>
    dplyr::left_join(ets_training_data, by = c("date", "icb_name")) |>
    dplyr::filter(date >= as.Date(prediction_start_date) - hyperparams$training_length) |>
    dplyr::group_by(icb_name) |>
    tidyr::fill(population) |>
    dplyr::ungroup() |>
    dplyr::mutate(prediction_start_date = as.Date(prediction_start_date))

  # create tsibble
  train_data <- tsibble::as_tsibble(transformed_data, index = date, key = c("icb_name", "nhs_region_name")) |>
    dplyr::filter(date < prediction_start_date) |>
    dplyr::group_by(icb_name) |>
    # impute missing data with last known value - ets does not handle missing values
    tidyr::fill(target) |>
    dplyr::ungroup()

  # run model for regional and national level
  ets_model <- train_data |>
    fabletools::model(
      ets = fable::ETS((target + 0.01) ~ error("A") + trend("Ad", alpha = hyperparams$alpha, beta = hyperparams$beta, phi = hyperparams$phi) + season("A", period = "1 week"), opt_crit = "amse", nmse = 14)
    )

  ets_fcast <- ets_model |>
    fabletools::forecast(h = forecast_horizon, bootstrap = TRUE, times = n_pi_samples) |>
    dplyr::mutate(distributional::parameters(target), family(target)) |>
    tidyr::unnest(x) |>
    dplyr::mutate(
      # Clip negative values to 0
      ".value" = pmax(x, 0),
      .keep = "unused"
    ) |>
    dplyr::group_by(icb_name, date, nhs_region_name) |>
    dplyr::mutate(
      ".sample" = dplyr::row_number(),
      "model" = "ets",
      "prediction_start_date" = prediction_start_date
    ) |>
    dplyr::select(dplyr::any_of(output_variables))

  ets_fcast <- as.data.frame(ets_fcast) |>
    dplyr::left_join(
      transformed_data |>
        dplyr::select(date, icb_name, population),
      dplyr::join_by(date, icb_name)) |>
    dplyr::mutate(prediction_start_date = as.Date(prediction_start_date))

  section_bounds <- unique(c(seq(1, hyperparams$training_length, by = 7), hyperparams$training_length))
  ets_fit <- c()
  for (ii in 1:(length(section_bounds) - 1)) {
    ets_fit <- dplyr::bind_rows(
      ets_fit,
      ets_model |>
        fabletools::generate(
          new_data = train_data |>
            dplyr::filter(
              date <= prediction_start_date - section_bounds[ii],
              date > prediction_start_date - section_bounds[ii + 1]
            ),
          bootstrap = TRUE,
          times = n_pi_samples
        )
    )
  }
  ets_fit <- ets_fit |>
    dplyr::mutate(
      "model" = "ets",
      ".value" = pmax(.sim, 0),
      ".sample" = as.numeric(.rep),
      .keep = "unused"
    ) |>
    dplyr::select(dplyr::any_of(output_variables))

  model_coefs <- coef(ets_model) |>
    dplyr::filter(term %in% c("alpha", "beta", "gamma", "phi"))

  ets_combined <- dplyr::bind_rows(ets_fcast, ets_fit)

  return(list(sample_predictions = ets_combined, model_coefs = model_coefs))
}
