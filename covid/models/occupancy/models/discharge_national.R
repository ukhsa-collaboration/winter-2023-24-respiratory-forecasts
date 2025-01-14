run_discharge <- function(training_data, overall_params) {
  # we only need the national level data for initial model
  national_training_data <- training_data |>
    dplyr::filter(!is.na(occupancy) & !is.na(total_beds) & !is.na(admissions)) |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      total_beds = sum(total_beds),
      occupancy = sum(occupancy),
      admissions = sum(admissions)) |>
    dplyr::ungroup()

  # keep only some of the time series (max_lag + training length)
  national_training_data_filtered <- national_training_data |>
    dplyr::filter(date >= max(date) - (overall_params$max_lag + overall_params$training_length)) |>
    dplyr::mutate(t = 1 + as.integer(date - min(date)))

  # construct stan ready training data
  input_data <- list(
    N = nrow(national_training_data_filtered),
    `T` = max(national_training_data_filtered$t),
    J = overall_params$max_lag,
    x = national_training_data_filtered$arrival_admissions,
    y = national_training_data_filtered$occupancy
  )

  # make path an argument?
  model <- cmdstanr::cmdstan_model("covid/models/occupancy/models/discharge_national.stan")

  model <- model$sample(
    data = input_data,
    iter_warmup = 1500,
    iter_sampling = overall_params$n_pi_sample,
    chains = 2,
    parallel_chains = 2)

  # extract posterior draws
  draws <- model$draws() %>%
    posterior::as_draws_df()

  # show historic predictions from `generated quantities`
  predictions <- draws %>%
    dplyr::select(.draw, dplyr::starts_with("y_hat")) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("y_hat")) %>%
    dplyr::mutate(t = as.integer(stringr::str_extract(name, "\\d+"))) %>%
    dplyr::group_by(t) %>%
    dplyr::summarise(pi_50 = quantile(value, p = 0.5),
      pi_95 = quantile(value, p = 0.95),
      pi_5 = quantile(value, p = 0.05)) %>%
    dplyr::ungroup() |>
    dplyr::left_join(national_training_data_filtered, ., by = "t")

  # extract posterior samples of interest for further modelling
  parameters <- draws %>%
    dplyr::select(.draw, lognormal_mu, lognormal_sigma, beta)

  return(list(
    parameters = parameters,
    predictions = predictions,
    model = model,
    training_data = national_training_data
  ))
}
