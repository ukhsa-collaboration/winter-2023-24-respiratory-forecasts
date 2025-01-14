run_discharge_region <- function(training_data, overall_params) {
  # now regional
  regional_training_data <- training_data |>
    dplyr::filter(!is.na(occupancy) & !is.na(total_beds) & !is.na(admissions)) |>
    dplyr::group_by(date, nhs_region_name) |>
    dplyr::summarise(
      total_beds = sum(total_beds),
      occupancy = sum(occupancy),
      admissions = sum(admissions)) |>
    dplyr::ungroup() |>
    dplyr::mutate(r = as.numeric(as.factor(nhs_region_name)))

  # keep only some of the time series (max_lag + training length)
  regional_training_data_filtered <- regional_training_data |>
    dplyr::filter(date >= max(date) - (overall_params$max_lag + overall_params$training_length)) |>
    dplyr::mutate(t = 1 + as.integer(date - min(date)))

  region_lookup <- regional_training_data_filtered %>%
    dplyr::select(r, nhs_region_name) |>
    dplyr::distinct()


  # construct stan ready training data
  input_data <- list(
    N = nrow(regional_training_data_filtered),
    `T` = max(regional_training_data_filtered$t),
    R = length(unique(regional_training_data_filtered$r)),
    day = regional_training_data_filtered$t,
    region = regional_training_data_filtered$r,
    J = overall_params$max_lag,
    x = regional_training_data_filtered$admissions,
    y = regional_training_data_filtered$occupancy
  )

  # make path an argument?
  model <- cmdstanr::cmdstan_model("influenza/models/occupancy/models/discharge_region_variation.stan")

  model <- model$sample(
    data = input_data,
    iter_warmup = overall_params$warmup_iterations,
    iter_sampling = overall_params$n_pi_sample,
    chains = 2,
    parallel_chains = 2)

  # extract posterior draws
  draws <- model$draws() %>%
    posterior::as_draws_df()


  # show historic predictions from `generated quantities`
  predictions <- draws %>%
    dplyr::select(.draw, dplyr::starts_with("y_hat")) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("y_hat")) |>
    dplyr::mutate(name = stringr::str_remove(name, "y_hat")) |>
    dplyr::mutate(name = stringr::str_remove_all(name, "[\\[\\]]")) |>
    tidyr::separate_wider_delim(cols = "name", names = c("r", "t"), delim = ",") |>
    dplyr::mutate(t = as.integer(t),
      r = as.integer(r)) |>
    dplyr::group_by(t, r) %>%
    dplyr::summarise(pi_50 = quantile(value, p = 0.5),
      pi_95 = quantile(value, p = 0.95),
      pi_5 = quantile(value, p = 0.05)) %>%
    dplyr::ungroup() |>
    dplyr::left_join(regional_training_data_filtered, ., by = c("t", "r")) |>
    dplyr::select(-r)

  # extract posterior samples of interest for further modelling
  parameters <- draws %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("lognormal_mu"), names_to = "r_lognormal_mu", values_to = "lognormal_mu") |>
    dplyr::mutate(r_lognormal_mu = as.integer(stringr::str_extract(r_lognormal_mu, "\\d+"))) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("beta"), names_to = "r_beta", values_to = "beta") |>
    dplyr::mutate(r_beta = as.integer(stringr::str_extract(r_beta, "\\d+"))) |>
    # this feels like wasting compute/memory, but not sure of a better way... it works
    dplyr::filter(r_beta == r_lognormal_mu) |>
    dplyr::rename(r = r_lognormal_mu) |>
    dplyr::select(.draw, r, lognormal_mu, lognormal_sigma, beta) |>
    dplyr::left_join(region_lookup, by = "r") |>
    dplyr::select(-r)

  return(list(
    parameters = parameters,
    predictions = predictions,
    model = model,
    training_data = regional_training_data,
    region_lookup = region_lookup
  ))
}
