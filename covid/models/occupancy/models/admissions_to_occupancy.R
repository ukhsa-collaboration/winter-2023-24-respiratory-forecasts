#' Function to combine admissions and discharge posteriors to create
#' occupancy forecast
#'
#' @param admissions_data The loaded in formatted admissions forecast
#' @param discharge_data The formatted discharge parameters
#' @param overall_param Hyperparameters of the model
#'
#' @returns Occupancy forecast table


run_occupancy <- function(admissions_data, discharge_data, overall_params) {
  # create historic data of "samples" of the true data, to allow for easy joining
  # we need to join historic admissions data onto the parameter samples for discharge
  prepared_admissions <- tidyr::expand_grid(
    # create spine of date, prediction start date, model and each sample instance
    date = seq(min(admissions_data$date), max(admissions_data$date), by = "day"),
    prediction_start_date = unique(na.omit(admissions_data$prediction_start_date)),
    model = unique(na.omit(admissions_data$model)),
    .sample = 1:overall_params$n_pi_sample) |>
    # not all prediction start dates need all dates going backward
    dplyr::filter(date >= prediction_start_date - (overall_params$max_lag + overall_params$training_length) &
      date <= prediction_start_date + overall_params$forecast_horizon - 1) |>
    dplyr::left_join(admissions_data |>
      dplyr::filter(!is.na(.sample) | !is.na(.value)) |>
      dplyr::select(date, prediction_start_date, model, .sample, .value, target_value),
    by = c("date", "prediction_start_date", "model", ".sample")) |>
    dplyr::left_join(discharge_data$training_data, by = "date") |>
    dplyr::select(-target_value) |>
    # fill in the empty historic values with true arrival admissions
    dplyr::mutate(.value = dplyr::coalesce(.value, arrival_admissions)) |>
    dplyr::left_join(discharge_data$parameters, by = c(".sample" = ".draw")) |>
    dplyr::mutate(estimated_admissions = .value) |>
    # we do this to reduce uncertainty, using the real admissions rather than esetimated
    # we may want to remove this in future and use estimated
    dplyr::mutate(estimated_admissions = ifelse(date < prediction_start_date, admissions, estimated_admissions))


  # combine our posterior samples with forecasts
  forecast_occupancy <- data.frame()

  # loop over each prediction start date, then each date within the admissions data
  for (p_start_date in unique(na.omit(prepared_admissions$prediction_start_date))) {

    p_start_date <- as.Date(p_start_date)

    print(glue::glue("Running lookback period {p_start_date}"))

    subset <- prepared_admissions |>
      dplyr::filter(prediction_start_date == p_start_date)

    # we want at minimum max_lag data to feed in + enough for historic fit
    # we could instead do max_lag + training length, however, the further back we go
    # the worse the fit can look
    for (i_date in seq(p_start_date - max(overall_params$max_lag, overall_params$training_length),
      max(subset$date),
      by = "day")) {

      i_date <- as.Date(i_date)

      window <- subset |>
        # create historic window
        dplyr::filter(date <= i_date & date >= (i_date - overall_params$max_lag)) |>
        # make sure in correct order
        dplyr::arrange(date) |>
        # create index going backward in time
        dplyr::mutate(j = as.integer(max(date) - date)) |>
        # calculate probability of discharge at j
        dplyr::mutate(discount = plnorm(j, meanlog = lognormal_mu, sdlog = lognormal_sigma)) |>
        # calculate expected discharges at j
        dplyr::mutate(discharges = estimated_admissions * discount) |>
        # remove admissions we think have discharged
        dplyr::mutate(effective = estimated_admissions - discharges)  |>
        # sum up those non-discharged admissions, and add our gamma error term
        dplyr::group_by(.sample, model, prediction_start_date) |>
        dplyr::summarise(.pred = rgamma(1, shape = sum(effective) * beta, rate = beta), .groups = "drop") |>
        dplyr::mutate(date = i_date)


      forecast_occupancy <- forecast_occupancy |>
        dplyr::bind_rows(window)

    }

  }

  return(
    list(
      forecast_occupancy = forecast_occupancy
    )
  )
}



run_occupancy_region <- function(admissions_data, discharge_data, overall_params) {
  # create historic data of "samples" of the true data, to allow for easy joining
  # we need to join historic admissions data onto the parameter samples for discharge
  prepared_admissions <- tidyr::expand_grid(
    # create spine of date, prediction start date, model and each sample instance
    date = seq(min(admissions_data$date), max(admissions_data$date), by = "day"),
    prediction_start_date = unique(na.omit(admissions_data$prediction_start_date)),
    location = unique(na.omit(admissions_data$location)),
    model = unique(na.omit(admissions_data$model)),
    .sample = 1:overall_params$n_pi_sample) |>
    # not all prediction start dates need all dates going backward
    dplyr::filter(date >= prediction_start_date - (overall_params$max_lag + overall_params$training_length) &
      date <= prediction_start_date + overall_params$forecast_horizon - 1) |>
    dplyr::left_join(admissions_data |>
      dplyr::filter(!is.na(.sample) | !is.na(.value)) |>
      dplyr::select(date, prediction_start_date, model, .sample, .value, target_value, location),
    by = c("date", "prediction_start_date", "model", ".sample", "location")) |>
    dplyr::rename(nhs_region_name = location) |>
    dplyr::left_join(discharge_data$training_data, by = c("date", "nhs_region_name")) |>
    dplyr::select(-target_value) |>
    # fill in the empty historic values with true arrival admissions
    dplyr::mutate(.value = dplyr::coalesce(.value, admissions)) |>
    dplyr::left_join(discharge_data$parameters, by = c(".sample" = ".draw", "nhs_region_name")) |>
    dplyr::mutate(estimated_admissions = .value) |>
    # we do this to reduce uncertainty, using the real admissions rather than esetimated
    # we may want to remove this in future and use estimated
    # the single LoS params don't translate to using true admissions it seems
    dplyr::mutate(estimated_admissions = ifelse(date < prediction_start_date, admissions, estimated_admissions))


  # combine our posterior samples with forecasts
  forecast_occupancy <- data.frame()

  # loop over each prediction start date, then each date within the admissions data
  for (p_start_date in unique(na.omit(prepared_admissions$prediction_start_date))) {

    p_start_date <- as.Date(p_start_date)

    print(glue::glue("Running lookback period {p_start_date}"))

    subset <- prepared_admissions |>
      dplyr::filter(prediction_start_date == p_start_date)

    # we want at minimum max_lag data to feed in + enough for historic fit
    # we could instead do max_lag + training length, however, the further back we go
    # the worse the fit can look
    for (i_date in seq(p_start_date - max(overall_params$max_lag, overall_params$training_length),
      max(subset$date),
      by = "day")) {

      i_date <- as.Date(i_date)

      window <- subset |>
        # create historic window
        dplyr::filter(date <= i_date & date >= (i_date - overall_params$max_lag)) |>
        # need region grouping before changing order
        dplyr::group_by(nhs_region_name) |>
        # make sure in correct order
        dplyr::arrange(date) |>
        # create index going backward in time
        dplyr::mutate(j = as.integer(max(date) - date)) |>
        # can ungroup after `j` created
        dplyr::ungroup() |>
        # calculate probability of discharge at j
        dplyr::mutate(discount = plnorm(j, meanlog = lognormal_mu, sdlog = lognormal_sigma)) |>
        # calculate expected discharges at j
        dplyr::mutate(discharges = estimated_admissions * discount) |>
        # remove admissions we think have discharged
        dplyr::mutate(effective = estimated_admissions - discharges)  |>
        # sum up those non-discharged admissions, and add our gamma error term
        dplyr::group_by(.sample, model, prediction_start_date, nhs_region_name) |>
        dplyr::summarise(.pred = rgamma(1, shape = sum(effective) * beta, rate = beta), .groups = "drop") |>
        dplyr::mutate(date = i_date)


      forecast_occupancy <- forecast_occupancy |>
        dplyr::bind_rows(window)

    }

  }

  return(
    list(
      forecast_occupancy = forecast_occupancy
    )
  )
}
