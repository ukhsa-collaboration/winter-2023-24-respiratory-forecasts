# helper function to bring into the agreed forecast schema
format_covid_outputs <- function(.data, target_name, forecast_horizon) {

  .data |>
    dplyr::mutate(
      age_group = "all",
      age_group_granularity = "none",
      target_name = paste0("covid_", target_name),
      forecast_horizon = forecast_horizon
    ) |>
    dplyr::rename(target_value = target) |>
    # I don't love having this in, but unclear where the random `calls` quantile duplicates are coming from...
    dplyr::distinct(.keep_all = TRUE)

}
