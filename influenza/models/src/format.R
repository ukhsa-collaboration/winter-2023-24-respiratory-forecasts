# helper function to bring into the agreed forecast schema
format_influenza_outputs <- function(.data) {

  .data |>
    dplyr::mutate(age_group = "all",
      age_group_granularity = "none",
      target_name = paste0("influenza_", as.character(overall_params$target_name)),
      forecast_horizon = overall_params$forecast_horizon
    ) |>
    dplyr::rename(target_value = target) |>
    dplyr::distinct(.keep_all = TRUE)

}
