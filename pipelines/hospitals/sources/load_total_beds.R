load_total_beds <- function(path, path_type, pattern, lookups) {
  #' Load NHSE total hospital beds data

  complete_data <- lookups$rs$respiratory.respiratory_hospital_metrics |>
    dplyr::select(
      date,
      trust_code,
      dplyr::starts_with("total_")
    ) |>
    # join icb and region
    dplyr::left_join(
      lookups$rs$lookups.nhs_trusts |>
        dplyr::select(
          "trust_code" = code,
          "trust_name" = name,
          "icb_name" = icb23nm,
          "nhs_region_name" = nhser23nm
        ),
      dplyr::join_by(trust_code)
    ) |>
    dplyr::left_join(
      lookups$rs$lookups.nhs_trusts_population,
      dplyr::join_by(trust_code)
    ) |>
    dplyr::relocate(
      date,
      trust_code,
      trust_name,
      icb_name,
      nhs_region_name,
      population
    ) |>
    dplyr::collect()


  list(
    path = path,
    data = complete_data,
    min_date = min(complete_data$date),
    max_date = max(complete_data$date)
  )
}
