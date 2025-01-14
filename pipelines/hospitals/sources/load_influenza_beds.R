load_influenza_beds <- function(path, path_type, pattern, lookups) {
  #' Load NHSE influenza hospital admissions and occupancy data

  complete_data <- lookups$rs$respiratory.respiratory_hospital_metrics %>%
    dplyr::select(date,
      trust_code,
      admissions = flu_admissions,
      occupancy_general_acute = flu_occupancy_general_acute,
      occupancy_critical_care = flu_occupancy_critical_care) %>%
    dplyr::collect()


  list(
    path = path,
    data = complete_data,
    min_date = min(complete_data$date),
    max_date = max(complete_data$date)
  )
}
