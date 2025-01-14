
load_covid_beds <- function(path, path_type, pattern, lookups) {

  rs <- redshift$data_model()

  complete_data <- rs$covid.pancast_covid_hospital_metrics %>%
    dplyr::select(-c(population)) %>%
    dplyr::collect() %>%
    dplyr::left_join(lookups$eric %>%
                       dplyr::select(trust_code, is_acute),
                     by="trust_code")

  list(
    path = path,
    data = complete_data,
    min_date = min(complete_data$date),
    max_date = max(complete_data$date)
  )
}
