load_covid_oneoneone_calls <- function(path, path_type, pattern, lookups) {

  oneoneone_calls <- lookups$rs$staging_covid.oneoneone_calls_by_trust |>
    tidyr::pivot_wider(
      id_cols = c(trust_code, date),
      names_from = c(destination_category, age_group),
      values_from = count,
      values_fill = 0
    ) %>%
    dplyr::rename_with(\(x) sub("covid_", "", x)) |>
    dplyr::collect()


  list(
    path = path,
    data = oneoneone_calls,
    min_date = min(oneoneone_calls$date),
    max_date = max(oneoneone_calls$date)
  )
}
