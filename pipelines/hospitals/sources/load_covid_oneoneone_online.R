load_covid_oneoneone_online <- function(path, path_type, pattern, lookups) {

  online <- lookups$rs$staging_covid.oneoneone_online_by_trust |>
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
    data = online,
    min_date = min(online$date),
    max_date = max(online$date)
  )
}
