load_covid_google_health <- function(path, path_type, pattern, lookups) {
  #' @param path String, full path and file name of the covid.txt symptoms.
  #' @param path_type Dummy variable to match structure of other such functions.
  #' @param pattern Dummy variable to match structure of other such functions.
  #' @param lookups List from load_lookups.


  google_health <- lookups$rs$lookups.nhs_trusts |>
    dplyr::select("trust_code" = code, postcode) |>

    dplyr::left_join(
      lookups$rs$lookups.nspl21 |>
        dplyr::select("postcode" = pcds, "lad23cd" = laua),
      dplyr::join_by(postcode)
    ) |>

    dplyr::left_join(
      lookups$rs$google.lad_symptom_trends |>
        dplyr::filter(symptom %in% !!readLines(path)) |> # note that `path` is a local filepath within this repo - see config
        tidyr::pivot_wider(
          names_from = symptom,
          names_prefix = "gt_",
          names_sort = TRUE,
          # From https://github.com/UKHSA-Internal/spikeprotein-dbt/blob/main/models/google/subregion_symptom_trends.R:
          #   Google do not provide interest values for "low-interest" search terms;
          #   so any "missing" combinations of `[date, sub_region_2, symptom]`
          #   should be assumed to have `value = 0`.
          values_fill = 0
        ),
      dplyr::join_by(lad23cd)
    ) |>
    # dplyr::filter(!is.na(date)) |> # some trusts don't seem to have any data:
    # # "RA4", "RCD", "RH5", "RNN", "RQF", "RT4", "RTX", "RYT"
    dplyr::select(!c(postcode, lad23cd)) |>
    dplyr::arrange(date, trust_code) |>
    dplyr::collect()

  # do a last observation carried forward for N steps.
  # crude but works..
  n_repeats <- 2

  google_health_last_day <- google_health |>
    dplyr::filter(date == max(date, na.rm = T))

  repeated_last_day <- data.frame()

  for (i in seq_len(n_repeats)) {

    repeated_last_day <- dplyr::bind_rows(repeated_last_day,
      google_health_last_day |> dplyr::mutate(date = date + i))
  }


  google_health <- dplyr::bind_rows(
    google_health,
    repeated_last_day
  )

  list(
    path = path,
    data = google_health,
    min_date = min(google_health$date, na.rm = TRUE), # last line of defence
    max_date = max(google_health$date, na.rm = TRUE) # against the anonymous NA.
  )
}
