

load_rsv_inpatient_tests <- function(path, path_type, pattern, lookups) {
  #' Load an SGSS tests data source to be joined to forecasting
  #'
  #' @param path string. The S3 path to either the full data, or the bucket that will
  #' be searched.
  #' @param lookups list(df). List of dataframes containing all lookup tables used
  #' for the analysis
  #' @param path_type string. Either "static" - individual file given, or "dynamic"
  #' meaning the bucket given will be searched for the most recent file matching
  #' the pattern given.
  #' @param pattern string or NULL. What to match should the path_type be dynamic
  #' @return list. Original path of data source, the processed data frame, and the
  #' minimum and maximum dates available in the data source.


  if (path_type == "dynamic") {
    path <- s3$find_latest_file(path, pattern)
  }

  # bring in all the tests for winter viruses
  sgss <- aws.s3::s3read_using(
    vroom::vroom,
    object = path
  ) %>%
    dplyr::rename(id = `...1`) %>%
    janitor::clean_names()

  sgss_rsv_dedupe <- sgss %>%
    # only care about RSV for this model
    dplyr::filter(organism_species_name == "RESPIRATORY SYNCYTIAL VIRUS (RSV)") %>%
    # We only want english regions
    dplyr::filter(!(phe_region_name %in% c("N IRELAND", "SCOTLAND", "WALES", "CHANNEL ISLANDS", "ISLE OF MAN"))) %>%
    dplyr::filter(trust_name != "PUBLIC HEALTH WALES NHS TRUST") %>%
    # this is an assumption as most useful for policy, but can be changed
    dplyr::filter(requesting_organisation_type_name %in% c("HOSPITAL INPATIENT")) %>% # "HOSPITAL A&E",
    # deduplicate based on time
    # keep the first test an individual has (assumes same episode across A&E and admission)
    dplyr::group_by(psuedo_uid) %>%
    dplyr::arrange(specimen_date, lab_report_date) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    # data input error
    dplyr::filter(patient_age >= 0) %>%
    # tidy names
    dplyr::rename(date = specimen_date,
      age = patient_age) %>%
    dplyr::group_by(date, trust_code, age) %>%
    # we care about how many positive tests by age (aggregate), not individual levels,
    # however, we don't know what age groupings we are going to use, so keep to single
    # ages and we can `cut` later on the analysis side
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(date, age) %>%
    tidyr::pivot_wider(id_cols = c("trust_code", "date"), names_from = "age", names_glue = "n_age_{age}", values_from = "count") %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("n_age_"), ~ dplyr::coalesce(., 0)))



  list(
    path = path,
    data = sgss_rsv_dedupe,
    min_date = min(sgss_rsv_dedupe$date),
    max_date = max(sgss_rsv_dedupe$date)
  )
}
