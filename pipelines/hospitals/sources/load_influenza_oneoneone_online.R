load_influenza_oneoneone_online <- function(path, path_type, pattern, lookups) {
  #' Load an NHS 111 online data source to be joined to forecasting
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

  online_processed <- lookups$rs$staging_covid.oneoneone_online %>%
    dplyr::rename(date = session_end_date,
      age_5yr = age_band) %>%
    dplyr::filter(date > "2022-06-01") %>%
    dplyr::mutate(date = as.Date(date)) %>%
    # change this filter to make running quicker
    dplyr::filter(
      !is.na(postcode_district),
      final_dx_code != "PATHWAY_NOT_FOUND",
      !is.na(final_dx_description)
    ) %>%

    # Group dx codes into severity categories
    dplyr::mutate(severity = dplyr::case_when(
      final_dx_code %in% c("Dx01010", "Dx011", "Dx0111", "Dx0112", "Dx01120", "Dx01121",
        "Dx01125", "Dx0113", "Dx0114", "Dx0115", "Dx0117", "Dx0118", "Dx012",
        "Dx0121", "Dx01213", "Dx01214", "Dx0122", "Dx0124", "Dx0126", "Dx0127",
        "Dx013", "Dx016", "Dx0162") ~ "Secondary_Care", # ambulance
      final_dx_code %in% c("Dx02", "Dx03", "Dx89", "Dx92", "Dx94") ~ "Secondary_Care", # A&E
      final_dx_code %in% c("Dx05", "Dx06", "Dx07", "Dx08", "Dx09", "Dx10", "Dx16", "Dx75",
        "Dx11", "Dx12", "Dx13", "Dx14", "Dx15") ~ "Primary_Care",
      final_dx_code %in% c("Dx80", "Dx85", "Dx86", "Dx87") ~ "Prescription",
      final_dx_code %in% c("Dx60") ~ "Optician",
      final_dx_code %in% c("Dx118", "Dx17", "Dx18", "Dx19", "Dx20", "Dx21", "Dx22") ~ "Dental",
      final_dx_code %in% c("Dx39") ~ "Self_Care",
      final_dx_code %in%  c("Dx108") ~ "No_Service",
      T ~ "Other"
    )) %>%
    # Irrelevant to diseases of study
    dplyr::filter(!severity %in% c("Dental", "Prescription", "Optician", "No_Service"),
      !stringr::str_detect(final_dx_description, "(COVID)|(corona)")) %>%

    # influenza symptoms
    dplyr::mutate(
      pathway_start = tolower(pathway_start),
      pathway_end = tolower(pathway_end)
    ) %>%

    dplyr::collect() %>%

    dplyr::left_join(lookups$flu_rsv_symptoms %>%
      dplyr::select(pathway,
        start_influenza = influenza_symptom),
    by = c("pathway_start" = "pathway")) %>%
    dplyr::left_join(lookups$flu_rsv_symptoms %>%
      dplyr::select(pathway,
        end_influenza = influenza_symptom),
    by = c("pathway_end" = "pathway")) %>%
    # filter out symptoms (potentially) not related to influenza and pregnancies
    dplyr::filter(start_influenza != "no" &
      end_influenza != "no" &
      !is.na(end_influenza) &
      !(end_influenza %in% c("maybe", "pregnant"))) %>%
    dplyr::select(-final_dx_code, -final_dx_description, -dx_count, -start_influenza, -end_influenza) %>%
    # Group broadly similar symptoms together
    dplyr::mutate(
      broad_symptom = dplyr::case_when(
        stringr::str_detect(pathway_end, "breath") ~ "Breathing_problems",
        stringr::str_detect(pathway_end,
          "(diarrhoea)|(vomit)|(nausea)") ~ "GI_problems",
        stringr::str_detect(pathway_end, "(cold)|(flu)") ~ "Cold_and_flu_symptoms",
        stringr::str_detect(pathway_end, "cough") ~ "Cough",
        stringr::str_detect(pathway_end, "(tired)|(fatigue)") ~ "Fatigue",
        stringr::str_detect(pathway_end, "(fever)|(high temperature)") ~ "Fever",
        stringr::str_detect(pathway_end, "sore throat") ~ "Sore_throat",
        stringr::str_detect(pathway_end, "pain") &
          stringr::str_detect(pathway_end, "abdominal", negate = TRUE)
        ~ "General_aches_and_pains",
        stringr::str_detect(pathway_end, "headache") ~ "Headache",
        stringr::str_detect(pathway_end, "(nose)|(nasal)") ~ "Stuffy_nose",
        TRUE ~ NA
      )
    ) %>%
    dplyr::filter(!is.na(broad_symptom)) %>%

    # Group age bands together into groups
    dplyr::mutate(
      age_5yr = stringr::str_replace_all(age_5yr, c("5yrAgeBand " = "", "5yrAgeBand_" = "", "yrAgeBand " = "")),
      age_5yr = dplyr::case_when(
        age_5yr == "0-4" ~ "00_04",
        age_5yr == "5-9" ~ "05_09",
        age_5yr == "10-14" ~ "10_14",
        age_5yr == "15-19" ~ "15_19",
        age_5yr == "20-24" ~ "20_24",
        T ~ NA),
      age_5yr = factor(age_5yr,
        levels = c("00_04", "05_09", "10_14", "15_19", "20_24")),
      age_10yr = dplyr::case_when(
        age_5yr %in% c("05_09", "10_14") ~ "05_14",
        age_5yr %in% c("15_19", "20_24") ~ "15_24",
        T ~ NA),
      age_10yr = factor(age_10yr,
        levels = c("05_14", "15_24")),
    ) %>%
    dplyr::filter(!is.na(age_5yr)) %>%

    # match ltla code to postcode. missing ltla_codes are either: missing postcodes, or welsh (most)
    dplyr::left_join(lookups$postcode_district_to_ltla,
      by = dplyr::join_by(postcode_district)) %>%
    dplyr::filter(!is.na(ltla_code)) %>%
    dplyr::select(-postcode_district) %>%
    dplyr::left_join(lookups$rs$lookups.population_mapping_trust_ltla |> dplyr::collect(),
      by = dplyr::join_by(ltla_code),
      relationship = "many-to-many") %>%
    dplyr::filter(!is.na(p_geo)) %>%
    dplyr::filter(!is.na(trust_code))

  # aggregate from postcode to trust
  # count data, therefore weighted sum

  # data grouped by broad symptom and 5yr age group
  online_broad_symptom_age_5yr <- online_processed %>%
    dplyr::filter(!is.na(age_5yr)) %>%
    dplyr::mutate(age_group = age_5yr) %>%
    dplyr::group_by(
      trust_code, broad_symptom, age_group, date) %>%
    dplyr::summarise(count = sum(count * p_geo), .groups = "drop_last") %>%
    dplyr::arrange(date)

  # data grouped by severity and 5yr age group
  online_severity_age_5yr <- online_processed %>%
    dplyr::filter(!is.na(age_5yr)) %>%
    dplyr::mutate(age_group = age_5yr) %>%
    dplyr::group_by(
      trust_code, severity, age_group, date) %>%
    dplyr::summarise(count = sum(count * p_geo), .groups = "drop_last") %>%
    dplyr::arrange(date)

  # data grouped by broad symptom and 10yr age group
  online_broad_symptom_age_10yr <- online_processed %>%
    dplyr::filter(!is.na(age_10yr)) %>%
    dplyr::mutate(age_group = age_10yr) %>%
    dplyr::group_by(
      trust_code, broad_symptom, age_group, date) %>%
    dplyr::summarise(count = sum(count * p_geo), .groups = "drop_last") %>%
    dplyr::arrange(date)

  # data grouped by severity and 10yr age group
  online_severity_age_10yr <- online_processed %>%
    dplyr::filter(!is.na(age_10yr)) %>%
    dplyr::mutate(age_group = age_10yr) %>%
    dplyr::group_by(
      trust_code, severity, age_group, date) %>%
    dplyr::summarise(count = sum(count * p_geo), .groups = "drop_last") %>%
    dplyr::arrange(date)

  online <- dplyr::bind_rows(
    online_broad_symptom_age_10yr,
    online_severity_age_10yr,
    online_broad_symptom_age_5yr,
    online_severity_age_5yr
  )

  online_wider <- online %>%
    tidyr::pivot_wider(
      id_cols = c(trust_code, date),
      names_from = c(age_group, broad_symptom, severity),
      values_from = c(count),
      values_fill = 0,
      names_glue = "online_{broad_symptom}_{age_group}_{severity}{sub('count', '', .value)}"
    ) %>%
    dplyr::mutate(prov_online = path) %>%
    dplyr::ungroup()



  list(
    path = path,
    data = online_wider,
    min_date = min(online_wider$date),
    max_date = max(online_wider$date)
  )
}
