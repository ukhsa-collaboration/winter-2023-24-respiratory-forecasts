box::use(
  box / redshift
)

load_lookups <- function(lookup_paths_list) {
  # load lookups to be used across all diseases and pipeline choices

  # Create a data model to work with
  rs <- redshift$data_model()

  ###################################
  #### POSTCODE DISTRICT TO LTLA ####
  ###################################

  # TODO fix names in Redshift
  # TODO this is obsolete once influenza oneoneone tables are built in Redshift
  postcode_district_to_ltla <- rs$staging_covid.district_to_ltla |>
    dplyr::rename(
      postcode_district = postcodedistrict,
      ltla_code = llta_code
    ) |>
    dplyr::collect()


  ############################################
  #### INFLUENZA AND RSV SYMPTOMS FOR 111 ####
  ############################################

  flu_rsv_symptoms <- aws.s3::s3read_using(
    readxl::read_excel,
    object = lookup_paths_list$winter_symptoms_path,
    range = readxl::cell_cols("B:D")
  )


  #################################
  ##### TRUST CHARACTERISTICS #####
  #################################

  # TODO do we need all of this info here?

  eric <- rs$lookups.nhs_trusts |>
    dplyr::mutate(
      "trust_code" = code,
      "commissioning_region" = REPLACE(national_grouping_name, " COMMISSIONING REGION", ""),
      "trust_type_clean" = type,
      "acute_type" = subtype,
      "is_acute" = (type == "acute"),
      "is_teaching" = (subtype == "teaching"),
      .keep = "none"
    ) |>
    dplyr::left_join(
      rs$lookups.nhs_trusts_eric |>
        dplyr::mutate(
          trust_code,
          "type_of_medical_records" = REGEXP_REPLACE(type_of_medical_records_select, "[0-9]\\. ", ""),
          .keep = "none"
        ),
      dplyr::join_by(trust_code)
    ) |>
    dplyr::collect()


  output <- list(
    rs = rs,
    postcode_district_to_ltla = postcode_district_to_ltla,
    flu_rsv_symptoms = flu_rsv_symptoms,
    eric = eric
  )

  return(output)
}
