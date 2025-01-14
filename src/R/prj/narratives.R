#' @name narratives
#' @section Version: 0.0.1
#'
#' @title Narratives
#'
#' @description Functions for creating narratives.
#'
".__module__."

box::use(
  box / deps_,
  box / help_,
  magrittr[`%>%`]
)

#'
#' Create a .txt file that has current epidemic numbers and forecasting numbers
#' at the desired geographic level.
#'
#' @param data Dataframe that need to be in the formatted model output and must include the columns:
#'  - prediciton_start_date
#'  - model
#'  - location_level
#'  - age_group
#'  - date
#'  - target_value
#'  - pi_5
#'  - pi_90
#'  - pi_50
#'
#'  Other columns may be required when adding other CIs
#'
#' @param target_name Metric that is being forecasted eg admissions
#' @param geography  Location level to be plotted
#' @param age_granularity Age level to be plotted
#' @param model_name Name of model to be plotted
#' @param disease Disease being forecasted
#' @param output_path Character giving the filename prefix, and optionally the parent folder location on the local system.
#'
#' @returns a .txt file containing the current and forecast values (and a 95% CI)
#' of the target metric for the disease, model and geographic level in question
#'
#' @examples
#' # Creating a narrative on current and forecast COVID-19 epidemic numbers at a
#' # national level
#'
#' narrative_txt_output(
#'   data = all_formatted,
#'   target_name = target_name_sym,
#'   model_name = model_name,
#'   geography = "nation",
#'   disease = "COVID-19",
#'   output_path = output_path
#' )
#'
#' @export
narrative_txt_output <- function(data,
                                 target_name,
                                 model_name,
                                 geography,
                                 age_granularity = "none",
                                 disease, output_path,
                                 rounding_level = 0,
                                 is_percent = FALSE) {
  sink(file = paste0(output_path, "/", model_name, "_narrative.txt"), append = TRUE)

  print(paste0("Summary: ", geography, " level (", model_name, ")"))

  current <- data %>%
    dplyr::filter(!is.na(target_value)) %>%
    dplyr::filter(!is.na(prediction_start_date)) %>%
    dplyr::filter(prediction_start_date == max(prediction_start_date)) %>%
    dplyr::filter(model == model_name) %>%
    dplyr::filter(location_level == geography) %>%
    dplyr::filter(age_group_granularity == age_granularity) %>%
    dplyr::mutate(target_7av = round(zoo::rollmean(target_value, k = 7, align = "right", fill = NA), digits = rounding_level)) %>%
    dplyr::filter(date == prediction_start_date - 1) %>%
    dplyr::select(date, location, target_7av)


  projection <- data %>%
    dplyr::filter(prediction_start_date == max(prediction_start_date, na.rm = T)) %>%
    dplyr::filter(model == model_name) %>%
    dplyr::filter(location_level == geography) %>%
    dplyr::filter(age_group_granularity == age_granularity) %>%
    dplyr::group_by(location, age_group) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(dplyr::across(
      dplyr::contains("pi_"),
      \(x) zoo::rollmean(x, k = 7, align = "right", fill = NA)
    )) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::contains("pi_"),
        \(x) round(x, rounding_level)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, location, age_group, pi_50, pi_5, pi_95) %>%
    dplyr::filter(date == max(date))

  latest_date <- unique(current$date)
  latest_av <- current[, c("target_7av", "location")]

  target_name_tidy <- gsub("_", " ", target_name)

  print(glue::glue('As of {format(latest_date, "%d %B %Y")}, the 7-day rolling average for {disease} {target_name_tidy} is {unique(latest_av$target_7av)}{ifelse(is_percent,"%","")}. The projected 7-day rolling average up to the {format(projection$date, "%d %B %Y")} is {projection$pi_50}{ifelse(is_percent,"%","")} (90% CI: {projection$pi_5}{ifelse(is_percent,"%","")} - {projection$pi_95}{ifelse(is_percent,"%","")}).'))

  # Covert occupancy rate to number of beds:
  if (is_percent && geography == "nation" && grepl("occupancy", output_path)) {
    if (!"population" %in% names(data)) {
      if ("total_beds" %in% names(data)) {
        data <- dplyr::rename(data, "total_beds" = "population")
        skip <- FALSE
      } else {
        skip <- TRUE
        print(noquote(paste(
          "No suitable column found to base beds estimate on;",
          "please supply data with a 'population' or 'total_beds' column.")))
      }
    } else {
      skip <- FALSE
    }

    if (!skip) { # I'm sure there's a better control flow version of this...
      beds_base <- data |>
        dplyr::filter(
          date == latest_date,
          location_level == "nation",
          !is.na(population)) |>
        dplyr::arrange(prediction_start_date) |>
        utils::tail(1) |> # pull latest prediction (in case total beds changed)
        dplyr::pull(population)
      skip <- is.na(beds_base) && beds_base < 1 # final check
    }
    if (skip) {
      print(noquote(paste(
        "No valid beds/population data found to base estimate on;",
        "please supply data with a full 'population' column.")))
    } else {
      projected_beds <- dplyr::mutate(
        projection,
        dplyr::across(
          dplyr::starts_with("pi_"), ~ round(. / 100 * beds_base)))
      print(glue::glue("This is approximately {round(latest_av$target_7av/100*beds_base)} currently occupied beds, with an equivalent projection of {projected_beds$pi_50} beds (90% CI: {projected_beds$pi_5} - {projected_beds$pi_95})."))
    }
  }
  sink(file = NULL)
}

#'
#' Create a .txt file that has lists the number of ICB areas in each projection
#' classification and the classification with the majority of ICB areas.
#'
#' @param data Dataframe that need to be in the formatted model output and must include the columns:
#'  - prediciton_start_date
#'  - model
#'  - location_level
#'  - age_group
#'  - date
#'  - target_value
#'  - pi_5
#'  - pi_90
#'  - pi_50
#'
#'  Other columns may be required when adding other CIs
#'
#' @param target_name Metric that is being forecasted eg admissions
#' @param geography  Location level to be plotted
#' @param age_granularity Age level to be plotted
#' @param model_name Name of model to be plotted
#' @param disease Disease being forecasted
#' @param output_path Character giving the filename prefix, and optionally the parent folder location on the local system.
#'
#' @returns a .txt file that has lists the number of ICB areas in each projection
#' classification and the classification with the majority of ICB areas.
#'
#' @examples
#' # Creating a narrative on forecasted COVID-19 projections at a
#' # icb level
#'
#' map_narrative_txt_output(
#'   data = all_formatted,
#'   target_name = target_name_sym,
#'   model_name = model_name,
#'   geography = "icb",
#'   disease = "COVID-19",
#'   output_path = output_path
#' )
#'
#' @export
map_narrative_txt_output <- function(data,
                                     target_name,
                                     model_name,
                                     geography = "icb",
                                     age_granularity = "none",
                                     disease,
                                     output_path) {
  # Filter data according to model specifications

  current <- data %>%
    dplyr::filter(!is.na(target_value)) %>%
    dplyr::filter(!is.na(prediction_start_date)) %>%
    dplyr::filter(prediction_start_date == max(prediction_start_date)) %>%
    dplyr::filter(model == model_name) %>%
    dplyr::filter(location_level == "icb") %>%
    dplyr::filter(age_group_granularity == age_granularity) %>%
    dplyr::filter(date == prediction_start_date - 1) %>%
    dplyr::select(date, location)

  # Count number of ICB areas for each projection classification (increase, decrease and stable)

  projection <- data %>%
    dplyr::filter(prediction_start_date == max(prediction_start_date, na.rm = TRUE) &
      model == model_name &
      location_level == "icb") %>%
    dplyr::mutate(is_projection = ifelse(date >= prediction_start_date, "Projection", "Model fit")) %>%
    dplyr::filter(is_projection == "Projection") %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::select(date, prediction_start_date, location, p_increase, p_decrease, p_stable) %>%
    tidyr::pivot_longer(cols = c("p_increase", "p_decrease", "p_stable"), names_to = "is_projection", values_to = "probability") %>%
    dplyr::group_by(date, prediction_start_date, location) %>%
    dplyr::filter(probability == max(probability)) %>%
    dplyr::mutate(dplyr::across("is_projection", stringr::str_replace, "p_", "")) %>%
    dplyr::ungroup() %>%
    dplyr::count(is_projection)

  latest_date <- unique(current$date)

  overall_trend <- projection %>%
    dplyr::filter(n == max(n, na.rm = TRUE))
  overall_trend <- unique(overall_trend$is_projection)

  stable_n <- ifelse("stable" %in% projection$is_projection,
    projection %>%
      dplyr::filter(is_projection == "stable") %>%
      dplyr::pull(n) %>%
      unique(),
    0)

  decrease_n <- ifelse("decrease" %in% projection$is_projection,
    projection %>%
      dplyr::filter(is_projection == "decrease") %>%
      dplyr::pull(n) %>%
      unique(),
    0)

  increase_n <- ifelse("increase" %in% projection$is_projection,
    projection %>%
      dplyr::filter(is_projection == "increase") %>%
      dplyr::pull(n) %>%
      unique(),
    0)


  cat(glue::glue('As of {format(latest_date, "%d %B %Y")}, there are
                 - {increase_n} ICB areas projected to increase over the next 14 days
                 - {stable_n} ICB areas projected to be stable over the next 14 days
                 - {decrease_n} ICB areas projected to decrease over the next 14 days

                 Overall, the most likely trend projection for ICB areas over the next 14 days is {overall_trend}.'),
    file = paste0(output_path, "/", model_name, "_map_narrative.txt"))
}



#'
#' Create a .txt file that gives length of stay summary statistics at the desired geographic level.
#'
#' @param data Dataframe of samples
#' @param geography  Location level: "nation" or "region"
#' @param disease Disease being modelled
#' @param output_path Character giving the filename prefix, and optionally the parent folder location on the local system.
#'
#' @returns a .txt file gives length of stay median and CI at the desired geographic level.
#'
#' @examples
#' # Creating a narrative for national length of stay
#'
#' los_narrative_txt_output(
#'   data = los_national_samples,
#'   geography = "nation",
#'   disease = "COVID-19",
#'   output_path = output_path
#' )
#'
#' @export
los_narrative_txt_output <- function(data,
                                     geography,
                                     disease,
                                     output_path) {

  if (geography == "nation") {

    median <- data |>
      dplyr::group_by(nhs_region_name, .draw) |>
      dplyr::filter(cd > 0.5) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::summarise(
        pi_50 = stats::quantile(t, 0.5),
        pi_05 = stats::quantile(t, 0.05),
        pi_95 = stats::quantile(t, 0.95)
      ) |>
      dplyr::mutate(
        pi_50 = round(pi_50, digits = 1),
        pi_05 = round(pi_05, digits = 1),
        pi_95 = round(pi_95, digits = 1)
      )

    cat(glue::glue("Nationally, the estimated median length of stay in hospital for {disease} is {median$pi_50} days (90% CI for median: {median$pi_05} - {median$pi_95})."),
      file = paste0(output_path, "/los/los_national_narrative.txt"))
  }

  if (geography == "region") {

    median <- data |>
      dplyr::group_by(nhs_region_name, .draw) |>
      dplyr::filter(cd > 0.5) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::group_by(nhs_region_name) |>
      dplyr::summarise(
        pi_50 = stats::quantile(t, 0.5),
        pi_05 = stats::quantile(t, 0.05),
        pi_95 = stats::quantile(t, 0.95)
      ) |>
      dplyr::mutate(
        pi_50 = round(pi_50, digits = 1),
        pi_05 = round(pi_05, digits = 1),
        pi_95 = round(pi_95, digits = 1)
      )

    median_E <- median |>
      dplyr::filter(nhs_region_name == "East of England")

    median_L <- median |>
      dplyr::filter(nhs_region_name == "London")

    median_M <- median |>
      dplyr::filter(nhs_region_name == "Midlands")

    median_NE <- median |>
      dplyr::filter(nhs_region_name == "North East and Yorkshire")

    median_NW <- median |>
      dplyr::filter(nhs_region_name == "North West")

    median_SE <- median |>
      dplyr::filter(nhs_region_name == "South East")

    median_SW <- median |>
      dplyr::filter(nhs_region_name == "South West")

    # TODO could be made more foolproof matching the region names rather than using indices
    cat(glue::glue("Regionally, the estimated median length of stay in hospital for {disease} is (90% CI for median):
    East of England: {median_E$pi_50} days (90% CI: {median_E$pi_05} - {median_E$pi_95})
    London: {median_L$pi_50} days (90% CI: {median_L$pi_05} - {median_L$pi_95})
    Midlands: {median_M$pi_50} days (90% CI: {median_M$pi_05} - {median_M$pi_95})
    North East and Yorkshire: {median_NE$pi_50} days (90% CI: {median_NE$pi_05} - {median_NE$pi_95})
    North West: {median_NW$pi_50} days (90% CI: {median_NW$pi_05} - {median_NW$pi_95})
    South East: {median_SE$pi_50} days (90% CI: {median_SE$pi_05} - {median_SE$pi_95})
    South West: {median_SW$pi_50} days (90% CI: {median_SW$pi_05} - {median_SW$pi_95})"),
      file = paste0(output_path, "/los/los_regional_narrative.txt"))
  }


}



