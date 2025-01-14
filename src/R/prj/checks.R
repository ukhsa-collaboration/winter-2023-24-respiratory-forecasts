#' @name checks
#' @section Version: 0.0.1
#'
#' @title
#' Checks to ensure data is in the right format for further processing
#'
#' @seealso
#' * [checks$check_forcast_format_sample()]
#' * [checks$check_forcast_format_summary()]
".__module__."

# TODO typos in function names!! for(e)cast

box::use(box / deps_,
  box / help_,
)
.on_load <- function(ns) {
  deps_$need(
    "glue",
    "stringr"
  )
}

#' Check forecast data format for summaries (quantiles)
#'
#' Checks the format of data to make sure it is compliant with agreed forecast model schema:
#' * All required columns are present
#' * No additional columns
#' * No duplicate rows
#'
#' @param data data frame from formatted model output
#'
#' @returns `TRUE` (invisibly) if the check was successful; otherwise,
#'  an error will be thrown with an informative message.
#'
#' @examples
#' checks$check_forcast_format_summary(calls_formatted)
#'
#' @export
check_forcast_format_summary <- function(.data) {


  required_columns <- c(
    "model", "prediction_start_date",
    "location", "location_level",
    "age_group", "age_group_granularity",
    "target_name", "target_value",
    "date",
    "forecast_horizon",
    "pi_50", "pi_95", "pi_5",
    "population",
    "p_increase", "p_decrease", "p_stable"
  )

  missing_columns <- setdiff(
    required_columns, names(.data)
  )

  if (length(missing_columns)) stop(glue::glue("The following columns are missing: {missing_columns}"))

  extra_columns <- setdiff(
    names(.data), required_columns
  )

  # having extra pi's is ok
  extra_columns <- extra_columns[!stringr::str_starts(extra_columns, "pi")]


  if (length(extra_columns)) stop(glue::glue("The following columns should not be in the dataframe: {extra_columns}"))

  duplicate_rows <- .data[.data |> duplicated(), ]

  if (nrow(duplicate_rows) > 0) stop(glue::glue("There are duplicated rows in this dataframe"))

  invisible(TRUE)
}



#' Check forecast data format for sample (draws from posterior)
#'
#' Checks the format of data to make sure it is compliant with agreed forecast model schema:
#' * All required columns are present
#' * No additional columns
#' * No duplicate rows
#'
#' @param data data frame from formatted model output
#'
#' @returns `TRUE` (invisibly) if the check was successful; otherwise,
#'  an error will be thrown with an informative message.
#'
#' @examples
#' checks$check_forcast_format_sample(calls_formatted)
#'
#' @export
check_forcast_format_sample <- function(.data) {


  required_columns <- c(
    "model", "prediction_start_date",
    "location", "location_level",
    "age_group", "age_group_granularity",
    "target_name", "target_value",
    "date",
    "forecast_horizon",
    ".sample", ".value",
    "population"
  )

  missing_columns <- setdiff(
    required_columns, names(.data)
  )

  if (length(missing_columns)) stop(glue::glue("The following columns are missing: {missing_columns}"))

  extra_columns <- setdiff(
    names(.data), required_columns
  )

  # having extra pi's is ok
  extra_columns <- extra_columns[!stringr::str_starts(extra_columns, "pi")]


  if (length(extra_columns)) stop(glue::glue("The following columns should not be in the dataframe: {extra_columns}"))

  duplicate_rows <- .data[.data |> duplicated(), ]

  if (nrow(duplicate_rows) > 0) stop(glue::glue("There are duplicated rows in this dataframe"))

  if (length(unique(.data$.sample)) <= 1) stop ("There's only one or fewer samples in this data")

  invisible(TRUE)
}
