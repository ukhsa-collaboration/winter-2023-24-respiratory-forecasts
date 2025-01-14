#' @name write
#' @section Version: 0.0.1
#'
#' @title
#' Support for generating forecast .csv outputs.
#'
#' @description
#' Provides easy formatting of model output data
#' Path is the generic name and does not need to conclude with '.csv'.
#' time is the time character string identifying this particular run
#'
#' @seealso
#' * [write$write_out_forecast_results()]
#'
".__module__."

box::use(box / deps_,
  box / help_,
  box / ops[...]
)

.on_load <- function(ns) {
  deps_$need(
    "glue",
    "vroom"
  )
}


#' Generate .csv of forecast results data.
#'
#' Provides easy formatting of model output data
#' Path is the generic name and does not need to conclude with '.csv'.
#' This will create these directories if necessary.
#' time is a character string identifying this particular model run.
#'
#' @param .data Dataframe containing the forecast results.
#' @param .path Character giving the filename prefix, and optionally the parent folder location on the local system. Should not end in .csv.
#' @param  time Charcter string to be appended to .path, specifying when the model was run.
#'
#' @returns .data (invisibly) - but main purpose is saving out the .csv file in the desired location.
#'
#' @examples
#' write$write_out_forecast_results(
#'   .data = forecasted_result,
#'   .path = "outputs/forecast_results",
#'   time = "2023-07-18_14_37_10")
#'
#' @export
write_out_forecast_results <- function(.data, .path, time) {
  # folder output should be in
  parent <- dirname(.path)
  # create directory if it doesn't exist
  dir.create(file.path(parent), recursive = TRUE, showWarnings = FALSE)

  vroom::vroom_write(x = .data,
    file = glue::glue("{.path}_{time}.csv.gz"),
    progress = TRUE)

}
