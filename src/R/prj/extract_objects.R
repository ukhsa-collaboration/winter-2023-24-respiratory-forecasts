#' @name extract_objects
#' @section Version: 0.0.1
#'
#' @title
#' Create individual model sample predictions
#'
#' @description
#' Extract prediction outputs in a nicer format.
#' Relies on structure of foreach list creation currently used.
#'
#' @seealso
#' * [extract_objects$extract_from_list()]
#'
".__module__."

box::use(
  box / deps_,
  box / help_,
  box / ops[...],
)

.on_load <- function(ns) {
  deps_$need(
    "purrr"
  )
}

#' Function to extract formatted dataframes and model objects
#'
#' The function binds the list elements by row.
#'
#' @param model_outputs Individual model output from run_model
#'
#' @returns List with dataframe with formatted predictions and model objects
#'
#' @export
extract_from_list <- function(model_outputs) {
  # we want to bind all prediction dates, and access model by prediction date
  sample_predictions <- model_outputs |>
    purrr::map("sample_predictions") |>
    purrr::list_rbind()

  # Yes okay, we have to iterate twice, but that's a small price to pay
  models <- purrr::map(model_outputs, "model")

  return(list(
    sample_predictions = sample_predictions,
    models = models
  ))
}
