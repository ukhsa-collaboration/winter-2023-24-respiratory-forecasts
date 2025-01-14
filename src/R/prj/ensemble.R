#' @name ensemble
#' @section Version: 0.0.1
#'
#' @title
#' Create ensemble model predictions
#'
#' @description
#' Combine predictions from multiple models to create ensemble model predictions.
#' This can be done via averaging prediction intervals or using individual prediction samples.
#'
#' @seealso
#' * [ensemble$ensemble_from_points()]
#' * [ensemble$ensemble_from_samples()]
#'
".__module__."

box::use(
  box / deps_,
  box / help_,
  box / ops[...],
  prj / intervals
)

# TODO
# Add more ensemble `methods`
# Add function to ensemble_from_samples using HDI

.on_load <- function(ns) {
  deps_$need(
    "dplyr",
    "glue",
    "tidyr"
  )
}

#' Ensemble models using prediction intervals
#'
#' Takes prediction intervals from multiple models and averages them. Currently only supports "mean" ensemble method.
#'
#' @param .data Dataframe of rows of prediction intervals from models to be ensembled - can contain historic non-predicted data, though will throw warning.
#' @param method String for ensemble method used
#' @param model_name String for a model name to assign to the ensemble, e.g. string containing names of models included in the ensemble
#'
#' @returns Dataframe of prediction intervals for the ensemble model
#'
#' @export
ensemble_from_points <- function(.data, method = "mean", model_name) {

  if (method == "mean") {
    ensemble <- .data |>
      # easier to average in long format rather than across imo
      tidyr::pivot_longer(cols = dplyr::starts_with("pi_"),
        names_to = ".quantile",
        values_to = ".value") |>
      # fixes different lengths of ensemble models
      # this will raise a warning due to the back data not having a model name
      dplyr::group_by(model, prediction_start_date) |>
      dplyr::mutate(first_date = min(date[!is.na(.value)])) |>
      dplyr::ungroup() |>
      dplyr::group_by(prediction_start_date) |>
      dplyr::mutate(.value = dplyr::case_when(
        date < max(is.finite(first_date)) ~ NA_real_,
        T ~ .value
      )) |>
      dplyr::ungroup() |>
      dplyr::select(-c("first_date")) |>
      # perform averaging
      dplyr::group_by(dplyr::across(-c("model", ".value"))) |>
      dplyr::summarise(.value = mean(.value)) |>
      dplyr::ungroup() |>
      # transform back to wide
      tidyr::pivot_wider(names_from = ".quantile", values_from = .value)
  }

  ensemble |>
    dplyr::mutate(model = dplyr::case_when(
      !is.na(pi_50) ~ glue::glue("{method}_ensemble_{model_name}"),
      T ~ NA))
}

#' Ensemble models using prediction samples
#'
#' Takes individual prediction samples from multiple models to produce prediction intervals for an ensemble model.
#'
#' Currently only supports "mellor" ensemble method, which uses [intervals$samples_to_quantiles()].
#'
#' @param .sample_predictions Dataframe of rows of individual prediction samples from models to be ensembled
#' @param identifiers Vector of column names used to identify a record in the aggregate data
#' @param remove_identifiers Vector of column names to remove for aggregating, e.g. lower spatial identifiers than those desired
#' @param method String for ensemble method used
#' @param model_name String for a model name to assign to the ensemble, e.g. string containing names of models included in the ensemble
#'
#' @returns Dataframe of training data and prediction intervals for the ensemble model
#'
#' @export
ensemble_from_samples <- function(.sample_predictions,
                                  remove_identifiers = c("trust_code", "nhs_region_name"),
                                  method = "mellor",
                                  model_name,
                                  overall_params) {
  # truncate to shortest model time series
  .sample_predictions <- .sample_predictions |>
    dplyr::group_by(model, prediction_start_date) %>%
    dplyr::mutate(first_date = min(date)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(prediction_start_date) %>%
    dplyr::filter(date >= max(first_date)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-first_date)

  if (method == "mellor") {
    ensemble <- .sample_predictions |>
      intervals$samples_to_quantiles(
        remove_identifiers = remove_identifiers,
        overall_params = overall_params)
  }

  ensemble |>
    dplyr::mutate(model = dplyr::case_when(
      !is.na(pi_50) ~ glue::glue("{method}_ensemble_{model_name}"),
      T ~ NA))
}
