#' @name intervals
#' @section Version: 0.0.2
#'
#' @title
#' Support for working at the interface between model samples and prediction intervals.
#'
#' @description
#' Module to allow conversion between `mgcv` model outputs to "posterior" style samples,
#' converting between levels of aggregation and converting to prediction intervals.
#' Broadly this is the "propogate uncertainty" module for forecasts.
#'
#' @seealso
#' * [intervals$generate_samples()]
#' * [intervals$generate_intervals()]
#' * [intervals$aggregate_samples()]
#' * [intervals$samples_to_quantiles()]
#' * [intervals$discretise_trends()]
#'
".__module__."


box::use(
  box / deps_,
  box / help_
)

.on_load <- function(ns) {
  deps_$need(
    "bayestestR",
    "data.table",
    "dplyr",
    "dtplyr",
    "mgcv>=1.9.0",
    "purrr",
    "stats",
    "tidyr"
  )
  deps_$need(
    "gratia>=0.8.1.38",
    msg = "We need the development version of gratia, for now!",
    install_cmd = quote(remotes::install_github("gavinsimpson/gratia"))
  )

  # dplyr needs to be attached for some dtplyr functions to work properly
  if (!"package:dplyr" %in% search())
    attachNamespace(loadNamespace("dplyr"))
}



#' Standardised set of quantiles for creating prediction intervals.
#'
#' Takes sample data and produces prediction intervals for pre-defined
#' quantiles. Note that quantiles of this form are an assumption. Data must be
#' grouped by whatever covariates or time/space identifiers *before* calling
#' this function.
#'
#' Alternative quantile approaches are available [see this
#' reference](https://easystats.github.io/bayestestR/articles/credible_interval.html)
#'
#' @param .data Input data frame of processed prediction samples (not directly
#'   from [intervals$generate_samples()]) which contains the column `.value`,
#'   the quantity to be summarized. One row per sample per covariate group.
#' @param method Whether to calculate using standard quantile approach or HDI
#'   (`"quantile"` or `"hdi"`).
#'
#' @returns Data frame with one row per set of groupings, rather than one row
#'   per sample (input)
#'
#' @examples
#' formatted_samples |>
#'   dplyr::group_by(date, location) |>
#'   intervals$generate_intervals() |>
#'   dplyr::ungroup()
#'
#' @export
generate_intervals <- function(.data, method = c("quantile", "hdi")) {

  method <- match.arg(method)

  nested <- switch(
    method,

    # allowing NA's through because this will be applied to empty data where there are
    # no predictions
    "quantile" = dplyr::summarise(
      .data,
      # TODO we could avoid list column here (and tidyr::unnest_wider() later)
      # if dtplyr::summarise() weren't slightly broken... https://github.com/tidyverse/dtplyr/issues/454
      "pi" = list(
        c(0.5, 0.05, 0.95, 0.025, 0.975, 0.25, 0.75, 0.17, 0.83) |>
          stats::quantile(.value, probs = _, na.rm = TRUE) |>
          purrr::set_names(\(x) paste0("pi_", x) |> sub("%$", "", x = _))
      ),
      .groups = "drop"
    ) |>
      # TODO currently have to collect() as there is no dtplyr method for unnest_wider()
      dplyr::collect(),


    # unclear whether NA can be passed through
    # unclear what the appropriate central value should be (pi_50) so have gone
    # with median
    "hdi" = dplyr::summarise(
      .data,
      "pi_50" = stats::quantile(.value, probs = 0.5, na.rm = TRUE), # median
      "pi" = list(
        c(0.9, 0.95, 0.5, 0.66) |>
          bayestestR::hdi(.value, ci = _, na.rm = TRUE) |>
          tidyr::pivot_wider(names_from = CI, values_from = !CI, names_vary = "slowest") |>
          purrr::set_names(paste0("pi_", 100 * c(0.05, 0.95, 0.025, 0.975, 0.25, 0.75, 0.17, 0.83)))
      ),
      .groups = "drop"
    ) |>
      dplyr::collect()
  )

  tidyr::unnest_wider(nested, pi)
}



#' Take low-level prediction samples and aggregate them to coarser covariates.
#'
#' Prediction samples are produced at the lowest level possible, which gives the
#' flexibility to aggregate predictions so e.g. higher geographies (Trust -> region).
#' This is prefered to aggregating intervals, as it preserves the uncertainty of the
#' modelled predictions more robustly.
#'
#' This is quite opinionated with what columns it expects within the dataframe, beware.
#' These include: .value, population, target
#'
#' @param .sample_predictions Dataframe of one row per sample per set of covariates.
#' @param remove_identifiers Vector of column names which appear in the model predictions, but we
#' do not want to be aggregated by. All covariates not specified in this arguement, or aggregated,
#' will be grouped by. In the case where the data is at the correct aggregation, or no variables
#' need to be removed, a NULL should be provided.
#'
#' @returns Dataframe of one row per sample per set of non-removed covariates.
#'
#' @examples
#' # Consider we want to aggregate from trust to region level, we need to remove trust and icb identifiers
#' trust_samples |>
#'   intervals$aggregate_samples(remove_identifiers = c("trust_code", "icb_name"))
#'
#' @export
aggregate_samples <- function(
    .sample_predictions,
    remove_identifiers = c("trust_code", "icb_name", "nhs_region_name", "age_group")
    ) {

  .sample_predictions |>
    # calcualte the summed up predictions to the level we want
    dplyr::summarise(
      .value = sum(.value, na.rm = T),
      population = sum(population, na.rm = T),
      target = if (all(is.na(target))) NA_real_ else sum(target, na.rm = TRUE),
      .by = !dplyr::all_of(c(remove_identifiers, ".value", "population", "target"))
    )

}


#' Take prediction samples and categorise them as changes in rates.
#'
#' We want to define categories "increase", "decrease", "stable", and assign them
#' probabilities. We do this via the change from the true target data `forecast_horizon`
#' days ago. We work in per-capita-rates to avoid different sized geographies being an issue.
#'
#' Assumes data is aggregated to the required level without extra covariates.
#' Rates are automatically converted to per 100k for ease of explaining thresholds.
#' .sample_predictions must have "date", ".value", "target", "population" columns.
#'
#' @param .sample_predictions Dataframe of one row per sample per set of covariates.
#' @param upper_rate The per capita (100k) rate change above which the trend is categorised as "increase".
#' @param lower_rate The per capita (100k) rate change below which the trend is categorised as "decrease".
#' @param forecast_horizon how many days ahead a prediction counts as a change, used to calculate change in value.
#'
#' @returns .samples_predictions with the additional columns `p_increase`, `p_decrease`, `p_stable`.
#'
#' @examples
#' formatted_samples |>
#'   intervals$discretise_trends(
#'     upper_rate = 0.1,
#'     lower_rate = -0.1,
#'     forecast_horizon = 14
#'   )
#'
#' @export
discretise_trends <- function(.sample_predictions,
                              upper_rate,
                              lower_rate,
                              forecast_horizon,
                              absolute_threshold = TRUE) {

  discrete_trends <- .sample_predictions |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      # work out what the true values were at a time step away
      # work in rates so thresholds are not dependent of geography population size
      "target_n_avg" =  (target / population) |>
        dplyr::lag(n = forecast_horizon) |>
        # roll average to remove day-of-week seasonality (allows more robust comparison)
        data.table::frollmean(7, align = "right", na.rm = TRUE),

      ".value_avg" = (.value / population) |>
        data.table::frollmean(7, align = "right", na.rm = TRUE),

      ".value_n_avg" = (.value / population) |>
        dplyr::lag(n = forecast_horizon) |>
        data.table::frollmean(7, align = "right", na.rm = TRUE),

      .by = !c(date, .value, target, population)
    )
  # probability is proportion of samples above / below threshold
  if (is.null(absolute_threshold) || absolute_threshold == TRUE ||
    grepl("^abs", absolute_threshold, ignore.case = TRUE)) {
    discrete_trends <- discrete_trends |>
      dplyr::mutate(
        "rate_diff" = .value_avg - .value_n_avg, # default absolute method
        "p_increase" = mean(rate_diff > upper_rate),
        "p_decrease" = mean(rate_diff <= lower_rate),
        "p_stable" = 1 - (p_increase + p_decrease),
        rate_diff = NULL, # we don't need this any more
        .by = !c(.sample, .value, target, population, model,
          .value_avg, .value_n_avg, target_n_avg),
        .keep = "unused"
      ) |>
      dplyr::select(- "target_n_avg") # now done automatically in relative case.
  } else { # for relative changes; needs quite high thresholds, ~ 20-50%.
    discrete_trends <- discrete_trends |>
      # dplyr::filter(.value_n_avg > 0) |> # 0 breaks the rate diff calculation
      dplyr::mutate(
        "rate_diff" = dplyr::case_when( # relativity methods
          # This is the primary way: compare current model to lagged model:
          .value_n_avg > 0 ~ (.value_avg - .value_n_avg) / .value_n_avg,
          # This is the backup way: compare model to lagged real world value:
          .value_n_avg == 0 & target_n_avg > 0 ~
            (.value_avg - target_n_avg) / target_n_avg,
          # Catch any stragglers; forces increase/decrease based just on model
          .value_n_avg == 0 & .value_avg > 0 ~ upper_rate + 0.01, # always p_inc
          .value_n_avg == 0 & .value_avg < 0 ~ lower_rate - 0.01, # always p_dec
          .value_n_avg == 0 & .value_avg == 0 ~ 0, # always stable
          TRUE ~ 0), # final catch-all; always stable
        "p_increase" = mean(rate_diff > upper_rate),
        "p_decrease" = mean(rate_diff <= lower_rate),
        "p_stable" = 1 - (p_increase + p_decrease),
        rate_diff = NULL, # we don't need this any more
        .by = !c(.sample, .value, target, population, model,
          .value_avg, .value_n_avg, target_n_avg),
        .keep = "unused"
      ) # tried separating the rate diff out, and it duplicated points in plots
  }
  discrete_trends <- discrete_trends |> # this part's not grouped so safe to do
    dplyr::relocate(p_stable, .after = p_increase) # outside of the if-else
}

#' Take prediction samples and convert to prediction intervals.
#'
#' Essentially a wrapper function of:
#'  - `aggregate_samples()`
#'  - `discretise_trends()`
#'  - `generate_intervals()`
#'  Which summarizes our prediction samples into communicable formats.
#'
#' @param .sample_predictions Dataframe of one row per sample per set of covariates.
#' @param remove_identifiers Vector of column names which appear in the model predictions, but we
#' do not want to be aggregated by. All covariates not specified in this arguement, or aggregated,
#' will be grouped by. In the case where the data is at the correct aggregation, or no variables
#' need to be removed, a NULL should be provided.
#' @param overall_params List of hyperparameters for the models / project. Must contain
#' $threshold_params$upper_rate, threshold_rates$lower_rate - which determine the discretisation
#' and $forecast_horizon, how far we are predicting into the future.
#'
#' @returns Dataframe of one row per covariate group, with quantiles and probabilities of trends.
#'
#' @examples
#' # Consider we want to aggregate from trust to region level, we need to remove trust and icb identifiers
#' trust_samples |>
#'   intervals$samples_to_quantiles(
#'     remove_identifiers = c("trust_code", "icb_name"),
#'     overall_params = list(forecast_horizon = 14,
#'       threshold_rates = list(upper_rate = 0.1,
#'         lower_rate = -0.1)))
#'
#' @export
samples_to_quantiles <- function(
    .sample_predictions,
    remove_identifiers = c("trust_code", "nhs_region_name", "icb_name",
      "age_group"),
    overall_params,
    method = "quantile"
    ) {

  unavailable_identifiers <- setdiff(remove_identifiers,
    names(.sample_predictions))
  if (length(unavailable_identifiers) != 0) {
    stop(
      "Columns named in `remove_identifiers` must be present in `.data`!\n",
      "These columns were not available: ",
      paste(unavailable_identifiers, collapse = ", "))
  }

  .sample_predictions |>
    dtplyr::lazy_dt() |>
    aggregate_samples(remove_identifiers = remove_identifiers) |>
    discretise_trends(
      upper_rate = overall_params$threshold_rates$upper_rate,
      lower_rate = overall_params$threshold_rates$lower_rate,
      forecast_horizon = overall_params$forecast_horizon,
      absolute_threshold = overall_params$threshold_type
    ) |>
    # group by the unique covariates
    dplyr::group_by(dplyr::across(!c(.sample, .value, model))) |>
    # find the summary stats we want
    generate_intervals(method = method) |>
    dplyr::ungroup() |>
    dplyr::collect() # TODO this collect() is redundant right now,
  # as we are forced to collect() within generate_intervals()
}



#' Generate prediction samples from fit `mgcv` and new data using the `gratia` package
#'
#' Thin wrapper for the `gratia::posterior_samples()` function, which gives us
#' model prediction samples.
#'
#' [gratia package version](https://gavinsimpson.github.io/gratia/reference/predicted_samples.html)
#'
#' @param .data Dataframe containing the historic (train) and future (test) data.
#' @param .model Fitted `mgcv` object.
#' @param .n_pi_samples Integer number of samples to generate from model.
#'
#' @returns Matrix with one row per `.data` input, and one column per `.n_pi_samples`.
#'
#' @examples
#' model_samples <- intervals$generate_samples(
#'   .model = mgcv_model,
#'   .data = training_data,
#'   .n_pi_samples = 1000)
#'
#' @export
generate_samples <- function(.data, .model, .n_pi_samples = 500) {

  results <- gratia::posterior_samples(
    model = .model,
    data = .data,
    n = .n_pi_samples,
    method = "gaussian"
  ) |>
    dplyr::rename(
      .sample = .draw,
      .value = .response
    )

  formatted_results <- .data |>
    # generate the row number in the raw data
    dplyr::mutate(row = 1:dplyr::n()) |> # better way?
    dplyr::left_join(results, by = c("row" = ".row")) |>
    dplyr::select(-row)

  formatted_results

}
