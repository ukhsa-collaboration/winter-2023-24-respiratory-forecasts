#' @name score
#' @section Version: 0.0.1
#'
#' @title Calculating and visualising scoring metrics
#'
#' @description
#' Take the model data and calculate the median absolute error,
#' bias (overprediction/underprediction), interval (50% and 90%) coverage,
#' a weighted interval score, and coverage deviation
#' nationally and regionally for each model.
#'
#' Metrics:
#'
#' - median absolute error: the median of the absolute distance between the
#'   predictive distribution and the observed distribution
#' - bias: measure of relative tendency to under- or over-predict, bounded
#'   between -1 and 1.
#' - interval coverage: the proportion of observed values that fall in a given
#'   prediction interval range.
#' - weighted interval score: a weighted sum of interval scores, where
#'   smaller scores imply better calibration
#' - relative weighted interval score: a weighted sum of interval scores computed on
#'   log transformed values and targets if add_log = TRUE, where smaller scores
#'   imply better calibration. This can be interpreted as a probabilistic version
#'   of relative error
#' - coverage deviation: the mean of the difference
#'   between the empirical and nominal interval coverage, for all the intervals
#'   used to calculate the weighted interval score
#'
".__module__."

box::use(
  box / deps_,
  box / help_
)

.on_load <- function(ns) {
  deps_$need(
    "dplyr",
    "ggplot2",
    "ggpubr",
    "stringr",
    "tidyr"
  )

  deps_$need(
    "scoringutils>=1.2.0",
    msg = "We need the development version of {scoringutils}, for now!",
    install_cmd = quote(remotes::install_github("epiforecasts/scoringutils"))
  )
}

#' @param data Dataframe that need to be in the formatted model output and must include the columns:
#'  - prediction_start_date
#'  - model
#'  - location_level
#'  - date
#'  - target_value
#'  - pi_X
#'
#'  Other columns may be required when adding other CIs
#'
#' @param geography  Location level to be investigated
#' @param age_cohort_level What age group granularity data to explore
#' @param model_names Names of models to be investigated
#' @param add_log Whether or not to add relative interval score (of log transformed values and targets) to scoring plots and table.
#' @param output_path Character giving the filename prefix, and optionally the parent folder location on the local system.

#'
#' @returns the required metrics (median absolute error, bias, interval coverage)
#' and visualisations of these metrics (heatmap plots highlighting these values)
#'
#' @export

score <- function(data, geography, age_cohort_level, model_names, add_log = FALSE, output_path) {

  predictions_of_interest <- data |>
    dplyr::filter(model %in% model_names) |>
    dplyr::filter(location_level == geography) |>
    dplyr::filter(age_group_granularity == age_cohort_level) |>
    # select only the predictions (in the future)
    dplyr::filter(date >= prediction_start_date) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("pi_"), names_to = "quantile", values_to = "prediction") |>
    # keep only PIs we care about
    dplyr::filter(quantile %in% c("pi_50", "pi_25", "pi_75", "pi_95", "pi_5")) |>
    dplyr::mutate(quantile = as.numeric(stringr::str_remove(quantile, "pi_")) / 100)


  if (geography == "nation" & age_cohort_level == "none") {
    predictions_of_interest_score <- predictions_of_interest |>
      dplyr::rename(true_value = target_value) |>
      scoringutils::transform_forecasts(fun = scoringutils::log_shift, offset = 1) |>
      scoringutils::score() |>
      scoringutils::add_coverage(by = c("model", "prediction_start_date", "scale"), ranges = c(50, 90)) |>
      scoringutils::summarise_scores(
        by = c("model", "prediction_start_date", "scale"),
        na.rm = TRUE
      ) |>
      scoringutils::summarise_scores(fun = round, digits = 3)

    predictions_of_interest_horizon_score <- predictions_of_interest |>
      dplyr::rename(true_value = target_value) |>
      scoringutils::transform_forecasts(fun = scoringutils::log_shift, offset = 1) |>
      scoringutils::score() |>
      scoringutils::add_coverage(by = c("model", "scale"), ranges = c(50, 90)) |>
      scoringutils::summarise_scores(by = c("model", "scale"), na.rm = TRUE) |>
      scoringutils::summarise_scores(fun = round, digits = 3) |>
      dplyr::mutate(horizon = "Overall") |>
      dplyr::select(model, scale, horizon, interval_score, underprediction, overprediction, ae_median, coverage_deviation)

    # Plot the 50% and 90% coverage, with dotted horizontal lines indicating the
    # optimal values of these scores, i.e. 0.5 and 0.9. For example, a 50%
    # coverage score of 0.5 indicates 50% of all observed values were actually
    # covered by all 50% prediction intervals. 50% coverage is plotted with a
    # solid line; 90% coverage is plotted with a dashed line.
    coverage <- predictions_of_interest_score |>
      dplyr::filter(scale == "natural") |>
      dplyr::rename(deviation = coverage_deviation, "50%" = coverage_50, "90%" = coverage_90) |>
      tidyr::pivot_longer(cols = ends_with("%"), names_to = "coverage_perc", values_to = "coverage_value") |>
      ggplot2::ggplot(ggplot2::aes(x = prediction_start_date)) +
      ggplot2::geom_line(ggplot2::aes(y = coverage_value, color = model, linetype = coverage_perc)) +
      ggplot2::geom_point(ggplot2::aes(y = coverage_value, color = model)) +
      ggplot2::geom_hline(yintercept = 0.5, color = "darkgrey", alpha = 0.7, linetype = "dotted") +
      ggplot2::geom_hline(yintercept = 0.9, color = "darkgrey", alpha = 0.7, linetype = "dotted") +
      ggplot2::scale_color_brewer(palette = "Set1") +
      ggplot2::theme_bw() +
      ggplot2::scale_x_date(breaks = "1 week", date_labels = "%d-%b") +
      ggplot2::labs(
        x = "Prediction start date",
        y = "Interval coverage",
        color = "Model",
        linetype = "Coverage"
      )

    # Plot the bias, with a dotted horizontal line indicating the optimal score,
    # i.e. 0.
    bias <- predictions_of_interest_score |>
      dplyr::filter(scale == "natural") |>
      ggplot2::ggplot(ggplot2::aes(x = prediction_start_date)) +
      ggplot2::geom_line(ggplot2::aes(y = bias, color = model)) +
      ggplot2::geom_point(ggplot2::aes(y = bias, color = model)) +
      ggplot2::ylim(-1, 1) +
      ggplot2::geom_hline(yintercept = 0, color = "darkgrey", alpha = 0.7, linetype = "dotted") +
      ggplot2::scale_color_brewer(palette = "Set1") +
      ggplot2::theme_bw() +
      ggplot2::scale_x_date(breaks = "1 week", date_labels = "%d-%b") +
      ggplot2::labs(
        x = "Prediction start date",
        y = "Bias",
        color = "Model"
      )

    # Plot the weighted interval score: the closer this is to 0, the better the
    # calibration of the model.
    is <- predictions_of_interest_score |>
      dplyr::filter(scale == "natural") |>
      ggplot2::ggplot(ggplot2::aes(x = prediction_start_date)) +
      ggplot2::geom_line(ggplot2::aes(y = interval_score, color = model)) +
      ggplot2::geom_point(ggplot2::aes(y = interval_score, color = model)) +
      ggplot2::scale_color_brewer(palette = "Set1") +
      ggplot2::theme_bw() +
      ggplot2::scale_x_date(breaks = "1 week", date_labels = "%d-%b") +
      ggplot2::labs(
        x = "Prediction start date",
        y = "Interval Score",
        color = "Model"
      )

    # Plot the relative weighted interval score: the closer this is to 0, the better the
    # calibration of the model. This is the weighted interval score of log transformed targets and predictions
    # and can be interpreted as a probabalistic version of relative (as opposed to absolute) error
    # It also can be interpreted as scoring how well the model predicts growth rates

    ris <- predictions_of_interest_score |>
      dplyr::filter(scale == "log") |>
      ggplot2::ggplot(ggplot2::aes(x = prediction_start_date)) +
      ggplot2::geom_line(ggplot2::aes(y = interval_score, color = model)) +
      ggplot2::geom_point(ggplot2::aes(y = interval_score, color = model)) +
      # ggplot2::scale_color_brewer(palette = "Set1") +
      ggplot2::theme_bw() +
      ggplot2::scale_x_date(breaks = "1 week", date_labels = "%d-%b") +
      ggplot2::labs(
        x = "Prediction start date",
        y = "Relative Interval Score",
        color = "Model"
      )

    # Plot the median absolute error: the closer this is to 0, the better the
    # calibration of the model.
    median_ae <- predictions_of_interest_score |>
      dplyr::filter(scale == "natural") |>
      ggplot2::ggplot(ggplot2::aes(x = prediction_start_date)) +
      ggplot2::geom_line(ggplot2::aes(y = ae_median, color = model)) +
      ggplot2::geom_point(ggplot2::aes(y = ae_median, color = model)) +
      ggplot2::scale_color_brewer(palette = "Set1") +
      ggplot2::theme_bw() +
      ggplot2::scale_x_date(breaks = "1 week", date_labels = "%d-%b") +
      ggplot2::labs(
        x = "Prediction start date",
        y = "Median Absolute Error",
        color = "Model"
      )

    # Include ris plot or not depending on add_log argument
    if (add_log) {
      ggpubr::ggarrange(coverage, bias, is, ris, median_ae, ncol = 1, nrow = 5, common.legend = TRUE, legend = "bottom") |>
        ggplot2::ggsave(filename = paste0(output_path, "/", geography, "_scoring_plots.png"), width = 16, height = 12)
    } else {
      ggpubr::ggarrange(coverage, bias, is, median_ae, ncol = 1, nrow = 4, common.legend = TRUE, legend = "bottom") |>
        ggplot2::ggsave(filename = paste0(output_path, "/", geography, "_scoring_plots.png"), width = 16, height = 12)
    }

  } else {
    # doing an `else` here to keep it generic, as we may have a range of different ages and regions and other
    # breakdowns
    predictions_of_interest_horizon_score <- predictions_of_interest |>
      dplyr::mutate(true_value = target_value) |>
      scoringutils::transform_forecasts(fun = scoringutils::log_shift, offset = 1) |>
      scoringutils::score() |>
      scoringutils::add_coverage(by = c("model", "scale"), ranges = c(50, 90)) |>
      scoringutils::summarise_scores(by = c("model", "scale"), na.rm = TRUE) |>
      scoringutils::summarise_scores(fun = round, digits = 3) |>
      dplyr::mutate(horizon = "Overall") |>
      dplyr::select(model, scale, horizon, interval_score, underprediction, overprediction, ae_median, coverage_deviation)

  }

  score_table <- scoringutils::plot_score_table(predictions_of_interest_horizon_score |> dplyr::filter(scale == "natural"), y = "model",
    metrics = c("interval_score", "underprediction", "overprediction", "ae_median", "coverage_deviation")) +
    ggplot2::facet_wrap(~horizon)

  ggplot2::ggsave(filename = paste0(output_path, "/", geography, "_", age_cohort_level, "_score_table.png"),
    plot = score_table, width = 8, height = 6)

  if (add_log) {
    score_table_log <- scoringutils::plot_score_table(predictions_of_interest_horizon_score |> dplyr::filter(scale == "log"), y = "model", metrics = c("interval_score", "overprediction", "underprediction")) +
      ggplot2::facet_wrap(~horizon)

    ggplot2::ggsave(filename = paste0(output_path, "/", geography, "_", age_cohort_level, "_log_score_table.png"),
      plot = score_table_log, width = 8, height = 6)
  }
}
