#' @name splines
#' @section Version: 0.0.1
#'
#' @title
#' Support for calculating k for splines.
#'
#' @description
#' Helper function to get the number of knots given the interval and total number of days
#' In the vain of: "I want a knot every .x days".
#'
#' @seealso
#' * [splines$every_k()]
#'
".__module__."

box::use(box / deps_,
  box / help_,
  box / ops[...]
)

.on_load <- function(ns) {
  # deps_$need()
}

#' Calculate number of knots for a spline.
#'
#' Helper function for finding the number of knots to pass into a spline,
#' based on wanting a knot every .x days, out of the training length's timespan.
#'
#' @param .x Integer value for the desired frequency of knots: e.g., "I want a knot every .x days"
#' @param training_length Integer value for the total number of days within the data being fit
#'
#' @returns Integer value for k: the number of knots to pass to the spline fit.
#'
#' @examples
#' k <- splines$every_k(.x = 30, training_length = 364) # aproximately one knot per month
#' form <- as.formula("count ~ s(t, bs='tp', k = splines$every_k(.x = 11, training_length = 60))")
#'
#' @export
every_k <- function(.x, training_length) {
  round(training_length / .x)
}
