
box::use(
  box / deps_,
  box / help_,
  prj / peaks[`filter_peaks`],
  magrittr[`%>%`]
)


.on_load <- function(ns) {
  deps_$need(
    "colorspace",
    "ggplot2",
    "ggthemes",
    "scales"
  )
}



#' Plotting theme for Health Analysis Modelling team
#'
#' @inheritParams ggthemes::theme_fivethirtyeight
#'
#' @export

theme_ham <- function(base_size = 14, base_family = "sans") {
  ggplot2::theme_bw(base_size, base_family) +
    ggplot2::theme(
      panel.spacing = ggplot2::unit(1.5, "lines"),
      panel.border = ggplot2::element_rect(color = "grey50", fill = NA, size = 1, linetype = 1),
      plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
      panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
      panel.grid = ggplot2::element_line(colour = "#D9D9D9"),
      strip.text = ggplot2::element_text(colour = "white", face = "bold"),
      axis.title = ggplot2::element_text(colour = "grey50", face = "bold"),
      strip.background = ggplot2::element_rect(colour = "grey50", fill = "grey50"),
      legend.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(linetype = 3, size = 0.2),
      plot.caption = ggplot2::element_text(size = 12, lineheight = 0.5, color="grey50", hjust=0),
      plot.caption.position = "plot",
      plot.title = ggtext::element_textbox(width = ggplot2::unit(1, "npc"), size = 16, padding = ggplot2::margin(t = 10)),
      plot.title.position = "plot"
    )
}


#' Produce a "projections plot"
#'
#' A typical output of the modelling process.
#'
#' @param current A data frame with columns `date`, `is_projection`, `fit`,
#'   `lower` `upper`, `admissions`/`occupancy` (or some other column specified
#'   by `target`), and optionally `region`. Typically this will be the result of
#'   joining the zero-lag model fit with the original admissions data.
#' @param previous An optional list of data frames of previous projections, each
#'   with columns `date`, `upper`, `lower`, and optionally `region`. Typically
#'   this is the (possibly manipulated) list of model fits for all non-zero
#'   lags. If `NULL`, instead the `current$rag` column (containing values in
#'   `c("r", "a", "g")`) will be used to determine the main line/ribbon colour.
#' @param peaks An optional set of peaks for admissions, possibly produced by
#'   calling [get_peaks()] with `value_vars = admissions`. If `NULL`, no
#'   peak marker lines will be plotted.
#' @param target A string specifying which variable should be plotted on the
#'   y-axis, if this can't be inferred from `current`.
#'
#' @export

# plot_projections <- function(current, previous = NULL, peaks = NULL, target = c("admissions", "arrival_admissions", "occupancy", "target")) {
#
#   target <- match.arg(target, several.ok = TRUE)
#
#   target <- intersect(target, colnames(current))
#
#   if (length(target) > 1)
#     stop("Dependent variable is ambiguous. Please use `target` argument to specify which should be used.")
#
#
#   is_regional <- "region" %in% colnames(current)
#
#
#   p <- ggplot2::ggplot(current, ggplot2::aes(x = date))
#
#
#   if (!is.null(peaks))
#     peaks = filter_peaks(peaks, area = case_when(
#       is_regional ~ 'region',
#       # TODO: It would be so much better to make this an explicit thing you have to put into the function
#       'trust_code' %in% colnames(current) ~ 'trust code',
#       TRUE ~ 'nation'),
#       metric = target, # should be easy enough
#       timespan = NULL) # TODO: This will have to be more dynamic; 'winter 2022/23' should do for now
#
#     p <- p +
#       ggplot2::geom_hline(
#         ggplot2::aes(yintercept = value, group = name),
#           data = peaks,
#         linetype = "dashed",
#         colour = "grey50",
#         size = 0.3
#       ) +
#
#       ggplot2::geom_text(
#         ggplot2::aes(x = date, y = value, label = name),
#         data = dplyr::mutate(peaks, "date" = max(current$date)),
#         vjust = -0.2,
#         hjust = 1,
#         colour = "grey50",
#         size = 3,
#         nudge_x = 40
#       )
#
#
#
#
#   if (!is.null(previous))
#     p <- add_projections_previous(p, previous, peaks, target, is_regional)
#   else
#     p <- add_projections_rag(p, peaks, target)
#
#   # na.rm = TRUE because we don't have actual values for future dates (but we do have projections!)
#   p <- p + ggplot2::geom_point(ggplot2::aes(y = .data[[target]]), na.rm = TRUE, size = 0.75, alpha = 0.75)
#
#
#   p <- p +
#     ggplot2::scale_linetype_manual(values = c("solid", "dotted"), guide = NULL) +
#     ggplot2::scale_x_date(labels = scales::label_date_short()) +
#     ggplot2::scale_y_continuous(
#       label = scales::label_comma(accuracy = 1),
#       expand = ggplot2::expansion(c(0.05, 0.1)),
#       limits = c(0, NA)
#     ) +
#     ggplot2::labs(
#       x = NULL,
#       y = glue::glue("Daily {gsub('_', ' ', target)}"),
#       fill = "Projection made (days prior):",
#       caption = glue::glue(
#         "**Data source:** NHS England COVID-19 Hospital Activity Data, ",
#         "from {format(min(current$date, previous$date, na.rm = TRUE), '%d %B %Y')} ",
#         "to {format(max(current$date[!current$is_projection], na.rm = TRUE), '%d %B %Y')}. ",
#         "Produced by Infectious Disease Modelling team (UK Health Security Agency)."
#     )) +
#     theme_ham()
#
#
#
#
#   if (is_regional)
#     p <- p +
#       ggplot2::facet_wrap(ggplot2::vars(region), ncol = 3, scales = "free_y") +
#       ggplot2::theme(legend.position = c(0.65, 0.125))
#   else
#     p <- p + ggplot2::theme(legend.position = "bottom", legend.margin = ggplot2::margin(0, 0, 0, 0))
#
#   p
# }
#
#
#
#
# add_projections_rag <- function(p, peaks, target) {
#
#   cutoff <- max(p$data[[target]], peaks$value, na.rm = TRUE) * 1.05
#
#   p +
#     ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = pmin(upper, cutoff), fill = rag), alpha = 0.5) +
#     ggplot2::geom_line(ggplot2::aes(y = fit), col = "white", size = 1.25, lineend = "round") +
#     ggplot2::geom_line(ggplot2::aes(y = fit, colour = rag, linetype = is_projection), lineend = "round", size = 0.75) +
#     ggplot2::scale_fill_manual(
#       values = c("r" = "#D55E00", "a" = "#E69F00", "g" = "#009E73"),
#       aesthetics = c("colour", "fill"),
#       guide = NULL
#     )
# }
#
#
#
#
# add_projections_previous <- function(p, previous, peaks, target, is_regional) {
#
#   p$data$offset <- "0"
#   previous$offset <- sub("past_0?", "", previous$offset)
#
#   cb <- projections_palette(length(unique(previous$offset)) + 1) %>%
#     purrr::set_names(c(0, sort(as.integer(unique(previous$offset)))))
#
#   cutoff <- max(p$data[[target]], peaks$value, na.rm = TRUE) * 1.05
#
#
#   p <- p +
#     ggplot2::geom_ribbon(
#       ggplot2::aes(ymin = lower, ymax = pmin(upper, cutoff), fill = offset),
#       data = previous,
#       alpha = 0.5
#     ) +
#     ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = pmin(upper, cutoff), fill = offset), alpha = 0.5) +
#     ggplot2::geom_line(ggplot2::aes(y = fit), colour = "white", size = 1.25, lineend = "round") +
#     ggplot2::geom_line(ggplot2::aes(y = fit, linetype = is_projection, colour = offset), size = 0.75, lineend = "round") +
#     ggplot2::scale_fill_manual(values = cb, aesthetics = c("fill", "colour"), guide = NULL, breaks = c("49", "42", "35", "28", "21", "14", "7", "0"))
#
#
#   if (is_regional)
#     p <- p + ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, reverse = TRUE, label.position = "bottom", title.position = "top"))
#   else
#     p <- p + ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, reverse = TRUE, label.position = "bottom"))
#
#   p
# }
#
#
#
#
# projections_palette <- function(n) {
#   scales::viridis_pal(end = 0.9)(n) %>%
#     colorspace::lighten(0.5) %>%
#     colorspace::desaturate(0.2)
# }
#
