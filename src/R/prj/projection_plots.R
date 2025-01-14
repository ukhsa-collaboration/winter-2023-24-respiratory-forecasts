#' @name projection_plots
#' @section Version: 0.0.2
#'
#' @title Projection plots
#'
#' @description Functions for plots in winter pressure forecasting
#'
".__module__."


box::use(
  box / deps_,
  box / redshift,
  prj / plots,
  box / help_,
  box / s3,
  prj / peaks,
  magrittr[`%>%`]
)

.on_load <- function(ns) {
  deps_$need(
    "ggplot2",
    "scales",
    "dplyr",
    "glue",
    "stringr",
    "stats",
    "ggnewscale",
    "sf"
  )
}


#' Create a basic plot for projections that can be used for any disease forecast
#'
#' Includes on plot - title, data source, colors by fit/projection, CI ribbon, real data points by geography
#' @param data Dataframe that need to be in the formatted model output and must include the columns:
#'  - prediction_start_date
#'  - model
#'  - location_level
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
#' @param age_granularity Age breakdown to be plotted. Either "none" or anything
#'    the other valid options as defined by the forecast schema.
#' @param model_name Name of model to be plotted
#' @param plot_elements Different parts of the projection plot that can be plotted
#' @param data_source Name of data source that contains the target that is used in the model
#' @param disease Disease being forecasted
#' @param peaks_data Optional Boolean or data frame of peaks values.
#' If TRUE it will look up the peaks based on disease, and use stored values.
#' @param y_limit numeric value for the maximum y-axis value to show on plots.
#' If you're happy to let the script pick this, leave it as NA.
#' If you have multiple geographies, with different maximum values, you can
#' supply a named list like this: c("nation" = NA, "region" = 150).
#' If names are not supplied they will be filled in in the geography order.
#' If more geographies than y_limit values are given, the remainder will be NA.
#' @param x_limit_upper Date to provide upper limit on plot's x-axis.
#' @param x_limit_lower Date to provide lower limit on plot's x-axis.
#'
#' The data_source and disease can be defined in disease specific scripts  and are used for caption/title
#'
#' @examples
#'
#' # Creating the most basic plot on national geography
#'
#' plot_projection(
#'   data = all_formatted,
#'   target_name = "admissions",
#'   geography = "nation",
#'   ages = "all",
#'   plot_elements = "base",
#'   days_plotted = 60,
#'   data_source = "NHSE UEC",
#'   disease = "COVID-19"
#' )
#'
#' # Creating a plot with lookbacks and RAG scale on a regional scale
#'
#' plot_projection(
#'   data = all_formatted,
#'   target_name = "admissions",
#'   geography = "region",
#'   ages = "all",
#'   plot_elements = c("lookbacks", "rag"),
#'   days_plotted = 60,
#'   data_source = "NHSE UEC",
#'   disease = "COVID-19"
#' )
#'
#' # Creating a plot with lookbacks and RAG scale on a national scale with age breakdown
#'
#' plot_projection(
#'   data = all_formatted,
#'   target_name = "admissions",
#'   geography = "national",
#'   ages = "granular",
#'   plot_elements = c("lookbacks", "rag"),
#'   data_source = "NHSE UEC",
#'   disease = "COVID-19"
#' )
#'
#' @export
plot_projection <- function(
    data,
    target_name,
    geography = "nation",
    age_granularity = "none",
    model_name,
    plot_elements,
    data_source,
    disease,
    peaks_data = peaks$get_peaks(data, disease, target_name),
    y_limit = NA,
    x_limit_upper = NA,
    x_limit_lower = NA
    ) {
  projection_colors <- c("Model fit" = "#12436D", "Projection" = "#801650")

  if (is.null(x_limit_lower) || is.na(x_limit_lower)) {
    x_limit_lower <- data |> # set a default for a required value
      dplyr::filter(
        prediction_start_date == max(prediction_start_date, na.rm = TRUE)) |>
      dplyr::pull(date) |>
      min() |>
      as.Date()
  }
  # Format all the data
  # need to order the age factors for nicer looking facet
  data <- data |>
    dplyr::mutate(
      age_group =
        stats::reorder(
          age_group,
          as.integer(stringr::str_extract(
            age_group,
            "\\d+"
          ))
        ),
      # Shortens names for ICB
      location = stringr::str_remove(location, "integrated care board"),
      location = stringr::str_remove(location, "nhs")
    )

  real_data <- data %>%
    dplyr::filter(
      location_level == geography & age_group_granularity == age_granularity,
      date > x_limit_lower
    ) %>%
    dplyr::distinct(date, location, age_group, target_value)


  current <- data %>%
    dplyr::filter(
      prediction_start_date == max(prediction_start_date, na.rm = TRUE) &
        model == model_name &
        location_level == geography & age_group_granularity == age_granularity
    ) %>%
    dplyr::mutate(is_projection = ifelse(date >= prediction_start_date,
      "Projection", "Model fit"))

  current_projection_text <- paste0("Current projection \nstarting ",
    max(current$prediction_start_date), ":")

  if (grepl("region", geography, ignore.case = TRUE)) {
    real_data <- peaks$peak_cleaning(
      real_data, # for standardising region names
      region_col = "location",
      maintain_colnames = TRUE
    )
    current <- peaks$peak_cleaning(
      current,
      region_col = "location",
      maintain_colnames = TRUE
    )
  }

  # Format the data for RAG plot
  if ("rag" %in% plot_elements) {
    current <- current %>%
      dplyr::group_by(location) %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::select(date, prediction_start_date, location,
        p_increase, p_decrease, p_stable) %>%
      tidyr::pivot_longer(cols = c("p_increase", "p_decrease", "p_stable"),
        names_to = "is_projection",
        values_to = "probability") %>%
      dplyr::group_by(date, prediction_start_date, location) %>%
      dplyr::filter(probability == max(probability)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-probability, -date) %>%
      dplyr::right_join(current %>% dplyr::select(-is_projection),
        by = c("location", "prediction_start_date")) %>%
      dplyr::mutate(is_projection = ifelse(date < prediction_start_date,
        "Model fit", is_projection)) %>%
      dplyr::mutate(dplyr::across("is_projection", stringr::str_replace,
        "p_", "Projection - "))

    projection_colors <- c("Projection - increase" = "#CA0020",
      "Projection - stable" = "#F4A582",
      "Projection - decrease" = "#0571B0",
      "Model fit" = "#12436D")
  }


  # Base plot
  plot <- ggplot2::ggplot(current, ggplot2::aes(x = date)) +
    plots$theme_ham() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1,
      label.position = "right")) +
    ggplot2::scale_x_date(labels = scales::label_date_short(),
      expand = ggplot2::expansion(mult = 0.02)) +
    ggplot2::scale_y_continuous(
      labels = scales::label_comma(accuracy = 1),
      expand = ggplot2::expansion(c(0.05, 0.1))
    ) +
    ggplot2::labs(
      x = "Date",
      y = glue::glue("Daily {gsub('_', ' ', target_name)}"),
      title = glue::glue("{disease} {gsub('_', ' ', target_name)} {max(current$forecast_horizon)}-day projection"),
      caption = glue::glue(
        "Data source: {data_source} from {format(min(real_data$date, na.rm = TRUE), '%d %B %Y')} to {format(max(real_data$date[!is.na(real_data$target_value)], na.rm = TRUE), '%d %B %Y')}",
        "Produced by Infectious Disease Modelling team - AHI - UKHSA",
        .sep = "\n\n"
      )
    ) +
    ggplot2::theme(legend.position = "bottom", legend.box = "horizontal")

  # Age specifics
  if (age_granularity != "none") {
    plot <- plot +
      ggplot2::facet_wrap(~age_group, nrow = 2, scales = "fixed") +
      ggplot2::labs(title = glue::glue("Age stratified {disease} {gsub('_', ' ', target_name)} {max(current$forecast_horizon)} day projection")) +
      ggplot2::theme( # legend.position = c(1, 0.5),
        legend.box = "vertical",
        legend.direction = "horizontal",
        strip.background = ggplot2::element_rect(fill = "#12436D")
      )
  }
  # Peaks plotting
  if (!is.null(peaks_data)) {
    peaks_data <- peaks$filter_peaks(
      peaks_data,
      area = dplyr::case_when(
        geography == "region" ~ "region",
        geography == "NHS region" ~ "NHS region",
        # TODO: If we ever start using Non-NHS English regions we'll have to
        #       rewrite all the geography stuff in the script to specify which.
        # geography == "trust" ~ 'trust code',
        # TODO: please check what the trust level would be for this;
        #       doesn't seem to have been written here yet
        #       Need ICB capability?
        TRUE ~ "nation"
      ),
      metric = target_name, # should be easy enough
      timespan = NULL # TODO: May need to get more specific; 'winter 2022/23'
    ) %>%
      dplyr::rename(location = geo_area) # makes facet_wrap ready

    plot <- plot +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = value, group = interaction(name, location)),
        data = peaks_data,
        linetype = "dashed",
        colour = "grey50",
        size = 0.3
      ) +
      ggplot2::geom_text(
        ggplot2::aes(x = date, y = value, label = name),
        data = dplyr::mutate(peaks_data, "date" = max(data$date)),
        vjust = -0.2,
        hjust = 1,
        colour = "grey50",
        size = 3 # , seems OK without the nudge for now
        # nudge_x = ifelse(is.na(x_limit_upper) | is.na(x_limit_lower),
        #                  60, as.numeric(x_limit_upper - x_limit_lower)) / 4
      )
  }

  # Regional specifics

  if (geography == "region") {
    plot <- plot +
      ggplot2::facet_wrap(~location, ncol = 3, scales = "fixed") +
      ggplot2::labs(title = glue::glue("Regional {disease} {gsub('_', ' ', target_name)} {max(current$forecast_horizon)} day projection")) +
      ggplot2::theme(
        legend.position = c(0.65, 0.125),
        legend.box = "vertical",
        legend.direction = "horizontal",
        strip.background = ggplot2::element_rect(fill = "#12436D")
      )
  }

  # ICB specifics

  if (geography == "icb") {
    plot <- plot +
      ggplot2::facet_wrap(~location, ncol = 3, scales = "fixed", labeller = ggplot2::label_wrap_gen(30)) +
      ggplot2::labs(title = glue::glue("{max(current$nhser23nm)} ICB {disease} {gsub('_', ' ', target_name)} {max(current$forecast_horizon)} day projection")) +
      ggplot2::theme(
        legend.box = "horizontal",
        legend.direction = "horizontal",
        strip.background = ggplot2::element_rect(fill = "#12436D"),
        strip.text.x = ggplot2::element_text(size = 8)
      )
  }


  # Adds past projections to the plot
  if ("lookbacks" %in% plot_elements) {
    if (length(unique(stats::na.omit(data$prediction_start_date))) == 1) {
      stop("You are trying to plot lookbacks with only one lookback in the data")
    }

    lookback_data <- data %>%
      dplyr::filter(model == model_name &
        location_level == geography & age_group_granularity == age_granularity) %>%
      dplyr::mutate(
        lookback = as.integer(max(prediction_start_date, na.rm = TRUE) - prediction_start_date),
        is_projection = ifelse(date >= prediction_start_date, TRUE, FALSE)
      ) %>%
      dplyr::filter(
        is_projection,
        lookback > 0
      )

    if (grepl("region", geography, ignore.case = TRUE)) {
      lookback_data <- peaks$peak_cleaning(
        lookback_data, # standardising region names
        region_col = "location",
        maintain_colnames = TRUE
      )
    }

    max_lookback <- max(lookback_data$lookback) / 7
    lookback_data$lookback <- lookback_data$lookback %>%
      factor(levels = seq(max_lookback, 1) * 7)

    lookback_colors <- scales::seq_gradient_pal("#28A197", "#CFF9F5", "Lab")(seq(0, 1, length.out = max_lookback)) %>%
      rev()


    plot <- plot +
      ggplot2::geom_ribbon(data = lookback_data, ggplot2::aes(ymin = pi_5, ymax = pi_95, fill = as.factor(lookback)), alpha = 0.4) +
      ggplot2::scale_fill_manual(values = lookback_colors) +
      ggplot2::guides(fill = ggplot2::guide_legend(order = 2, nrow = 1, label.position = "bottom")) +
      ggplot2::labs(fill = "Past projections (days prior):")
  }


  # Format model name and add as a subtitle to the plot
  if (grepl("ensemble", model_name)) {
    model_name <- "ensemble"
  }

  # model fit, projection and real data added to the plot
  plot <- plot +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = pi_5, ymax = pi_95, fill = is_projection, alpha = "90% interval")) +
    ggplot2::geom_line(ggplot2::aes(y = pi_50, color = is_projection), linewidth = 0.75, lineend = "round") +
    ggplot2::geom_point(data = real_data, ggplot2::aes(x = date, y = target_value), size = 0.75, alpha = 0.75) +
    ggplot2::scale_alpha_manual(breaks = c("95% interval"), values = c(0.4), guide = NULL) +
    ggplot2::scale_fill_manual(values = ggplot2::alpha(projection_colors, 0.4)) +
    ggplot2::scale_color_manual(values = projection_colors) +
    ggplot2::labs(
      color = current_projection_text,
      fill = current_projection_text,
      subtitle = glue::glue("{stringr::str_to_title(gsub('_', ' ', model_name))} model")
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = 2, label.position = "right"),
      color = ggplot2::guide_legend(nrow = 2, label.position = "right"),
      alpha = ggplot2::guide_legend(nrow = 2, label.position = "right")
    )



  # Add another CI to the plot
  if ("multiple_cis" %in% plot_elements) {
    plot <- plot +
      ggplot2::geom_point(data = real_data, ggplot2::aes(x = date, y = target_value), size = 0.75, alpha = 0.75) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = pi_25, ymax = pi_75, fill = is_projection, alpha = "50% interval")) +
      ggplot2::geom_point(data = real_data, ggplot2::aes(x = date, y = target_value), size = 0.75, alpha = 0.75) +
      ggplot2::scale_alpha_manual(breaks = c("90% interval", "50% interval"), values = c(0.4, 0.5)) +
      ggplot2::labs(alpha = "Prediction Interval:")
  }

  # set axis limits
  plot <- plot +
    ggplot2::coord_cartesian(
      xlim = c(x_limit_lower, x_limit_upper),
      ylim = c(0, y_limit)
    )

  plot
}

#' Create RAG probability column plot for forecasting projections.
#'
#' Includes on plot - title, data source, colors by projection.
#' @param data Dataframe that need to be in the formatted model output and must include the columns:
#'  - prediciton_start_date
#'  - model
#'  - location_level
#'  - date
#'  - target_value
#'  - pi_5
#'  - pi_90
#'  - pi_50
#'  - p_increase
#'  - p_decrease
#'  - p_stable
#'
#'  Other columns may be required when adding other CIs
#'
#' @param target_name Metric that is being forecasted eg admissions
#' @param geography  Location level to be plotted
#' @param model_name Name of model to be plotted
#' @param data_source Name of data source that contains the target that is used in the model
#' @param disease Disease being forecasted
#'
#' The data_source and disease can be defined in disease specific scripts  and are used for caption/title
#'
#' @examples
#'
#' # Creating the plot on national geography
#'
#' plot_projection(
#'   data = all_formatted,
#'   target_name = "admissions",
#'   geography = "nation",
#'   data_source = "NHSE UEC",
#'   disease = "COVID-19"
#' )
#'
#' @export
plot_probability <- function(data,
                             target_name,
                             geography = "nation",
                             model_name,
                             data_source,
                             disease) {
  plot_data <- data %>%
    dplyr::filter(prediction_start_date == max(prediction_start_date, na.rm = TRUE) &
      model == model_name &
      location_level == geography) %>%
    dplyr::group_by(location) %>%
    dplyr::filter(date == max(date, na.rm = TRUE)) %>%
    dplyr::select(date, prediction_start_date, location, p_increase, p_decrease, p_stable) %>%
    tidyr::pivot_longer(cols = c("p_increase", "p_decrease", "p_stable"), names_to = "is_projection", values_to = "probability") %>%
    dplyr::mutate(dplyr::across("is_projection", stringr::str_replace, "p_", "")) %>%
    dplyr::mutate(is_projection = factor(is_projection, levels = c("increase", "stable", "decrease")))



  probability_colors <- c("decrease" = "#0571B0", "stable" = "#F4A582", "increase" = "#CA0020")

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = date)) +
    plots$theme_ham() +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, label.position = "right")) +
    ggplot2::scale_x_date(labels = NULL) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::theme(legend.position = "bottom", legend.box = "horizontal") +
    ggplot2::geom_col(ggplot2::aes(y = probability, x = date, fill = is_projection), position = "stack") +
    ggplot2::scale_fill_manual(values = probability_colors) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(0.5, 3.5, 0.5, 0.5, "cm")
    ) +
    ggplot2::labs(
      y = "Probability",
      x = NULL,
      fill = "Projection direction",
      title = "Probability of trend direction for the 14-day projection",
      caption = glue::glue(
        "Produced by Infectious Disease Modelling team - AHI - UKHSA",
        .sep = "\n\n"
      )
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::coord_flip()

  plot
}

#' Create RAG maps (Region and ICB level) for forecasts
#'
#' Includes on plot - title, data source, colors by projection.
#' @param data Dataframe that need to be in the formatted model output and must include the columns:
#'  - prediciton_start_date
#'  - model
#'  - location_level
#'  - location
#'  - date
#'  - target_value
#'  - pi_5
#'  - pi_90
#'  - pi_50
#'  - p_increase
#'  - p_decrease
#'  - p_stable
#'
#'  Other columns may be required when adding other CIs
#'
#' @param target_name Metric that is being forecasted eg admissions
#' @param geography  Location level to be plotted
#' @param model_name Name of model to be plotted
#' @param data_source Name of data source that contains the target that is used in the model
#' @param disease Disease being forecasted
#'
#' The data_source and disease can be defined in disease specific scripts and are used for caption/title
#'
#' @examples
#'
#' # Creating the map on regional geography
#'
#' plot_projection(
#'   data = all_formatted,
#'   target_name = "admissions",
#'   geography = "region",
#'   data_source = "NHSE UEC",
#'   disease = "COVID-19"
#' )
#'
#' @export
plot_maps <- function(data,
                      target_name,
                      geography = geography,
                      model_name,
                      days_plotted = 60,
                      data_source,
                      disease) {
  real_data <- data %>%
    dplyr::filter(
      location_level == geography,
      date > as.Date(max(date)) - days_plotted
    ) %>%
    dplyr::distinct(date, location, target_value)

  probability_colors <- c("increase" = "#CA0020", "stable" = "#F4A582", "decrease" = "#0571B0")
  probability_alphas <- c("0-25%" = 0.25, "26-50%" = 0.5, "51-75%" = 0.75, "75+%" = 1)
  # If you change these names you'll need to change the values below to match

  rs <- redshift$data_model(c("census21", "geo"))

  region_data_model <- rs$geo.nhser22_en_buc |>
    dplyr::select(geometry, nhser22nm)

  region_lookup <- dplyr::collect(region_data_model) %>%
    dplyr::rename(nhs_region = nhser22nm) %>%
    dplyr::select(nhs_region, geometry)

  region_lookup$nhs_region <- tolower(region_lookup$nhs_region)
  data$location <- tolower(data$location)

  if (geography == "region") {
    map_data <- data %>%
      dplyr::filter(prediction_start_date == max(prediction_start_date, na.rm = TRUE) &
        model == model_name &
        location_level == geography) %>%
      dplyr::mutate(is_projection = ifelse(date >= prediction_start_date, "Projection", "Model fit")) %>%
      dplyr::filter(is_projection == "Projection") %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::select(date, prediction_start_date, location, p_increase, p_decrease, p_stable) %>%
      tidyr::pivot_longer(cols = c("p_increase", "p_decrease", "p_stable"), names_to = "is_projection", values_to = "probability") %>%
      dplyr::group_by(date, prediction_start_date, location) %>%
      dplyr::filter(probability == max(probability)) %>%
      dplyr::mutate(dplyr::across("is_projection", stringr::str_replace, "p_", "")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across("location", stringr::str_replace, " commissioning region", "")) %>%
      dplyr::left_join(region_lookup, by = c("location" = "nhs_region")) %>%
      dplyr::mutate(ranges = cut(probability, c(0, 0.25, 0.5, 0.75, 1),
        labels = names(probability_alphas) # ensures they match
      )) %>%
      sf::st_as_sf(crs = sf::st_crs("epsg:4326"))

    map <-
      ggplot2::ggplot(map_data) +
      ggplot2::geom_sf(ggplot2::aes(geometry = geometry, fill = is_projection, alpha = ranges), lwd = 0.3) +
      ggplot2::scale_alpha_manual(
        name = "Probability",
        values = probability_alphas,
        # labels = names(probability_alphas),
        drop = FALSE) + # apparently we do need all levels every time
      ggplot2::scale_fill_manual(
        name = "Direction of trend",
        values = probability_colors,
        drop = FALSE # if you want keep all 3, make them factor levels first
        # labels = names(ICB_data$is_projection)
      ) +
      ggplot2::guides(alpha = ggplot2::guide_legend(override.aes = list(fill = "gray44"))) +
      plots$theme_ham() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank()
      ) +
      ggplot2::labs(
        title = "Trend projected in 14-days",
        subtitle = glue::glue(" Data up to {format(max(data$date), '%d %b %Y')}"),
        caption = glue::glue(
          "Data source: {data_source} from {format(min(real_data$date, na.rm = TRUE), '%d %B %Y')} to {format(max(real_data$date[!is.na(real_data$target_value)], na.rm = TRUE), '%d %B %Y')}",
          "Produced by Infectious Disease Modelling team - AHI - UKHSA",
          .sep = "\n\n"
        )
      )
  }

  if (geography == "icb") {
    icb_data_model <- rs$geo.icb23_en_bsc |>
      dplyr::select(geometry, icb23nm, icb23cd)

    icb_lookup <- dplyr::collect(icb_data_model) %>%
      dplyr::rename(icb_name = icb23nm) %>%
      dplyr::select(icb_name, geometry, icb23cd)

    data$location <- tolower(data$location)
    icb_lookup$icb_name <- tolower(icb_lookup$icb_name)

    data <- data %>%
      dplyr::mutate(dplyr::across("location", stringr::str_replace, " integrated care board", ""))

    icb_lookup <- icb_lookup %>%
      dplyr::mutate(dplyr::across("icb_name", stringr::str_replace, " integrated care board", ""))

    map_data <- data %>%
      dplyr::filter(prediction_start_date == max(prediction_start_date, na.rm = TRUE) &
        model == model_name &
        location_level == geography) %>%
      dplyr::mutate(is_projection = ifelse(date >= prediction_start_date, "Projection", "Model fit")) %>%
      dplyr::filter(is_projection == "Projection") %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::select(date, prediction_start_date, location, p_increase, p_decrease, p_stable) %>%
      tidyr::pivot_longer(cols = c("p_increase", "p_decrease", "p_stable"), names_to = "is_projection", values_to = "probability") %>%
      dplyr::group_by(date, prediction_start_date, location) %>%
      dplyr::filter(probability == max(probability)) %>%
      dplyr::mutate(dplyr::across("is_projection", stringr::str_replace, "p_", "")) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(icb_lookup, by = c("location" = "icb_name")) %>%
      dplyr::mutate(ranges = cut(probability, c(0, 0.25, 0.5, 0.75, 1),
        labels = names(probability_alphas)
      )) %>%
      sf::st_as_sf(crs = sf::st_crs("epsg:4326"))

    map <-
      ggplot2::ggplot(map_data) +
      ggplot2::geom_sf(ggplot2::aes(geometry = geometry, fill = is_projection, alpha = ranges), lwd = 0.3) +
      ggplot2::scale_alpha_discrete(name = "Probability") +
      ggplot2::geom_sf(data = region_lookup, ggplot2::aes(geometry = geometry), lwd = 0.8, colour = "black", alpha = 0) +
      ggplot2::scale_alpha_manual(
        name = "Probability",
        values = probability_alphas,
        # labels = names(probability_alphas),
        drop = FALSE) +
      ggplot2::scale_fill_manual(
        name = "Direction of trend",
        values = probability_colors,
        drop = FALSE
        # labels = names(ICB_data$is_projection)
      ) +
      ggplot2::guides(alpha = ggplot2::guide_legend(override.aes = list(fill = "gray44"))) +
      plots$theme_ham() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank()
      ) +
      ggplot2::labs(
        title = "Trend projected in 14-days",
        subtitle = glue::glue(" Projection up to {format(max(data$date), '%d %b %Y')}"),
        caption = glue::glue(
          "Data source: {data_source} from {format(min(real_data$date, na.rm = TRUE), '%d %B %Y')} to {format(max(real_data$date[!is.na(real_data$target_value)], na.rm = TRUE), '%d %B %Y')}",
          "Produced by Infectious Disease Modelling team - AHI - UKHSA",
          .sep = "\n\n"
        )
      )
  }

  map
}



