#' @name outputs
#' @section Version: 0.1.1
#'
#' @title
#' Combined Pack Output Functions.
#'
#' @description
#'
#' @details
#' The main function is projections_plotter(), which works as a wrapper around
#' the projection_plots$plot_projection() function, with user input tidying,
#' looping through geographies and included plots, as well as saving the result.
#'
#' There is also the rag_plotter() which is for showing a model's probabilistic
#' outputs, and is a wrapper around the two projection_plots functions:
#' plot_probability() for national use, and plot_maps() for regional & ICB use.
#' You can force it to use the map on national, and vice versa, but I doubt it'd
#' go well. This function also does some cleaning and tidying as it goes, loops
#' through given geographies, and saves the results.
#'
#' The last function you might use is the los_plotter(), for showing the length
#' of stay for hospital occupancy. This currently doesn't loop over geographies
#' as that's not typically required from the same data.
#'
#' There's also the peaks_handler() function for dealing with some of the more
#' complicated issues that arise from the peaks of certain diseases.
#'
#' @examples
#' # setup box module:
#' box::use(prj / outputs)
#'
#' # National & regional projection plots:
#' outputs$projections_plotter( # pull from box module
#'   plots_include = list(c("lookbacks", "rag")), # the kind of plots you want
#'   data = all_formatted_summary, # the summarised model results
#'   target_name = target_name_sym, # usually admissions or occupancy.
#'   model_name = model_name, # either individual or ensemble model name.
#'   geography = c("nation", "region"), # two supplied, will process both.
#'   output_path = output_path, # full path to dated outputs folder.
#'   y_limit = NA, # can supply different limits with geography-named vector.
#'   disease = config$overall_params$disease, # all config files will have now.
#'   peaks_data = config$overall_params$show_peaks # in config this is T/F
#' ) # peaks data may be supplied manually, or looked up internally if just T.
#'
#' # RAG probabilities
#' outputs$rag_plotter(
#'   data = all_formatted_summary, # same data as above; like other inputs here.
#'   target_name = target_name_sym,
#'   model_name = model_name,
#'   geography = c("nation", "region", "icb"), # will do bar for national only,
#'   output_path = output_path,                # and maps for region & ICBs.
#'   disease = config$overall_params$disease,
#' )
#'
#' # Length of Stay plot
#' outputs$los_plotter(
#'   data = los_regional_summary, # summarised discharge_results with quantiles.
#'   discharge_results = discharge_results, # direct model result
#'   geography = "region", # only one geographic level at a time
#'   output_path = output_path, # timestamped output folder path
#'   disease = config$overall_params$disease # no in all config files here.
#' )
#'
#' @seealso
#' * [outputs$los_plotter()]
#' * [outputs$projections_plotter()]
#' * [outputs$rag_plotter()]
#'
".__module__."

#### Outstanding TODOs ####
# In decreasing importance:
# Break down projection plots by NHS region (as ICB is already done).
# Add ICB level breakdowns to more plots & peaks codes (if modelling expands).
# Consider Trust level outputs (if modelling expands).
# See if making los_projection loop over geography is worthwhile. - prob. not.
# Add the devolved nations to this 'UK'HSA code.
# If trough values are added to Norovirus peaks, use an average of peak & trough
#   for the total beds value.
# Embed RSV population script, or turn into function call somewhere.
# Check RSV population contribution factors again.

#### Setup ####
# Sys.setenv( # sometimes needed if box struggling to find path; fix in Rprofile
#   "R_BOX_PATH" = fs::path(rprojroot::find_root(rprojroot::is_git_root),
#                           "src", "R")) # ensures box always works

box::use(
  box / deps_,
  box / help_,
  box / s3,
  prj / peaks,
  prj / plots,
  prj / projection_plots,
  prj / user_check
  # magrittr[`%>%`] # looks unused
)
.on_load <- function(ns) {
  deps_$need( # ensures needed libraries are available
    "aws.s3",
    "dplyr",
    "ggplot2",
    "lubridate",
    "rlang",
    "tidyr",
    "tidyselect"
  )

  deps_$min_version("dplyr", "1.1.0")
}


#### Exported Functions ####
#' Projections Plotter
#'
#' Operates as a wrapper around the projection_plots$plot_projection() function,
#' plus some tidying up of the user input, looping through geographies and
#' included plots, as well as saving the result.
#'
#' @param plots_include A list of the kinds of plots you want to produce.
#' Will loop over each item, though multiple parts can be added into one item to
#' be plotted together, for example, this will give two kinds of plots:
#'  list("multiple_cis", c("lookbacks", "rag", "map"))
#' @param data A data frame, the formatted summary of the model outputs.
#' @param target_name A symbol string of the kind of model being run;
#' current options are one of: admissions, arrival_admissions, or occupancy.
#' @param model_name String of the type of model being plotted.
#' @param geography String of the geographic level, or levels, to be plotted.
#' You can give a list like this to loop through: c("nation", "region", "icb").
#' @param age_granularity Optional string of the age grouping fidelity to use.
#' Should be the default "none" for all but RSV (also an option for RSV).
#' RSV can also have "fine", "course", anything else will be treated as "full".
#' @param output_path String of the path the the timestamped output folder.
#' @param disease String picking the disease being plotted; always in config.
#' @param data_source Optional string of the data's source.
#'  Defaults based on disease read in from list at the end of this script.
#' @param peaks_data Optional Boolean or data frame of peaks values.
#' If TRUE it will look up the peaks based on disease, and use stored values.
#' Will deal with changes needed for specific diseases, see peaks_handler below.
#' @param y_limit numeric value for the maximum y-axis value to show on plots.
#' If you're happy to let the script pick this, leave it as NA.
#' If you have multiple geographies, with different maximum values, you can
#' supply a named list like this: c("nation" = NA, "region" = 150).
#' If names are not supplied they will be filled in in the geography order.
#' If more geographies than y_limit values are given, the remainder will be NA.
#' @param x_limit_upper Date to provide upper limit on plot's x-axis.
#' @param x_limit_lower Date to provide lower limit on plot's x-axis.
#'
#' @examples
#' # National & regional projection plots:
#' outputs$projections_plotter( # pull from box module
#'   plots_include = list(c("lookbacks", "rag")), # the kind of plot you want
#'   data = all_formatted_summary, # the summarised model results
#'   target_name = target_name_sym, # usually admissions or occupancy.
#'   model_name = model_name, # either individual or ensemble model name.
#'   geography = c("nation", "region"), # two supplied, will make both.
#'   output_path = output_path, # full path to dated outputs folder.
#'   y_limit = NA, # can supply different limits with geography-named vector.
#'   disease = config$overall_params$disease, # all config files have now.
#'   peaks_data = config$overall_params$show_peaks # config has TRUE/FALSE
#' ) # peaks data may be supplied manually, or looked up internally if TRUE.
#'
#' @export

# Create and save covid projection plots
projections_plotter <- function(
    plots_include = list("multiple_cis", c("lookbacks", "rag", "map")),
    data,
    target_name,
    model_name,
    geography,
    age_granularity = "none",
    output_path,
    disease,
    y_limit = NA,
    x_limit_upper = NA,
    x_limit_lower = NA, # as.Date(min(data$prediction_start_date, na.rm = T)) ,
    data_source = "", # list of disease defaults below, but user can put in here
    peaks_data = FALSE
    ) {

  if (is.null(x_limit_upper) || is.na(x_limit_upper)) {
    x_limit_upper <- as.Date(max(data$date, na.rm = T))
  } else {
    x_limit_upper <- as.Date(x_limit_upper)
  }

  if (is.null(x_limit_lower) || is.na(x_limit_lower)) {
    x_limit_lower <- data |>
      dplyr::filter(prediction_start_date == max(prediction_start_date,
        na.rm = TRUE)) |>
      dplyr::pull(date) |>
      min() |>
      as.Date()
  } else if (is.numeric(x_limit_lower)) {
    x_limit_lower <- as.Date(x_limit_upper - lubridate::days(x_limit_lower))
  } else {
    x_limit_lower <- as.Date(x_limit_lower)
  }

  # cleaning user input:
  disease <- user_check$disease_checker(disease)
  if (is.null(data_source) || data_source == "") { # fill in sources if blank:
    data_source <- tidyr::replace_na(data_sources[[disease]],
      "Unspecified source")
  }
  geography <- unique(geography) # remove any duplicates
  if (is.null(names(y_limit))) { # will create a named list if not supplied
    if (length(y_limit) != length(geography)) {
      y_limit <- y_limit[1:length(geography)]
      # This will pad y_limit with NA if geography is longer,
      #  and crop it down if y_limit is shorter
    }
    # # Alternative version where it pads with the final value:
    # if (length(y_limit) < length(geography)) {
    # # pad y_limit with final value if there aren't enough given:
    #   y_limit = c(y_limit, rep(dplyr::last(y_limit),
    #                            length(geography) - length(y_limit)))
    # } else if (length(y_limit) > length(geography)) {
    #   y_limit = y_limit[1:length(geography)]
    # }
    y_limit <- rlang::set_names(as.list(y_limit), geography) # naming the list
  } else if (any(!geography %in% names(y_limit))) {
    y_limit <- c( # Fills in missing geographies from named input with default NA
      y_limit,
      rlang::set_names(as.list(rep(NA, length(geography) - length(y_limit))),
        base::setdiff(geography, names(y_limit)))) |> as.list()
  }

  for (geo in geography) {
    geo <- match.arg(geo, c("nation", "region", "icb")) # "NHS region",
    # TODO Maybe something for trust level? one day, maybe.

    # Get peaks data, if asked for
    if (is.logical(peaks_data) && peaks_data) { # i.e. peaks_data is True:
      peaks_data <- peaks_handler( # this got so complicated I made it a function
        disease,
        target_name,
        geo, # only use one geography please; needed for RSV
        age_granularity # needed for RSV
      )
    } else if (!is.data.frame(peaks_data)) { # leaves supplied data frames alone
      peaks_data <- NULL # else drop it from plot
    }
    for (plot_type in plots_include) {
      plot_type <- match.arg(
        plot_type, # checks plot type is recognised
        c("multiple_cis", "lookbacks", "rag", "base", "map"), # TODO map needed?
        several.ok = TRUE)

      plot_base <- projection_plots$plot_projection(
        data = data,
        target_name = target_name,
        model_name = model_name,
        geography = geo,
        age_granularity = age_granularity,
        plot_elements = plot_type,
        data_source = data_source,
        disease = dplyr::case_when( # reformatting disease name for printing
          disease %in% names(disease_prints) ~ disease_prints[[disease]],
          TRUE ~ stringr::str_to_title(disease)),
        peaks_data = peaks_data,
        y_limit = y_limit[[geo]], # allows for different limits per geography
        x_limit_lower = x_limit_lower,
        x_limit_upper = x_limit_upper
      )

      # print(plot_base)

      # Save output
      ggplot2::ggsave(
        paste0( # why does everyone use glue?
          output_path, "/", # When there's dynamic elements and no new lines,
          geo, "/", # paste works and is easier to tell what parts can change.
          disease, "_",
          ifelse(geo == "icb", # break down individual ICBs; TODO: add regions?
            paste0(gsub(" ", "_", max(data$nhser23nm)), "_"), ""),
          model_name, "_", paste0(plot_type, collapse = "_"),
          "_projection.png"),
        plot_base,
        width = 11,
        height = 10,
        dpi = 500
      )
    }
  }
}

#' RAG plotter
#'
#' Create and save RAG probability plots and maps
#' rag_plotter() which is for showing the model's probabilistic
#' outputs, and is a wrapper around the two projection_plots functions:
#' plot_probability(), for national use, & plot_maps(), for regional & ICB use.
#' You can force it to use the map on national, and vice versa, but I doubt it'd
#' go well. This function also does some cleaning and tidying as it goes, loops
#' through given geographies, and saves the results.
#'
#' @param plots_include A list of the kinds of plots you want to produce.
#' Will loop over each item, though multiple parts can be added into one item to
#' be plotted together, for example, this will give two kinds of plots:
#'  list("multiple_cis", c("lookbacks", "rag", "map"))
#' @param data A data frame, the formatted summary of the model outputs.
#' @param target_name A symbol string of the kind of model being run;
#' current options are one of: admissions, arrival_admissions, or occupancy.
#' @param model_name String of the type of model being plotted.
#' @param geography String of the geographic level, or levels, to be plotted.
#' You can give a list like this to loop through: c("nation", "region", "icb").
#' @param output_path String of the path the the timestamped output folder.
#' @param disease String picking the disease being plotted; always in config.
#' @param data_source Optional string of the data's source.
#'  Defaults based on disease read in from list at the end of this script.
#' @param force_plot Optional string containing "map" &/or "probability" if you
#' want to force that kind of plot regardless of geography. Default is NULL.
#' Everything else will warn you it will probably look silly.
#'
#' @examples
#' # RAG probabilities
#' outputs$rag_plotter(
#'   data = all_formatted_summary, # the summarised model results.
#'   target_name = target_name_sym, # usually admissions or occupancy.
#'   model_name = model_name, # either individual or ensemble model name.
#'   geography = c("nation", "region", "icb"), # will plot bar for national,
#'   # and maps for region & ICBs.
#'   output_path = output_path, # full path to dated outputs folder.
#'   disease = config$overall_params$disease, # all config files have now.
#' )
#'
#' @export
rag_plotter <- function(
    data,
    target_name,
    model_name,
    geography,
    output_path,
    disease,
    data_source = "",
    force_plot = NULL # allows for manual mode
    ) {
  # cleaning user input:
  disease <- user_check$disease_checker(disease)
  if (is.null(data_source) || data_source == "") { # select default sources:
    data_source <- tidyr::replace_na(data_sources[[disease]],
      "Unspecified source")
  }

  for (geo in geography) {
    geo <- match.arg(geo, c("nation", "region", "icb")) # "NHS region",
    if (geo == "nation" ||
      any(grepl("prob|confi|ci|yard", force_plot, ignore.case = TRUE))) {
      if (geo != "nation") {
        print(noquote(paste(
          "Looks like you're maybe trying to plot the probability outcomes",
          "of multiple areas at once.",
          "If you haven't already filtered the data to an individual region,",
          "there's a good chance this wont work out well.")))
      }
      plot_rag_prob <- projection_plots$plot_probability(
        data = data,
        target_name = target_name,
        model_name = model_name,
        geography = geo,
        data_source = data_source,
        disease = dplyr::case_when( #  disease name for printing
          disease %in% names(disease_prints) ~ disease_prints[[disease]],
          TRUE ~ stringr::str_to_title(disease))
      )

      ggplot2::ggsave(glue::glue("{output_path}/{geo}/{disease}_{model_name
                             }_rag_projection_probability.png"),
        plot_rag_prob,
        width = 10,
        height = 3,
        dpi = 500
      )
    }

    if (grepl("region|icb", geo, ignore.case = TRUE) ||
      any(grepl("map", force_plot, ignore.case = TRUE))) {
      if (geo == "nation") {
        print(noquote(paste(
          "Looks like you're maybe trying to map the probability outcomes",
          "of the whole country at once.",
          "If you haven't got data for multiple counutries,",
          "and patched projection_plots to accomodate this,",
          "there's a good chance this will look very silly.")))
      }

      plot_rag_map <- projection_plots$plot_maps(
        data = data,
        target_name = target_name,
        model_name = model_name,
        geography = geo,
        data_source = data_source,
        disease = dplyr::case_when( #  disease name for printing
          disease %in% names(disease_prints) ~ disease_prints[[disease]],
          TRUE ~ stringr::str_to_title(disease))
      )

      ggplot2::ggsave(
        glue::glue("{output_path}/{geo}/{disease}_{model_name}_rag_map.png"),
        plot_rag_map,
        width = 10,
        height = 11,
        dpi = 500
      )
    }
  }
}


#' LOS plotter
#'
#' Create and save COVID length of stay plots for hospital occupancy.
#' This currently doesn't loop over geographies as that's complicated and
#' not typically performed from the same data in the current running scripts.
#'
#' @param data A data frame, the formatted summary of the model outputs,
#' built from joining the discharge_results into an expanded grid.
#' @param discharge_results Data frame containing the raw model result.
#' @param geography String of the geographic level to be plotted.
#' Looping through multiple ones is difficult due to the expanded grid of data.
#' @param output_path String of the path the the timestamped output folder.
#' @param disease String picking the disease being plotted; always in config.
#' @param x_limit numeric value for the maximum length of stay to show.
#' @param data_source Optional string of the data's source.
#'  Defaults based on disease read in from list at the end of this script.
#'
#' @examples
#' # Discharge Model and Regional length of stay plots:
#' discharge_results <- run_discharge_region(training_data, overall_params)
#' # explore posterior parameters:
#' los_regional_samples <- expand.grid(
#'   t = seq(0, overall_params$max_lag, 0.1),
#'   nhs_region_name = unique(discharge_results$training_data$nhs_region_name)) |>
#'   dplyr::left_join(discharge_results$parameters, by = "nhs_region_name") |>
#'   # probability density:
#'   dplyr::mutate(p = dlnorm(t, lognormal_mu, lognormal_sigma)) |>
#'   # cumulative probability density:
#'   dplyr::mutate(cd = plnorm(t, lognormal_mu, lognormal_sigma))
#'
#' # Build summary to plot:
#' los_regional_summary <- los_regional_samples |>
#'   dplyr::group_by(t, nhs_region_name) |>
#'   dplyr::summarise(
#'     p_50 = quantile(p, 0.5),
#'     p_5 = quantile(p, 0.05),
#'     p_95 = quantile(p, 0.95),
#'     c_50 = quantile(cd, 0.5),
#'     c_5 = quantile(cd, 0.05),
#'     c_95 = quantile(cd, 0.95)
#'   ) |>
#'   dplyr::ungroup()
#'
#' # Plot length of stay:
#' outputs$los_plotter(
#'   data = los_regional_summary, # summarised discharge_results with quantiles.
#'   discharge_results = discharge_results, # direct model result
#'   geography = "region", # only one geographic level at a time
#'   output_path = output_path, # timestamped output folder path
#'   disease = config$overall_params$disease # now in all config files here.
#' )
#' @export
los_plotter <- function(
    data,
    discharge_results,
    geography,
    output_path,
    disease,
    x_limit = 15,
    data_source = ""
    ) {
  # cleaning user input:
  disease <- user_check$disease_checker(disease)
  if (is.null(data_source) || data_source == "") { # fill in sources if blank:
    data_source <- tidyr::replace_na(data_sources[[disease]],
      "Unspecified source")
  }

  dir.create(glue::glue("{output_path}/los"))

  LOS_plot <- data |>
    ggplot2::ggplot() +
    plots$theme_ham() +
    ggplot2::geom_line(ggplot2::aes(x = t, y = p_50), color = "#12436D") +
    ggplot2::geom_ribbon(ggplot2::aes(x = t, ymin = p_5, ymax = p_95),
      alpha = 0.4, fill = "#12436D") +
    ggplot2::ylab("Probability of discharge") +
    ggplot2::xlab("Days since admission")  +
    ggplot2::coord_cartesian(xlim = c(0, x_limit)) +
    ggplot2::labs(
      caption = glue::glue(
        "Data source: {data_source} from {
          format(min(discharge_results$predictions$date), '%d %B %Y')} to {
          format(max(discharge_results$predictions$date), '%d %B %Y')}",
        "Produced by Infectious Disease Modelling team - AHI - UKHSA",
        .sep = "\n\n"
      )
    )

  if (geography == "nation") {
    LOS_plot <- LOS_plot +
      ggplot2::labs(
        title = glue::glue("National {disease_prints[[disease]]
                             } estimated length of stay in hospital")
      )

    ggplot2::ggsave(glue::glue("{output_path}/los/{disease}_los_national.png"),
      LOS_plot,
      width = 10,
      height = 10,
      dpi = 500
    )
  } else if (geography == "region") {
    LOS_plot <- LOS_plot +
      ggplot2::facet_wrap(~nhs_region_name) +
      ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = "#12436D")) +
      ggplot2::labs(
        title = glue::glue("Regional {disease_prints[[disease]]
                             } estimated length of stay in hospital")
      )

    ggplot2::ggsave(glue::glue("{output_path}/los/{disease}_los_regional.png"),
      LOS_plot,
      width = 10,
      height = 10,
      dpi = 500
    )
  } # TODO? else if (geography == "icb") {}

  LOS_plot

}

#### Internal Functions ####
#' Peaks Handler
#'
#' Bundles all the horrible, disease specific peaks handeling into one place.
#' Do not export, internal use only.
#'
#' @param disease String picking the disease being plotted; always in configs.
#' @param target_name A (symbol) string of the kind of model being run;
#' current options are one of: admissions, arrival_admissions, or occupancy.
#' @param geography String of the geographic level to use; doesn't loop.
#' Only really needed for RSV.
#' @param age_granularity Optional string of the age grouping fidelity to use.
#' Should be the default "none" for all but RSV (also an option for RSV).
#' RSV can also have "fine", "course", or "full" (which shouldn't do anything).
#'
#' ~Atexport~
peaks_handler <- function( # this function is 40% of the script, minus READMES
    disease,
    target_name,
    geography = "nation", # only use one; call function within geographies loop
    age_granularity = "none" # only needed for RSV
    ) {
  if (disease == "rsv") {
    RSV_peaks_data <- peaks$get_peaks(data = NULL, disease = disease,
      metric = "peaks_lookup")
    # it helps to give it the start of the file name in case someone puts other
    # files containing the string 'rsv' in that folder.
    if ("age_group" %in% colnames(RSV_peaks_data)) {
      if (age_granularity == "none" || age_granularity == FALSE ||
        is.null(age_granularity)) { # remove duplicate lines from age groups:
        RSV_peaks_data <- dplyr::filter(RSV_peaks_data, age_group == "All ages")
      } else { # remove all-age groups from plots with age breakdowns:
        RSV_peaks_data <- dplyr::filter(RSV_peaks_data, age_group != "All ages")
      }
    }

    if (grepl("occupancy", target_name, ignore.case = TRUE)) {
      # calculating the rate before generating peaks only works at trust level
      RSV_peaks_data <- peaks$peak_rate_calculator(
        pop_data = RSV_peaks_data |> # here 'population' is the number of beds
          tidyr::pivot_wider(names_from = variable, values_from = value) |>
          dplyr::group_by(geo_type, geo_area, age_group) |>
          dplyr::summarise( # max/mean future-proofs this; summarise drops rest
            total_paed_beds = max(c(total_paed_beds, 0), na.rm = TRUE)) |>
          dplyr::ungroup(),
        peaks_data = RSV_peaks_data,
        numerator_col = "rsv_paediatric_occupancy",
        denominator_col = "total_paed_beds",
        rate_label = rlang::as_string(target_name), # in case it's not already
        disease = "rsv")
    } else if (grepl("estimate", target_name, ignore.case = TRUE) &&
      grepl("admission", target_name, ignore.case = TRUE)) {
      # Correcting for limited coverage with population contributions by area
      # Step 1: get population from source("./rsv/models/src/population.R") :
      column_order <- c("geo_type", "geo_area", "age_group", "population")
      if (!exists("trust_age_population")) {
        source("./rsv/models/src/population.R")
      } # TODO: could make less dependant on an external script?
      pop <- trust_age_population |>
        dplyr::mutate(geo_type = "trust_code") |> # standardising column names
        dplyr::rename(geo_area = trust_code) |>
        dplyr::select(all_of(column_order), nhs_region_name) # temp keep regions

      # Step 2: re-breakdown by age if necessary:
      if (age_granularity == "none") { # aggregate into all ages by trust
        pop <- pop |>
          dplyr::group_by(geo_type, geo_area, nhs_region_name) |>
          dplyr::summarise(population = sum(population, na.rm = TRUE),
            age_group = "All ages") |> # for merge match later
          dplyr::ungroup()
      } else {
        if (age_granularity == "fine") { # Fine breakdown (the default):
          age_breakdowns <- c(0, 2, 5, 18, 65, 75, 120)
        } else if (age_granularity == "course") { # Coarse breakdown:
          age_breakdowns <- c(0, 5, 18, 65, 120)
        } else { # Full breakdown:
          age_breakdowns <- c(0, 1, 2, 5, 18, 65, 75, 85, 120)
        }  # the 'full' age breakdown will be skipped, as it's how pop arrives
        age_brokedowns <- cut(seq(0, 119), age_breakdowns, right = FALSE) |>
          unique() |>
          as.character() # the age breakdowns in the form they appear in data.

        if (any(!unique(pop$age_group) %in% c("All ages", age_brokedowns))) {
          # probably a cleverer way of doing this but I don't know it right now
          pop <- pop |>
            dplyr::mutate(age_group = dplyr::case_when(
              age_granularity == "fine" &
                age_group %in% c("[0,1)", "[1,2)") ~ "[0,2)",
              age_granularity == "fine" &
                age_group %in% c("[75,85)", "[85,120)") ~ "[75,120)",
              age_granularity == "coarse" &
                age_group %in% c("[0,1)", "[1,2)", "[2,5)") ~ "[0,5)",
              age_granularity == "coarse" &
                age_group %in% c("[65,75)", "[75,85)", "[85,120)") ~ "[65,120)",
              TRUE ~ age_group)) |> # leaves the rest alone
            dplyr::group_by(geo_type, geo_area, age_group, nhs_region_name) |>
            dplyr::summarise(population = sum(population, na.rm = TRUE)) |>
            dplyr::ungroup()
        } else {
          print(noquote(paste(
            "I don't recognise these age breakdowns (in output$peaks_handler).",
            "Please add your new ones manually, or check again.")))
          break
        }
      }

      # Step 3: Group to the appropriate regional or national level. maybe ICB.
      if (length(geography) > 1) {
        stop(paste("The peaks handeler can manage one geography with RSV.",
          "Please ensure you place this function inside a geo-loop."))
      }
      if (grepl("nation", geography, ignore.case = TRUE)) {
        pop_group <- pop |>
          dplyr::group_by(age_group) |> # TODO if we ever start including the
          dplyr::summarise(geo_type = "nation", # devolved administrations
            geo_area = "England", # this will need changed
            population = sum(population, na.rm = TRUE)) |>
          dplyr::select(all_of(column_order)) # puts columns in nice order
      } else if (grepl("region", geography, ignore.case = TRUE)) {
        pop_group <- pop |>
          dplyr::group_by(age_group, nhs_region_name) |>
          dplyr::summarise(geo_type = "NHS region",
            population = sum(population, na.rm = TRUE)) |>
          dplyr::rename("geo_area" = "nhs_region_name") |>
          dplyr::select(all_of(column_order))
      } else if (grepl("icb", geography, ignore.case = TRUE)) {
        print(noquote(paste("The following process only works for a regional,",
          "or national, level.",
          "No peak lines avalible at this time.")))
        show_peaks <- FALSE
        next # maybe break?
        # TODO: prospective ICB code; merge in a list of trust_code to ICB names
        # pop_group <- pop |>
        #   dplyr::left_join(icb_name_source, by = "trust_code") |>
        #   dplyr::group_by(age_group, icb_name) |>
        #   dplyr::summarise(geo_type = "icb",
        #                    population = sum(population, na.rm = TRUE)) |>
        #   dplyr::rename("geo_area" = "icb_name") |>
        #   dplyr::select(all_of(column_order))
        # } else {
        #   pop_group <- pop # keep it at trust level
      } # if geography is anything else, this function has bigger problems :(

      # Step 4: prepare the covered population from training data:
      # to do this by ICB level would need to rebuild from sgss_clean
      if (!exists(training_data)) {
        stop(paste("The RSV peaks handeler requires training data,",
          "typically from running the admission script: DATA PREPROCESSING.",
          "Please run enough to get through that section and try again."))
      }
      trained_data <- training_data |> # removing date
        dplyr::filter(dplyr::between( # full winter 22/23 more stable than 23/24
          date, lubridate::ymd("2022-10-01"), lubridate::ymd("2023-02-28"))) |>
        dplyr::group_by(nhs_region_name, age_group) |>
        dplyr::summarise(population = mean(population, na.rm = TRUE)) |>
        dplyr::ungroup()

      if (age_granularity == "none") { # remove ages
        trained_data <- trained_data |>
          dplyr::group_by(nhs_region_name) |>
          dplyr::summarise(population = sum(population, na.rm = TRUE),
            age_group = "All ages") |>
          dplyr::ungroup()

      } else if (age_granularity %in% c("fine", "coarse") && # rescale ages
        any(!unique(trained_data$age_group) %in% c("All ages",
          age_brokedowns))) {
        trained_data <- trained_data |>
          dplyr::mutate(
            age_group = dplyr::case_when(
              age_granularity == "fine" &
                age_group %in% c("[0,1)", "[1,2)") ~ "[0,2)",
              age_granularity == "fine" &
                age_group %in% c("[75,85)", "[85,120)") ~ "[75,120)",
              age_granularity == "coarse" &
                age_group %in% c("[0,1)", "[1,2)", "[2,5)") ~ "[0,5)",
              age_granularity == "coarse" &
                age_group %in% c("[65,75)", "[75,85)", "[85,120)") ~ "[65,120)",
              TRUE ~ age_group)) |> # leaves the rest alone
          dplyr::group_by(nhs_region_name, age_group) |>
          dplyr::summarise(population = sum(population, na.rm = TRUE)) |>
          dplyr::ungroup()
      }

      if (grepl("nation", geography, ignore.case = TRUE)) {
        trained_data <- trained_data |>
          dplyr::group_by(age_group) |>
          dplyr::summarise(
            geo_area = "England", # TODO: add devolved nations someday
            contributing_population = sum(population, na.rm = TRUE)) |>
          dplyr::ungroup()
      } else { # might need cases for ICB, eventually; this is just for regions:
        trained_data <- trained_data |>
          dplyr::rename("geo_area" = "nhs_region_name",
            "contributing_population" = "population")
      }

      pop_group <- pop_group |>
        dplyr::rename(total_population = population) |>
        dplyr::left_join(trained_data, by = c("geo_area", "age_group")) |>
        dplyr::mutate(
          pop_contribution = total_population / contributing_population
        ) # this will be the multiplicative factor for the estimated admissions
      # This factor in current data ranges between 1.05 & 5.6-ish for regions
      # but I only get 1.12-2.19 in this for the previous winter period. ???
      # TODO: Might want to check up on these again.


      # Step 5: apply correction factor
      RSV_peaks_data <- RSV_peaks_data |>
        dplyr::left_join(dplyr::select(pop_group,
          geo_area, age_group, pop_contribution),
        by = c("geo_area", "age_group")) |>
        dplyr::mutate(
          variable = ifelse( # correcting admissions of just the relevant areas
            variable == "admissions" &
              grepl(geography, geo_type, ignore.case = TRUE),
            "estimated admissions",
            variable),
          value = ifelse(variable == "estimated admissions",
            value * pop_contribution, value)) |>
        dplyr::select(-pop_contribution)

      # Step 6: Re-factor Ages (if any are still involved)
      if (age_granularity != "none") {
        RSV_peaks_data <- RSV_peaks_data |>
          dplyr::mutate(age_group = forcats::fct_reorder(
            age_group,
            age_group |> # create ordering vector
              stringr::str_extract("\\d+") |> # "All ages" => ""
              as.integer() |> # "" => NA
              tidyr::replace_na(999))) # keeps non-numeric terms last)
      }
    }
    return(RSV_peaks_data)
  } else if (disease == "norovirus" &&
    grepl("occupancy", target_name, ignore.case = TRUE)) {
    noro_peaks_data <- peaks$get_peaks(
      data = NULL, disease = disease, metric = "peaks_lookup")

    noro_peaks_data <- peaks$peak_rate_calculator(
      pop_data = noro_peaks_data |> # TODO: if trough is available,
        dplyr::filter(variable == "total_beds") |> # make this the average
        dplyr::rename("total_beds" = value) |> # of max & min total_beds.
        dplyr::select(-variable, -name),
      pop_base_data = NULL, # ready made above
      peaks_data = noro_peaks_data,
      numerator_col = "occupancy",
      denominator_col = "total_beds",
      rate_label = "occupancy rate (%)",
      region_col_name = "nhs_region_name",
      disease = disease) |>
      dplyr::filter(variable != "total_beds") # no longer needed
    return(noro_peaks_data)
  } else if (disease %in% c("covid", "influenza") &&
    grepl("occupancy.*rate", target_name, ignore.case = TRUE)) {
    if (!exists("pipeline_path")) {
      config <- yaml::read_yaml(paste0(
        "./", disease, "/models/occupancy/occupancy_config.yaml"))
      pipeline_path <- s3$find_latest_file(
        uri = config$files$pipeline_data,
        pattern = config$files$pipeline_pattern)
    }
    rate_peaks_data <- peaks$peak_rate_calculator(
      pop_data = NULL, # calculated internally from base below
      pop_base_data = aws.s3::s3read_using(
        vroom::vroom,
        object = pipeline_path) |>
        dplyr::mutate(total_beds = rowSums(
          dplyr::across(
            dplyr::starts_with("total_")),
          na.rm = TRUE),
        occupancy = rowSums(dplyr::across(dplyr::starts_with("occupancy_")),
          na.rm = TRUE)),
      peaks_data = NULL, # pulled in from s3 internally
      numerator_col = "occupancy",
      denominator_col = "total_beds",
      rate_label = "bed_occupancy_rate_(%)", # needs to match target_name later
      region_col_name = "nhs_region_name",
      disease = disease)
    return(rate_peaks_data)
  } else {
    return(peaks$get_peaks(NULL, disease, "")) # general peaks lookup
  }
}

##### Internal Lookups #####
#' Common data sources and alternate print names by disease
#' usage: data_sources[[disease]] & disease_prints[[disease]]
#' don't need to export
data_sources <- list("covid" = "NHSE UEC COVID-19 data",
  "influenza" = "NHSE UEC Influenza data",
  "norovirus" = "NHSE UEC SitRep - Sentinel",
  "rsv" = "UKHSA SGSS")
disease_prints <- list("covid" = "COVID-19",
  "influenza" = "Influenza",
  "norovirus" = "Norovirus-like symptoms",
  "rsv" = "RSV")
