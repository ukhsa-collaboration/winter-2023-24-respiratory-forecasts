#' @name peaks
#' @section Version: 0.2.5
#'
#' @title
#' Combined Peaks Functions.
#'
#' @description
#'
#' The main function is get_peaks(), which operates as a wrapper around the
#' lookup_peaks() and generate_peaks() functions.
#' You may also want to use filter_peaks() on the result.
#' If you're looking up a saved set of peaks & building a new one from new data
#' you may possibly want to use compare_peaks() for a quick comparison.
#' @details
#' get_peaks():
#' The main workhouse of the module, does what it says on the tin:
#' it gets the peak values, either from a data frame you give it,
#' or from a path to a local or S3 file if you've already built a csv of peaks.
#' As an example of generating new peaks data:
#' peaks_data <- peaks$get_peaks( # needs:  box::use(prj / peaks)  to use module
#'   data = pipeline_data, # this requires date & region columns, see below.
#'  disease = "covid", # ensures it (looks up or) saves to the right disease.
#'   # column names of data you want peaks of; handles multiple & {{raw names}}:
#'   metric = c("occupancy", "admissions", "arrival_admissions"),
#'   date_col = "date", # in case the useful date column has a different name.
#'   region_col = 'nhs_region_name', # in case region column has specific name.
#'   window_size = 7, # [days] the trailing smoothing to use in rolling average.
#'   output_file = TRUE # this will save peaks as local file but not put in S3,
#' ) # To save to S3: set output_file = 'S3'; ending '/example.csv' to name it.
#' Additional ad hoc peak calculations can also be supplied here in the form:
#'   extra_peaks = list("peak name" = \(x) max(x[filter terms]), 'peak2' = etc.)
#' if your expression uses another column from the data frame, such as the date,
#' you need to wrap it in dplyr::pick(date) so the function can find it.
#'
#' If you or someone has already generated peaks you want to use,
#' then use the same function like this to read them in:
#' peaks_data <- peaks$get_peaks(
#'   data='path to data', # start 's3://' if in S3, or set =NULL for default S3.
#'   disease = "covid", # only necessary if not specifying a .csv file above.
#' ) # can optionally supply pattern = 'identifying part of file name' to help,
#'     and other terms to be passed to read in function (unnecessary now).
#'
#' Once you have a set of the peaks data you will probably need to filter it to
#' the type of area, metric, and/or specific peak you want.
#' The structure of the peaks data is:
#' geo_type: The kind of area the peak was calculated for; nation/region/trust.
#' geo_area: The area the peak was calculated for; e.g. England/Midlands/RJZ
#' variable: Column name the peak was derived from: admissions/occupancy/etc.
#' name: The name of the peak, will be printed on graphs alongside the line.
#' value: Rounded numerical value of peak, used as the y-intersect on graphs.
#'
#' You can filter this yourself, or use the filter_peaks function here:
#' selected_peaks_data = filter_peaks(
#'   peaks_data, # from whichever above source
#'   area = 'NHS region', # whatever area_type will be called in peaks_data;
#'                        # can also filter to specific area if term is present.
#'   metric = admissions, # what column was used to generate the peak.
#'   timespan = 'winter 2022/23' # keep just peak names with this string
#' ) # In the plotting function, this is built in already.
#'
#'
#' @seealso
#' * [peaks$get_peaks()]
#' * [peaks$lookup_peaks()]
#' * [peaks$generate_peaks()]
#' * [peaks$filter_peaks()]
#' * [peaks$peak_cleaning()]
#' * [peaks$peak_rate_calculator()]
#' * [peaks$compare_peaks()]
#'
".__module__."

# Outstanding TODOs in decreasing importance:
# Expand calculations for other diseases (see rates above) & filtering outliers;
# Check NHS/geographic regions based on column content not just existing name;
#     Propagate this to projection_plots and/or user_check if necessary.
# Clean up alternative date formats.
# A notes column if deemed useful;
# Make S3 save default, but impose comparison on user first;
# Potentially make S3 folder more dynamic for testing purposes;
# Move rounding to generate_peaks if we're always going to want round numbers;

box::use(
  box / deps_,
  box / help_,
  box / s3,
  prj / user_check,
  magrittr[`%>%`]
)

.on_load <- function(ns) {
  deps_$need( # ensures needed libraries are available
    "aws.s3",
    "dplyr",
    "here",
    "lubridate",
    "purrr",
    "readxl",
    "rlang",
    "slider",
    "tidyr",
    "tidyselect",
    "writexl"
  )

  deps_$min_version("dplyr", "1.1.0")
}



#' Get "peaks" using predefined definitions
#'
#' @param data A data frame, or a string path to an existing lookup file;
#'             be it local or in our S3 repository.
#' @param disease String picking the disease being looked up.
#' @param metric Variables to calculate peaks for; {{data mask}} compatible.
#' @param date_col string name of the column containing dates
#' @param region_col String of the column name containing region names in data;
#'                   gets passed to peak_cleaning to standardise names.
#' @param age_grouping String of the column name of the age groups, if used.
#'                     Defaults to NULL, will try to guess from data if TRUE.
#' @param window_size Size of (trailing) window for moving-average calculations.
#' @param output_file Option to save out as file, as this path if not NULL/FALSE
#'                    Can pass a file name with extension, or use the default.
#'                    Will then also save to S3 if string contains 's3'.
#' TODO: ATparam rate Binary option to calculate the rate from available data.
#' TODO: ATparam extra_notes String to include in the associated notes column.
#' @param ... Extra terms which can be passed into the reading in functions or
#'            for adding a list of extra peak calculations.
#'
#' @examples
#' # example usage 1: Generating new peaks from existing covid data,
#' #                  containing the columns 'date' and 'target_name'
#' #                  The latter of which you want the peaks from;
#' #                  will then put the file in your local lookup directory & S3:
#' peaks_data <- get_peaks(data, "covid", "target_name", output_file = "S3")
#'
#' # example usage 2: Lookup stored peaks from S3:
#' peaks_data <- get_peaks(NULL, "covid")
#' # if there are too many covid files in that S3 folder
#' # you must also supply metric = 'some specific part of that file name'
#'
#' @export
get_peaks <- function(
    data,
    disease = "covid",
    metric = "admissions",
    date_col = "date",
    region_col = "region",
    age_grouping = NULL,
    window_size = 7,
    output_file = NULL, ...) {
  # catch variation in typing in disease names:
  disease <- user_check$disease_checker(disease)

  if (is.null(data) || is.character(data)) {
    output <- lookup_peaks(path = data,
      disease = disease,
      pattern = {{ metric }},
      ...)
    # assign(paste0('peak_lookup_', disease), output, inherits = TRUE)
    # saves this part out to the next environment it's defined in, or globally
    # Probably don't need unless we bring in the filter here.

  } else {
    # clean up column names and regional names:
    data <- peak_cleaning(data, date_col = date_col, region_col = region_col)



    # check for multiple nations in data (after cleaning will include 'country'):
    if ("nation" %in% colnames(data)) {
      nationals <- rlang::enquo(nation)
    } else {
      nationals <- NULL
    }

    # Check for age grouping in data (used in RSV, and possibly more)
    if (!is.null(age_grouping)) {
      if (is.character(age_grouping) && # allows user to feed in column name.
        age_grouping %in% colnames(data)) {
        age_grouping <- rlang::enquo(age_grouping)
      } else if (age_grouping) { # == TRUE; tries to guess from column names.
        age_grouping <- grep("age", colnames(data),
          ignore.case = TRUE, value = TRUE)
        if (length(age_grouping) == 1) {
          user_check$user_check(paste("Use", age_grouping, "to group ages?"))
          age_grouping <- rlang::enquo(age_grouping)
        } else {
          print(noquote(ifelse(length(age_grouping) == 0,
            "No age column found.",
            paste("No unique age column found in:",
              paste(age_grouping, collapse = ", ")))))
          print(noquote("Skipping age grouping."))
          age_grouping <- NULL
        }
      }

    }
    # consistent column order, with the age group if included:
    column_order <- c("geo_type", "geo_area", "variable",
      if (is.null(age_grouping)) { # ifelse hates NULL outputs
        NULL
      } else {
        rlang::as_name(age_grouping) # keeps all as strings
      },
      "name", "value")
    # Build table
    # TODO: Add age_group term into group_vars section, check NULL wont break it
    output <- rbind(
      # National:
      generate_peaks(data = data,
        group_vars = c(nationals, !!age_grouping),
        value_vars = {{ metric }},
        window_size, disease, ...) |>
        dplyr::mutate(geo_type = "nation",
          geo_area = ifelse(
            "nation" %in% colnames(data), # for non-England;
            nation, "England")) |> # probably won't get soon
        dplyr::select(dplyr::all_of(column_order)), # keeps consistent ordering
      # Regional:
      generate_peaks(data, c(region, !!age_grouping), {{ metric }}, window_size,
        disease, ...) |>
        dplyr::mutate(
          geo_type = ifelse(
            grepl("nhs", region_col, ignore.case = TRUE) ||
              all(c("North East and Yorkshire", # NHS only regions
                "Midlands") %in% unique(data$region)),
            "NHS region", "region")) |>
        dplyr::rename(geo_area = region) |>
        dplyr::select(dplyr::all_of(column_order)),
      # ICB Level:
      generate_peaks(data, c(icb_name, !!age_grouping), {{ metric }},
        window_size, disease, ...) |>
        dplyr::mutate(geo_type = "ICB") |>
        dplyr::rename(geo_area = icb_name) |>
        dplyr::select(dplyr::all_of(column_order)),
      # Trust Level:
      generate_peaks(data, c(trust_code, !!age_grouping), {{ metric }},
        window_size, disease, ...) |>
        dplyr::mutate(geo_type = "trust code") |>
        dplyr::rename(geo_area = trust_code) |>
        dplyr::select(dplyr::all_of(column_order))
    ) |> dplyr::as_tibble()

    # Saving out as a .csv:
    if (!is.null(output_file) && !output_file %in% c(
      FALSE, "n", "N", "F", "no", "FALSE", "none", "don't")) {
      local_path <- paste0(
        system("echo $(git rev-parse --show-toplevel)", intern = TRUE),
        "/data/lookups/",
        ifelse(stringr::str_ends(output_file, ".csv"),
          basename(output_file),
          paste0("peaks_lookup_", disease, ".csv")))

      if (!dir.exists(dirname(local_path))) {
        dir.create(dirname(local_path), recursive = TRUE)
      }

      # writexl::write_xlsx(output, local_path) # Excel separated sheets version
      readr::write_csv(output, local_path) # combined csv version, ready for dbt

      if (grepl("s3", output_file, ignore.case = TRUE)) {
        # TODO: Consider making s3 save the default: || output_file == TRUE
        # But make people check it first before overwriting one that exists
        aws.s3::put_object(
          file = local_path,
          object = ifelse(stringr::str_ends(output_file, ".csv"),
            basename(local_path),
            paste0("peaks_lookup_", disease, ".csv")),
          bucket = paste0("PATH REMOVED/",
            "hospital_projections_lookups/",
            "AdmissionsExtrema") # TODO: alt output folders?
        )
      }
    }
  }
  if (!is.null(age_grouping)) { # re-factorise age groups so they stay in order
    output <- output |>
      dplyr::mutate(age_group = forcats::fct_reorder(
        age_group,
        age_group |> # create ordering vector
          stringr::str_extract("\\d+") |> # "All ages" => ""
          as.integer() |> # "" => NA
          tidyr::replace_na(999))) # keeps non-numeric terms last)
  }
  return(output)
}



#' Generate peak list for national, regional, and trusts, then can save to S3
#'
#' @param data A data frame of admissions data; requires date, region,
#'             trust code and the supplied metric columns.
#' @param group_vars column names to group by, e.g. region;
#'                   date is already included by necessity.
#' @param value_var column name(s) from data to find the peaks of
#' @param window_size number of days for the trailing window for moving-average.
#' @param disease String allowing for different calculations for each disease.
#' @param extra_peaks A named list of any additional ad hoc peak calculations,
#'                    must be of the form: list("peak name" = \(x) max(x))
#'                    To use a column not part of value_var (becomes x here),
#'                    you need to wrap it in dplyr::pick(date).
#'
#' @examples
#' generate_peaks(
#'   data,
#'   "nhs_region_name",
#'   "arrival_admissions",
#'   7,
#'   extra_peaks = list(
#'     "Arbitrary line" = 101,
#'     "Spring 2023 peak" = \(x) max(
#'       x[dplyr::between(dplyr::pick(date), "2023-03-01", "2023-05-31")]
#'     )
#'   )
#' )
#'
#' @returns A tibble of peaks in required format.
#' @export
generate_peaks <- function(data, group_vars, value_vars, window_size = 7,
                           disease = "covid", extra_peaks = NULL) {
  # Disease based peak calculations list:
  if (disease == "covid") {
    disease_peaks <- list(
      "Winter 2022/23 peak" = \(x) suppressWarnings(
        max(x[dplyr::pick(date) >= "2022-10-01" &
          dplyr::pick(date) <= "2023-02-28"])),
      "Winter 2022/23 trough" = \(x) suppressWarnings(
        min(x[dplyr::pick(date) >= "2022-10-01" &
          dplyr::pick(date) <= "2023-02-28"])))
  } else if (disease == "influenza") {
    # TODO: how to exclude outliers? then do a rate and scale up???
    disease_peaks <- list(
      "Winter 2022/23 peak" = \(x) suppressWarnings(
        max(x[dplyr::pick(date) >= "2022-10-01" &
          dplyr::pick(date) <= "2023-02-28"])))
  } else if (disease == "rsv") {
    disease_peaks <- list(
      "Winter 2022/23 peak" = \(x) suppressWarnings(
        max(x[dplyr::pick(date) >= "2022-10-01" &
          dplyr::pick(date) <= "2023-02-28"]))
    )
  } else if (disease == "norovirus") {
    disease_peaks <- list(
      "Winter 2022/23 peak" = \(x) suppressWarnings(
        max(x[dplyr::pick(date) >= "2022-10-01" & # data available from late Nov
          dplyr::pick(date) <= "2023-02-28"])))
    # "Winter 2022/23 trough" = \(x) suppressWarnings(
    #   min(x[dplyr::pick(date) >= "2022-10-01" &
    #           dplyr::pick(date) <= "2023-02-28"]))
  } else { # the old list in full, with updated syntax in case we ever need it
    disease_peaks <- list(
      "All-time peak" = \(x) suppressWarnings(max(x)),
      "Delta plateau" = \(x) suppressWarnings(
        mean(x[lubridate::floor_date(dplyr::pick(date),
          "month") == "2021-08-01"])),
      "BA2 peak" = \(x) suppressWarnings(
        max(x[lubridate::floor_date(dplyr::pick(date),
          "2 month") == "2022-03-01"])),
      "BA4/BA5 peak" = \(x) suppressWarnings(
        max(x[lubridate::floor_date(dplyr::pick(date),
          "month") == "2022-07-01"])),
      "Winter 2021/22 peak" = \(x) suppressWarnings(
        max(x[dplyr::pick(date) >= "2021-11-01" &
          dplyr::pick(date) <= "2022-02-28"])),
      "October 2022 peak" = \(x) suppressWarnings(
        max(x[lubridate::floor_date(dplyr::pick(date),
          "month") == "2022-10-01"])),
      "Winter 2022/23 peak" = \(x) suppressWarnings(
        max(x[dplyr::pick(date) >= "2022-10-01" &
          dplyr::pick(date) <= "2023-02-28"])),
      "Winter 2022/23 average" = \(x) suppressWarnings(
        mean(x[dplyr::pick(date) >= "2022-10-01" &
          dplyr::pick(date) <= "2023-02-28"])),
      "Winter 2022/23 trough" = \(x) suppressWarnings(
        min(x[dplyr::pick(date) >= "2022-10-01" &
          dplyr::pick(date) <= "2023-02-28"])))
  }

  output <- data |>
    dplyr::group_by(dplyr::across({{ group_vars }}), date) |>
    dplyr::summarise(
      dplyr::across({{ value_vars }}, ~ sum(.x, na.rm = TRUE)),
      .groups = "drop_last"
    ) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      dplyr::across(
        {{ value_vars }},
        ~ slider::slide_index_dbl(.x, date, mean, .before = window_size - 1,
          .complete = TRUE)
      )
    ) |>
    tidyr::drop_na() |>
    dplyr::summarise(
      dplyr::across(
        {{ value_vars }},
        c(disease_peaks, extra_peaks), # Allows adding new ad hoc peak functions
        .names = "{.col}[{.fn}]"
      )
    ) |>
    dplyr::ungroup() |> # removing -Inf's from places with no data in timespan:
    dplyr::filter(dplyr::if_any(dplyr::where(is.double), ~ is.finite(.)))

  # print(output)

  if (nrow(output) > 0) {
    output <- output |>
      tidyr::pivot_longer(
        cols = -{{ group_vars }},
        names_to = c("variable", "name"),
        names_pattern = "^(.*)\\[(.*)\\]$"
      ) |>
      dplyr::ungroup() %>% # this one has to be a dplyr pipe.
      dplyr::mutate(value = round(as.numeric(value), ifelse(.$value > 1, 0, 2)))
    # print(output)
    return(output)
  } else {
    stop(paste("No output available.",
      "Check there is any real data for the dates you're looking at."))
  }

}

#' Data Cleaning Function:
#' Ensures df contains the 'date' column, and can generate a 'metric' column.
#' Also sets all column names to lower-case for consistency,
#' and converts any 'country' column to 'nation'.
#'
#' @param raw_data Data frame to clean column names of
#' @param date_col String containing the column name of the date data
#'        will convert non-date format to date using ymd (TODO: more options)
#' @param region_col String containing the column name containing the regions;
#'                   will clean names to standard form.
#' @param maintain_colnames Boolean indicating whether to keep the column names
#'                          found in the original data.
#'                          Useful for external calls, to clean region names.
#' @export
peak_cleaning <- function(raw_data, date_col = "date", region_col = "region",
                          maintain_colnames = FALSE) {
  cleaned_data <- raw_data |>
    dplyr::rename("date" = date_col,
      "region" = region_col) |>
    dplyr::mutate(region = stringr::str_to_title(region) |>     # standardising
      stringr::str_remove(" Commissioning Region") |> # region
      stringr::str_replace_all(" Of ", " of ") |>      # names
      stringr::str_replace_all(" And ", " and ") |>
      stringr::str_replace_all(" The ", " the ") |>
      stringr::str_trim("both")) |>
    dplyr::rename_with(stringr::str_to_lower) # all column names lower case
  if (!lubridate::is.Date(cleaned_data$date)) {
    # TODO: build a checker for different date formats:
    # date_checker = dplyr::first(cleaned_data$date, na_rm = TRUE)
    # date_form = tryCatch(lubridate::ymd(date_checker))
    cleaned_data <- dplyr::mutate(cleaned_data, date = lubridate::ymd(date))
  }

  if (!maintain_colnames && "country" %in% colnames(cleaned_data)) {
    cleaned_data <- dplyr::rename(cleaned_data, "nation" = "country")
  }

  if (maintain_colnames) { # revert column names back to source
    cleaned_data <- dplyr::rename_with(cleaned_data,
      .fn = ~ c(date_col, region_col),
      .cols = c("date", "region"))
  } # TODO: There may be circumstances to change only one col, but this is fine.
  return(cleaned_data)
}

#' Peaks Lookup Function
#' Loads in the desired peaks lookup file, from S3 or locally.
#' You can supply a full file path, or just the disease and a file name pattern;
#' local files can also be similarly searched for if you supply the folder path.
#' Will break if it finds no or too many files.
#' Currently accepts .csv, including if zipped as .zip/.gz, or .xls/.xlsx files.
#' Feel free to add more as necessary.
#' Additional terms can be passed on to whichever read in function will be used.
#' @param path string of path to file, or folder, which can be local or in S3.
#'             If specifying a folder, it will probably need these filters:
#' @param disease String to pick the disease from those we have built peaks for.
#' @param pattern String to then uniquely identify the file name.
#' @param ... Space for additional terms passed to the read in function.
#' @returns A data frame of peaks in standard format.
#' @export
lookup_peaks <- function(path = NULL, disease = "covid", pattern = NULL,
                         ...) {



  if (is.null(path) || stringr::str_starts(path, "s3://")) {
    # read in from S3

    route <- ifelse(is.null(path), # I know it's clunky checking the this twice;
      paste0("s3://REMOVED PATH/", # all S3 stuff from
        "hospital_projections_lookups/", # here is the same.
        "AdmissionsExtrema/"), # default folder path
      path) # user supplied S3 path

    if (stringr::str_ends(route, ".csv|.zip|.gz|.xls|.xlsx", negate = TRUE)) {
      remote_path <- s3$list_objects(route, pattern = disease) |>
        dplyr::mutate(fname = stringr::str_remove(uri, route)) |>
        # next line is a test, might be too strict but would be useful if works:
        dplyr::filter(stringr::str_starts(fname, "peaks_lookup_")) |> # no :(
        dplyr::arrange(dplyr::desc(last_modified))

      pattern <- rlang::enquo(pattern) |> # defusing a potential data mask term
        rlang::quo_get_expr()
      # |> rlang::as_string() # should be fine without this; symbols work too
      if (!is.null(pattern)) {
        remote_path <- dplyr::filter(remote_path,
          grepl(pattern, fname, ignore.case = TRUE))
      }
      if (nrow(remote_path) == 1) {
        route <- remote_path$uri
      } else {
        print(noquote(paste(ifelse(nrow(remote_path) == 0, "No", "Too many"),
          "lookup files found.")))
        print(noquote(paste("Please check what you're looking up,",
          "and try again later.")))
        if (nrow(remote_path) > 1) {
          print(noquote(paste(remote_path$fname, collapse = "; ")))
        }
        break
      }
    }
    # we've found the right thing! probably...
    output <- aws.s3::s3read_using(FUN = ifelse(
      stringr::str_ends(route, ".xls|.xlsx"),
      readxl::read_excel, readr::read_csv),
    object = route,
    show_col_types = FALSE, ...)
  } else {
    # read in from local source
    if (stringr::str_ends(path, ".csv|.zip|.gz|.xls|.xlsx", negate = TRUE)) {
      wd <- ifelse(exists("wd"), wd, here:here())

      local_path <- data.frame(paths = list.files(paste0(
        ifelse(stringr::str_starts(path, "~/|/home/"), "",
          paste0(wd, ifelse(
            stringr::str_ends(wd, "/") &
              stringr::str_starts(path, "/", negate = TRUE), "", "/"))),
        path), pattern = disease, full.names = TRUE)) |>
        dplyr::mutate(fname = basename(paths))

      pattern <- rlang::enquo(pattern) |> # defusing a potential data mask term
        rlang::quo_get_expr()
      if (!is.null(pattern)) {
        local_path <- dplyr::filter(local_path,
          grepl(pattern, fname, ignore.case = TRUE))
      }
      if (nrow(local_path) == 1) {
        path <- local_path$paths[1]
      } else {
        print(noquote(paste("No", ifelse(nrow(local_path) == 0, "", "unique"),
          "lookup file found at:", path)))
        print(noquote("Check your path and filter terms, and try again."))
        # Could try pulling from S3 instead if no files found locally.
        if (nrow(local_path) > 1) {
          print(noquote("Known files within path:"))
          print(noquote(paste(local_path$fname, collapse = "; ")))
        }
        break
      }
    }
    if (stringr::str_ends(path, ".csv|.zip|.gz")) {
      output <- readr::read_csv(path, ...)
    } else if (stringr::str_ends(path, ".xlsx|.xls")) {
      output <- readxl::read_excel(path, ...)
    } else {
      print(noquote(paste("Unrecognised file format:",
        sub(".*\\.", ".", path))))
      print(noquote(paste("Please add read in function to:",
        "PATH REMOVED/src/R/prj/peaks.R")))
    }
  }

  return(output)
}

#' Peaks Compare Function
#' For comparing two sets of peak listings.
#' You can supply each as data frames, or paths to load them in (local or S3).
#' If given a generic path, you must supply the disease and a file name pattern.
#' Will break if it finds no or too many files.
#' If the two data frames have different column names, this might break.
#' Best fixed prior to function.
#'
#' @param old_peaks Data frame, or path to look up, of the preavious peaks data.
#' @param new_peaks Data frame, or path to look up, of the latest peaks data.
#' @param ... Space for additional terms passed to lookup functions;
#'            Most likely to need strings for disease and file pattern.
#'            Maybe some terms for read in functions.
#'            If the sources will need different terms, read them in yourself.
#' ATparam pattern String to then uniquely identify the file name.
#' @returns A data frame of merged peaks, with a table of differences between
#' @export

compare_peaks <- function(old_peaks, new_peaks, ...) {
  # can supply paths to old and/or new peaks;
  # will require disease = [], pattern = [] in call if not given complete path
  if (is.character(old_peaks)) {
    old_peaks <- lookup_peaks(path = old_peaks, ...)
  }

  if (is.character(new_peaks)) {
    new_peaks <- lookup_peaks(path = new_peaks, ...)
  }

  both_peaks <- merge(dplyr::rename(old_peaks, old_value = value),
    dplyr::rename(new_peaks, new_value = value),
    all = TRUE) |>
    dplyr::mutate(old_peaks = round(old_peaks),
      new_peaks = round(new_peaks),
      diff = dplyr::case_when(
        old_peaks == new_peaks ~ "Same",
        old_peaks > new_peaks ~ "New lower",
        old_peaks < new_peaks ~ "New higher",
        is.na(old_peaks) & !is.na(new_peaks) ~ "Newly added",
        !is.na(old_peaks) & is.na(new_peaks) ~ "Newly missing",
        TRUE ~ "Incomparable"))

  print(table(both_peaks$diff))
  return(both_peaks)
}


#' Peaks Filter Function
#' For filtering peak listings down to just the ones you want.
#' You supply the peaks data frame, and the terms you want.
#' This returns just those peaks that fit all your terms.
#'
#' @param peaks_data Data frame of the peaks data.
#' @param area String for the type of area you want covered. Takes only one.
#' @param metric String for the desired variable(s) to keep.
#' @param timespan String for the grep pattern to search the peak names for.
#'                 Multiple patterns will be collapsed into one 'a|b|c' pattern.
#' @returns A data frame of the remaining peak rows.
#' @export

filter_peaks <- function(peaks_data,
                         area = NULL, metric = NULL, timespan = NULL) {
  # Area Filter
  if (!is.null(area[1])) {
    area <- stringr::str_to_lower(area[1]) # standardising
    # checking if you mean to filter down to a particular area:
    if (area %in% stringr::str_to_lower(unique(peaks_data$geo_area))) {
      peaks_data <- dplyr::filter(peaks_data, geo_area == area)
    } else { # picking the type of area to filter by
      if (grepl("nation", area) || area %in% c(
        "country", "britain", "everything", "everywhere", "all")) { # at once
        area <- "nation"
      } else if (grepl("region", area)) {
        avalible_regions <- grep("region", unique(peaks_data$geo_type),
          ignore.case = TRUE, value = TRUE)
        if (length(avalible_regions) == 1) {
          area <- avalible_regions # if only one option, take it
        } else if (area %in% avalible_regions) {
          area <- area # filter term already correct
        } else if ((grepl("nhs", area) && "NHS region" %in% avalible_regions) ||
          !"region" %in% avalible_regions) {
          area <- "NHS region" # asking for NHS or no non-NHS base version
        } else {
          area <- "region" # default
        }
        if (!area %in% avalible_regions) {
          print(noquote(paste(
            "No identifiable type region found.", "Did you mean 'NHS region'?",
            "Please select one of the following next time:",
            paste(avalible_regions, collapse = "; "),
            "Or rebuild peaks data to include the right kind of region.")))
          break
        }
      } else if (grepl("trust|hospital", area)) {
        area <- "trust code"
      } else {
        print(noquote(paste(
          "No identifiable area found.",
          "Please select one of the following next time:",
          paste(unique(peaks_data$geo_type), collapse = "; "))))
        break # might be a little overzealous?
      }
      peaks_data <- dplyr::filter(peaks_data, geo_type == area)
    }
  }

  # Variable filter
  if (!is.null(metric)) {
    metric <- stringr::str_to_lower(metric) # standardise & check it's available
    if (any(metric %in% stringr::str_to_lower(unique(peaks_data$variable)))) {
      peaks_data <- dplyr::filter(peaks_data, variable %in% metric)
    } else {
      print(noquote(paste(
        grep(paste(unique(peaks_data$variable), collapse = "|") |>
          stringr::str_to_lower(),
        metric, invert = TRUE, value = TRUE) |>
          paste(collapse = "; "), "not found in peaks data.",
        "Please try again with a new peaks data set",
        "that includes your desired metric.")))
      break
    }
  }

  # Time period/Name filter
  if (!is.null(timespan)) {
    if (length(timespan) > 1) {
      timespan <- paste(timespan, collapse = "|")
    }
    peaks_data <- dplyr::filter(peaks_data, grepl(timespan, name, ignore.case = TRUE))
  }
  if (nrow(peaks_data) > 0) {
    return(peaks_data)
  } else {
    print(noquote(paste(
      "You seem to have filtered out all your peaks data.",
      "Please check your data and filter terms are correct and try again.")))
    break
  }
}

# Converting Raw Occupancy peaks into Occupancy rates

#' Peaks Normalisation Function
#' Takes the totals from historic peaks data and some data to use as denominator
#' either as a format similar to the original data frame as pop_base_data, or
#' as the cleaner population baseline pop_data already derived from that;
#' otherwise done here from pop_base_data. Either can be read from S3 URI's.
#' Should merge by age groups if you supply peaks data and pop data with a
#' matching 'age_group' column in both. Currently these age groups are not built
#' from pop_base_data and this these must be calculated externally.
#' Additional terms can be passed on to get_peaks if no peaks data is supplied.
#' @param pop_data Data frame with the pre-built population or other denominator
#'                 to use, or string starting "s3://" with S3 path to such.
#' @param pop_base_data Data frame, or string of S3 path to equivalent, to build
#'                      pop_data from if not supplied above.
#' @param peaks_data Data frame containing peaks data to be turned into a rate
#' @param numerator_col String of the peaks variable you want turned into rate.
#' @param denominator_col String of column name of the population denominator.
#' @param rate_label String to rename numerator to in peaks data.
#' @param region_col_name String of column name in pop_base_data for region name
#'                        cleaning and standardising.
#' @param date_col_name String of date column name in pop_base_data to clean.
#' @param disease String containiang disease name.
#' @param ... Space for additional terms to be passed to get_peaks.
#' @returns A data frame of selected peak data rates in the standard format.
#' @export

peak_rate_calculator <- function(
    pop_data = NULL,
    pop_base_data = NULL, # it needs one of these input; prioritises pop_data
    peaks_data = NULL, # tries to get from S3, then generate from pop_base_data
    numerator_col,
    denominator_col = "population",
    rate_label = paste0(numerator_col, "_rate"), # basic default
    region_col_name = "region",
    date_col_name = "date",
    disease = "covid",
    ...) {
  if (is.null(pop_base_data) & is.null(pop_data)) {
    print(noquote(paste(
      "This function requires a source for the denominator.",
      "pop_data for pre-selected denominator for each area;",
      "pop_base_data will attempt to build its own pop_data.")))
    break
  }

  disease <- user_check$disease_checker(disease) # check disease matches folders

  if (is.null(pop_data) & !is.null(pop_base_data)) {
    if (is.character(pop_base_data) && # take S3 path as an option
      stringr::str_starts(pop_base_data, "s3://")) {
      pop_base_data <- aws.s3::s3read_using(vroom::vroom, object = pop_base_data)
    }

    peak_date_start <- lubridate::ymd("2022-10-01") # TODO These dates must match
    peak_date_close <- lubridate::ymd("2023-02-28") # the given period in disease

    # Generate baselines from this data:
    pop_base_data <- pop_base_data |>
      # dplyr::rename(date = date_col_name, region = region_col_name) |>
      peak_cleaning(date_col = date_col_name, # should be fixed elsewhere
        region_col = region_col_name) %>% # clean region names
      dplyr::select("date", "trust_code", "region", # needs dplyr pipes for this
        ifelse("nation" %in% names(.), "nation", "region"),
        numerator_col, denominator_col) |>
      dplyr::filter(date >= peak_date_start & date <= peak_date_close,
        !!rlang::sym(denominator_col) > 0) # I'd hope these are just wrong.

    if (nrow(pop_base_data) == 0) { # in case beds data is missing one week:
      pipeline_path2 <= paste0(
        "s3://REMOVED PATH/pancast_pipeline/", disease, "/",
        dplyr::case_when( # TODO Test the non-covid ones work:
          disease == "covid" ~ "2023-09-13_08:36:34.519772_covid_beds_",
          disease == "influenza" ~ "2023-08-16_16:04:59.909742_influenza_beds_",
          disease == "norovirus" ~ "2023-09-25_12:57:39.087446_norovirus_beds_",
          disease == "rsv" ~ "2023-10-25_09:18:32.815159_rsv_beds_",
          TRUE ~ "Unidentified"
        ),
        dplyr::case_when(
          disease == "covid" ~
            "oneoneone_calls_oneoneone_online_google_combined",
          disease == "influenza" ~ "oneoneone_online_ensemble",
          disease == "norovirus" ~ "univariate",
          disease == "rsv" ~ "inpatient_tests_univariate",
          TRUE ~ "Unidentified"
      ), "_prod.csv.gz")

      print(noquote(paste(
        "Your pipeline data seems to be missing all the beds for every trust.",
        "Maybe a historic cut off or something more fundamental.",
        "You might want to go check the pipeline isn't broken.",
        "For now this will use an altanative pipeline we've checked works.")))

      pop_base_data <- aws.s3::s3read_using(
        vroom::vroom,
        object = pipeline_path2) |>
        dplyr::mutate(total_beds = rowSums(
          dplyr::across(
            dplyr::starts_with("total_")),
          na.rm = TRUE)) |>
        peak_cleaning(date_col = date_col_name, # necessary for older data
          region_col = region_col_name) |>
        dplyr::select(date, trust_code, region, dplyr::any_of(c("nation", "region")),
          numerator_col, denominator_col) |>
        dplyr::filter(date >= peak_date_start & date <= peak_date_close,
          !!rlang::sym(denominator_col) > 0)
    }

    if (!"nation" %in% names(pop_base_data)) {
      pop_base_data <- pop_base_data |>
        dplyr::mutate(nation = ifelse(
          region %in% c( # needs the region names cleaned in pipeline
            "East of England", "London", "Midlands", "North East and Yorkshire",
            "North West", "South East", "South West", # adding non-NHS regions:
            "North East", "East Midlands", "West Midlands",
            "Yorkshire and the Humber"),
          "England",
          region)) # assumes devolved administrations would have themselves here
    }

    # build divisors
    base_trust <- pop_base_data |>
      dplyr::group_by(trust_code, region, nation) |>
      dplyr::summarise(denominator = round(mean( # TODO: max not round-mean?
        !!(rlang::sym(denominator_col))))) |> # na.rm?
      dplyr::ungroup()

    base_region <- base_trust |>
      dplyr::group_by(region, nation) |>
      dplyr::summarise(denominator = sum(denominator)) |>
      dplyr::ungroup()

    base_nation <- base_region |>
      dplyr::group_by(nation) |>
      dplyr::summarise(denominator = sum(denominator)) |>
      dplyr::ungroup() |> # standardising columns before row binding:
      dplyr::mutate(geo_type = "nation") |>
      dplyr::rename(geo_area = nation) |>
      dplyr::select(geo_type, geo_area, denominator)

    # combine:
    pop_data <- rbind(
      base_nation,
      base_region |>
        dplyr::mutate(geo_type = ifelse(
          grepl("NHS", region_col_name, ignore.case = TRUE),
          "NHS region", "region")) |>
        dplyr::rename(geo_area = region) |>
        dplyr::select(geo_type, geo_area, denominator),
      base_trust |>
        dplyr::mutate(geo_type = "trust code") |>
        dplyr::rename(geo_area = trust_code) |>
        dplyr::select(geo_type, geo_area, denominator))

  } else if (is.character(pop_data) && stringr::str_starts(pop_data, "s3://")) {
    # take S3 path as an option
    pop_data <- aws.s3::s3read_using(vroom::vroom, object = pop_data)
  } else { # ensures column is correct for the following code
    pop_data <- dplyr::rename(pop_data, "denominator" = denominator_col)
  }

  # pull in peak data, if not input directly
  if (is.null(peaks_data)) {
    peaks_data <- tryCatch( # in case there's no saved peaks data
      expr = get_peaks(data = NULL, metric = "peaks_lookup",
        disease = disease, ...),
      # TODO: Delete/move old peaks file in S3, then it won't need metric:
      # https://REMOVED PATH/
      # hospital_projections_lookups/AdmissionsExtrema/admissions_peaks.xlsx
      error = \(e) get_peaks(
        peaks_data_base |>
          dplyr::rename_with(.fn = region_col_name,
            .cols = "region"), # changes name back to match
        metric = numerator_col,
        region_col = region_col_name,
        disease = disease,
        ...))
  }

  # Accounting for the possibility of joining by age group too
  if ("age_group" %in% colnames(peaks_data) && # You will have to be sure they're
    "age_group" %in% colnames(pop_data)) { # both called age_group yourself
    joining_groups <- dplyr::join_by(geo_type, geo_area, age_group)
  } else {
    joining_groups <- dplyr::join_by(geo_type, geo_area)
  }

  # Merge population denominator into peaks data.
  peaks_data <- dplyr::left_join(peaks_data, pop_data, by = joining_groups) |>
    dplyr::mutate(
      variable = ifelse( # Defuse col name in case it came in for {{embraceure}}
        variable %in% rlang::quo_get_expr(rlang::enquo(numerator_col)),
        rate_label, variable), # corrects names due to be made into rates
      value = ifelse(
        variable == rate_label,
        value / denominator * 100, # rate value [%]
        value), # keep any other peaks the same
      value = ifelse(value > 0, round(value, 2), 0)) |> # catches NA's too
    dplyr::select(-denominator)

  return(peaks_data)
}
