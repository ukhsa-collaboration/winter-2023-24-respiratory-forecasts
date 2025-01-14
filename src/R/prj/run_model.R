#' @name run_model
#' @section Version: 1.0.1
#'
#' @title
#' Function to take data and params and run models over each lookback.
#'
#' @description
#' Wrapper that adds the parallelisation and different lookbacks.
#' TODO: Maybe add some of the occupancy model running functions here,
#'       but they look quite different between diseases so requires realigning.
#'
#' @seealso
#' * [run_model$run_scripted_model()]
".__module__."

# Sys.setenv("R_BOX_PATH" = fs::path( # in case not set already?
#   rprojroot::find_root(rprojroot::is_git_root), "src", "R"))
box::use(box / deps_,
  prj / user_check,
  prj / parallel)

.on_load <- function(ns) {
  deps_$need("dplyr",
    "stringr",
    "parallel",
    "doParallel")

  deps_$min_version("dplyr", "1.1.0")
}

#' Wrapper function for running the selected model from script
#'
#' Takes data, parameters, and model selection to run over each lookback.
#' This wrapper can also run multiple models in parallel on different lookbacks.
#'
#' @param wd string of working directory, defaults to git root folder.
#' @param model_name String of the function name of the model to be run;
#' in most cases this will be the same as the script name it came from,
#' if not supply full model_path within overall_params below.
#' @param training_data Data frame of the input data.
#' @param overall_params A named list of parameters for the model.
#' This is generally taken from config, but could be manually set in a script.
#' Now requires a disease term or it defaults to 'covid'.
#' Can accept a metric, either admissions or occupancy based on model/folder,
#' else it will ask about its attempted mapping from target_name.
#' Can accept a model_path to pick a model totally manually: this path should
#' include everything from working directory to the final .R of the file name.
#' @param start_dates A sequence of dates defining the start of each look back.
#' @param required_covariates A list of strings of data's column names to keep,
#' generally already listed in the config.
#' @param model_hyperparams A named list of numerical priors for the model,
#' also generally found in the config.
#'
#'
#' @returns Combined model outputs, as a data frame.
#'
#' @examples
#' config <- yaml::read_yaml(config_path) # wherever your configurations file is
#'
#' model_outputs <- run_model$run_scripted_model(
#'   wd, # working directory
#'   model_name = "univariate", # the model function name
#'   training_data = training_data, # pipeline data
#'   overall_params = config$overall_params, # includes disease & metric
#'   start_dates = start_dates, # list of dates for lookbacks
#'   required_covariates = config$required_covariates, # training_data columns
#'   model_hyperparams = config$hyperparams$calls # priors for model
#' )
#'
#' @export

run_scripted_model <- function(
    wd = system("echo $(git rev-parse --show-toplevel)", intern = TRUE),
    model_name,
    training_data,
    overall_params,
    start_dates,
    required_covariates,
    model_hyperparams
    ) {
  if (is.null(overall_params$model_path)) { # if there isn't anything set
    if (is.null(overall_params$disease)) {
      print(noquote(
        paste(
          "The run_model functionneeds to be told what disease to use now.",
          "Will guess covid for now.",
          "In future please set it up in overall_params$disease"
        )
      ))
      overall_params$disease <- "covid"
    } else {
      overall_params$disease <- user_check$disease_checker(overall_params$disease)
    }

    # Metric Checker
    metric <- ifelse(
      is.null(overall_params$metric), # can be given directly.
      overall_params$target_name,
      overall_params$metric
    )
    if (!metric %in% c("admissions", "occupancy")) {
      user_metric <- metric
      metric <- dplyr::case_when(
        # If arrival_admissions becomes separate thing put it here.
        grepl("admi", metric, ignore.case = TRUE) ~ "admissions",
        grepl("occ", metric, ignore.case = TRUE) ~ "occupancy",
        TRUE ~ metric
      ) # for other models in future, on your own head be it
      if (metric != user_metric) {
        print(noquote(paste0('Your entered metric was: "', user_metric, '".')))
        print(noquote(paste0('Known metrics are "admissions" or "occupancy".')))
        print(noquote(paste0("We're guessing you meant ", '"', metric, '".')))
        user_check$user_check("Is this correct?")
      }
    }
    # match.arg() is cleaner than these, but less flexible
    # e.g. I don't find pmatch() picks up 'flu' for 'influenza' while mine does.

    model_path <- file.path(
      wd,
      overall_params$disease,
      "models",
      metric,
      "models",
      ifelse(
        stringr::str_ends(model_name, ".R"),
        model_name,
        paste0(model_name, ".R")
      )
    )
  } else {
    model_path <- overall_params$model_path
  }

  if (file.exists(model_path)) {
    suppressWarnings(source(model_path))
  } else {
    stop(paste(
      "No such model file found. Please check this path for mistakes:",
      model_path
    ))
  }

  print(glue::glue("Fitting {model_name} model: started"))

  `%runloop%` <- parallel$set_parallel_type(overall_params$is_parallel)

  # create parallel clusters
  if (overall_params$is_parallel) {
    cl <- parallel::makeCluster(overall_params$n_lookbacks, type = "FORK")
    doParallel::registerDoParallel(cl)
  }
  model_outputs <- foreach::foreach(
    start_date = start_dates,
    .combine = "c") %runloop% {
    outputs <- do.call(
      paste0("run_", stringr::str_remove(model_name, ".R$")),
      args = list(
        .data = training_data,
        forecast_horizon = overall_params$forecast_horizon,
        n_pi_samples = overall_params$n_pi_sample,
        prediction_start_date = start_date,
        output_variables = required_covariates,
        hyperparams = model_hyperparams
      )
    )

    nesting <- list()
    nesting[[as.character(start_date)]] <- outputs
    nesting
  }

  if (overall_params$is_parallel) {
    parallel::stopCluster(cl)
  }

  print(glue::glue("Fitting {model_name} model: done"))

  return(model_outputs)

}
