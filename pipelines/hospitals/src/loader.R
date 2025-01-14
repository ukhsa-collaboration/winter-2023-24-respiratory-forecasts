run_loader <- function(source_name, source_params, lookups) {
  #' Function which calls each of the individual data source loading functions
  #' Each loading function's name must match with the name in the run.yaml file
  if (source_params[[source_name]]$include == TRUE) {
    source(glue::glue("./pipelines/hospitals/sources/load_{source_name}.R"))

    fn_name <- paste0("load_", source_name)

    result <- do.call(fn_name, list(
      source_params[[source_name]]$path,
      source_params[[source_name]]$path_type,
      source_params[[source_name]]$pattern,
      lookups
    ))

    # normalise names
    names(result$data) <- tolower(names(result$data))

    return(result)

  } else {
    print(paste(source_name, "excluded from run"))
    return(NULL)
  }
}
