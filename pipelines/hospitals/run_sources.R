# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#### DESCRIPTION ####
# Script to build combined data for hospital admissions forecasting.
#
# The data set is defined by a config file of the structure run.yaml.
#
# Each data source/stream has a loading function which manipulates it into
# trust:date format, which can be joined onto the NHSE&I total beds data
#
# All bed metrics & leading indicators are left joined onto the total beds data.
# This script loops through each source and joins it one by one, based on the
# config's parameters.
#
# The data is clipped to be complete over the whole time range. Therefore
# how up to date the data is, is determined by the most lagged data set.
#
# See this directory's README for more detail.
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

setwd(system("echo $(git rev-parse --show-toplevel)", intern = TRUE))
current_time <- gsub(" ", "_", Sys.time())
Sys.setenv("R_BOX_PATH" = fs::path(rprojroot::find_root(rprojroot::is_git_root), "src", "R"))

# install all required packages for the data pipeline
source("./pipelines/src/depends.R")
source("./pipelines/hospitals/sources/load_lookups.R")
source("./pipelines/hospitals/src/loader.R")
source("./pipelines/hospitals/src/plot_metrics.R")
source("./pipelines/hospitals/src/NAchecker.R")


# # # # # # # # # #
####  CONFIG   ####
# # # # # # # # # #

disease_choice <- "covid"
pipeline_choice <- "ensemble" # "ensemble" or "univariate"


# change which config file to use for each model
run_config_file <- paste0("./pipelines/hospitals/config/run_", disease_choice,
  "_", pipeline_choice, ".yaml")

params <- yaml::read_yaml(run_config_file)
source_params <- params$sources
lookup_paths_list <- yaml::read_yaml(
  "./pipelines/hospitals/config/lookup_paths.yaml")

# which data sources to load:
keep_sources <- c()
for (source_name in names(source_params)) {
  if (source_params[[source_name]]$include == TRUE) {
    keep_sources <- append(keep_sources, source_name)
  }
}

# output
used_sources_name <- paste0(
  gsub(glue::glue("{disease_choice}_"), "", keep_sources[-1]),
  collapse = "_")

output_name <- paste0(
  current_time, "_",
  disease_choice, "_",
  used_sources_name, "_",
  params$run_type,
  ".csv.gz"
)
bucket_path <- params$output_bucket

print("Config complete")



# # # # # # # # # #
####  LOOKUPS  ####
# # # # # # # # # #

lookups <- load_lookups(lookup_paths_list)
print("Lookups loaded")


# # # # # # # # # # # # # # #
####  FEATURES/TARGETS   ####
# # # # # # # # # # # # # # #

results <- list()

for (source_name in keep_sources) {
  print(paste0("running: ", source_name))
  results[[source_name]] <- run_loader(source_name, source_params, lookups)
  print(paste0("loaded: ", source_name))
}



# # # # # # # # # # # # #
####  JOIN FEATURES  ####
# # # # # # # # # # # # #

combined_data <- results$total_beds$data

for (source_name in keep_sources) {
  if (source_name != "total_beds") {
    print(paste0("joining: ", source_name))
    combined_data <- combined_data %>%
      dplyr::left_join(results[[source_name]]$data,
        c("trust_code" = "trust_code", "date" = "date"))
  }
}
print("all data sources joined")


# Filter combined data from latest first date to earliest last date of sources

min_dates <- c()
max_dates <- c()
for (source_name in names(results)) {
  min_dates <- append(min_dates, results[[source_name]]$min_date)
  max_dates <- append(max_dates, results[[source_name]]$max_date)
}

print("Max dates of each source")
freshness <- data.frame(source = keep_sources, most_recent_date = max_dates)
print(freshness)

user_check$user_check(lead_message = "Happy with the dates?")

combined_data <- combined_data %>%
  dplyr::filter(date >= max(min_dates) & date <= min(max_dates)) %>%
  dplyr::filter(date >= params$earliest_date)
print("Dates filtered")

print(paste0("Earliest date: ", min(combined_data$date)))
print(paste0("Most recent date: ", max(combined_data$date)))

View(combined_data)



# # # # # # # # #
####  FIXES  ####
# # # # # # # # #

# TODO
# Create separate script for ad hoc fixes for each pipeline,
# each fix contained within a function called on `combined_data`
#
#

# # # # # # # # # # # #
####  DATA QUALITY ####
# # # # # # # # # # # #

##### Check NAs #####

# TODO: WIP
NAchecker(combined_data)


##### Plot metrics #####

plots_output_path <- glue::glue(
  "./pipelines/hospitals/outputs/metric_plots/{current_time}/")
dir.create(plots_output_path, recursive = TRUE)
plot_metrics(results, combined_data)
print("Plotting complete")


# # # # # # # # # # #
####  WRITE DATA  ####
# # # # # # # # # # #

if (params$write == TRUE) {
  s3_path <- s3$write_to_s3(combined_data,
    glue::glue("s3://{bucket_path}/{output_name}"))
}

if (params$wipe == TRUE) {
  rm(list = ls())
}
