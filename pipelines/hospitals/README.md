# Data Preparation

Takes range of data sources, then processes, joins and uploads to S3 for hospital forecasting.

### /sources/

Contains an R file:

- for each data source used for the prediction model (`load_{data source}.R`)
- to load all of the lookup tables and redshift data model (`load_lookups.R`)

### /config/

Contains a yaml file:

- which defines the data set parameters for each disease and pipeline choice (`run_{disease}_{pipeline}.yaml`)
- which defines the lookup paths (`lookup_paths.yaml`)

### /src/

Contains a function in an R file:

- to load all dependent packages (note all functions within the pipeline code are defined as `{package}::function}` so packages do not necessarily need to be loaded) (`depends.R`)
- to call each of the individual data source loading functions in `/sources/` (`loader.R`)

Contains data quality functions in an R file:

- to check for NAs in columns of the combined data - WIP (`NAchecker.R`)
- to plot recent time series for the metrics in each data source as a data quality check (`plot_metrics.R`)
- [Now moved to Box] to prompt the user to check and acknowledge output (`user_check.R`)



## Run

To run the process, source the file `./run_sources.R` and specify `disease_choice` (e.g. "influenza") and `pipeline_choice` (e.g. "univariate" for hospital admissions or occupancy models without leading indicator (only response variables), or "ensemble" for ensemble models with various data sets) which match a config file.

The config files are stored in `./config/run_{disease_choice}_{pipeline_choice}.yaml` and specifies the data sources to be loaded for each disease and pipeline choice.

The data loading files for each data source within the config file are stored in `./sources/load_{data source}.R`.



## Design

The main principles of this design:

1. data sets are defined separately, so they can be loaded independently
2. lookup files are shared across data set loaders
3. the data set is defined by parameters within `run_{disease}_{pipeline}.yaml`
4. different data sets can be selected for different combinations of sources, for the different models
5. each data source has a unique name, acting as a key across the processing, such as "beds", "google" etc.

The data sources are always joined onto a total beds dataset.

Each config file `run_{disease}_{pipeline}.yaml` should specify:

- a string to be added to the output file name describing the purpose or model for the combined data set, e.g. `ensemble_prod`: `run_type`
- whether the combined data set is to be written into S3: `write`
- whether parameters should be wiped from the workspace at the end of the pipelin run: `wipe`
- the minimum date from which the combined data is filtered to start: `earliest_date`
- where in S3 the combined data should be written: `output_bucket`
- each data source that may be loaded, including `total_beds`: `sources`, including whether they are included in this pipeline (`include`), and if the data source is from S3, the `path, path_type, pattern`



Each loading function `load_{data source}.R` should:

- take the inputs: `path, path_type, pattern` (which may be empty if the data is loaded from redshift), and `lookups`
- add a column with the path the data is from if applicable `prov_{data source}`
- convert the data to Trust-level geography
- have the option for loading dynamic files, if appropriate for that source.
