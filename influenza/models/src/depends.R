install.packages(setdiff("pacman", rownames(installed.packages())))
pacman::p_load(
  dplyr,
  parallel,
  glue,
  fs,
  rprojroot,
  yaml,
  lubridate,
  rlang,
  vroom,
  aws.s3,
  tidyr,
  stringr,
  mgcv,
  zoo,
  purrr,
  scales,
  doParallel,
  foreach,
  ggplot2
)

# # The box module `intervals` relies on the `gratia` package for one version of
# # posterior sampling. This requires at minimum `mgcv`==1.8-41 and `gratia`==0.8.1.35
# # as the function we need was implimented in this update.
# # You may need to uninstall mgcv and upgrade that before installing the newer gratia
# remotes::install_github("gavinsimpson/gratia")
# library(gratia)

box::use(
  box / redshift,
  box / s3,
  prj / checks,
  prj / run_model,
  prj / splines,
  prj / intervals,
  prj / parallel,
  prj / ensemble,
  prj / write,
  prj / extract_objects,
  prj / narratives,
  prj / projection_plots,
  prj / outputs,
  prj / scoring,
  prj / peaks,
  prj / user_check
)
