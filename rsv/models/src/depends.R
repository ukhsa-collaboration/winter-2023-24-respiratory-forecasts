install.packages(setdiff("pacman", rownames(installed.packages())))
pacman::p_load(
  dplyr,
  stringr,
  vroom,
  janitor,
  ggnewscale
)

# # The box module `intervals` relies on the `gratia` package for one version of
# # posterior sampling. This requires at minimum `mgcv`==1.8-41 and `gratia`==0.8.1.35
# # as the function we need was implimented in this update.
# # You may need to uninstall mgcv and upgrade that before installing the newer gratia
remotes::install_github("gavinsimpson/gratia")
library(gratia)
