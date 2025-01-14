# TODO: this is a work in progress but should help zero in on any issues.
# Not so useful for flu perhaps as we have not removed NAs from the hospitals data
# Note: the disbanded since Apr 2023 trust Yeovil district RA4 is only trust with NA population
# Sys.setenv("R_BOX_PATH" = fs::path(rprojroot::find_root(rprojroot::is_git_root), "src", "R"))
box::use(prj / user_check)

NAchecker <- function(combined_data,
                      print_rows = 10) {
  # Prints a selection of rows containing NAs (number of rows = print_rows)
  # and names of columns containing NAs

  rows_with_NA <- combined_data[!complete.cases(combined_data), ]

  if (nrow(rows_with_NA) > 0) {
    print(noquote(paste(ifelse(nrow(rows_with_NA) < print_rows, "All", "Sample of"),
      "rows containing NAs:")))
    print(rows_with_NA %>%
      dplyr::select("date", "nhs_region_name", "trust_name", "trust_code") %>%
      dplyr::sample_n(size = min(nrow(rows_with_NA), print_rows)) %>%
      dplyr::arrange(desc(date)))

    missingness <- colnames(rows_with_NA)[apply(rows_with_NA, 2, anyNA)]
    cat(noquote(paste("\nColumns with NAs in:\n",
      paste(missingness, collapse = ", "))))
    user_check$user_check()
  }
}
