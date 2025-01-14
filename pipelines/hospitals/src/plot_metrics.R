plot_metrics <- function(results, combined_data) {
  # Plots time series for the first 10 metrics for each data source by trust

  # check for NA regions
  regions <- unique(combined_data$nhs_region_name)
  if (any(is.na(regions))) {
    print(noquote("Trusts missing regional information:"))
    print(noquote(combined_data %>%
      dplyr::filter(is.na(nhs_region_name)) %>%
      dplyr::pull(trust_code) %>%
      unique()))
    user_check()
    regions <- regions[!is.na(regions)]
  }

  # plot metrics for each source
  for (source_name in names(results)) {
    print(glue::glue("Plotting {source_name}"))
    metrics <- results[[source_name]]$data %>%
      dplyr::ungroup() %>%
      dplyr::select(
        -matches(paste0("^", c("date", "trust_code", "trust_name", "icb_name", "nhs_region_name", "population"), "$")),
        -matches("^prov_")
      ) %>%
      names() %>%
      head(10)

    data_reshaped <- combined_data %>%
      dplyr::select(date, trust_name, nhs_region_name, all_of(metrics)) %>%
      tidyr::pivot_longer(cols = -c(date, trust_name, nhs_region_name)) %>%
      dplyr::mutate(trust_name = gsub(" NHS |FOUNDATION |TRUST", "", trust_name)) %>%
      dplyr::rename(metric = name)

    for (choose_region in regions) {
      p <- ggplot2::ggplot(data_reshaped %>%
        dplyr::filter(
          nhs_region_name == choose_region,
          date > max(date) - 30
        )) +
        ggplot2::geom_line(ggplot2::aes(x = date, y = value, color = metric)) +
        ggplot2::facet_wrap(~trust_name, labeller = ggplot2::label_wrap_gen(30)) +
        ggplot2::ggtitle(choose_region) +
        ggplot2::xlab("") +
        ggplot2::ylab("") +
        ggplot2::theme_bw()

      ggplot2::ggsave(
        filename = glue::glue("{plots_output_path}/{source_name}_{gsub(' ','_',choose_region)}.png"),
        plot = p,
        type = "cairo",
        width = 15, height = 10
      )
    }
  }

  # plot national level time-series of admissions

  # helper function for bimonthly date breaks
  bimonthly <- function(x) {
    x_range <- range(x, na.rm = TRUE)

    date_range <- c(
      lubridate::floor_date(x_range[1], "month"),
      lubridate::ceiling_date(x_range[2], "month")
    )
    monthly <- seq(date_range[1], date_range[2], by = "1 month")

    sort(c(monthly, monthly + lubridate::days(14)))
  }

  print(noquote("Plotting national hospital admissions"))

  if (disease_choice == "covid") {
    admissions_plot <- combined_data %>%
      dplyr::filter(date >= as.Date("2023-04-30")) %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(admissions = sum(admissions, na.rm = TRUE), .groups = "keep") %>%
      ggplot2::ggplot() +
      ggplot2::geom_col(ggplot2::aes(x = date, y = admissions), fill = "#3288BD", width = 1) +
      ggplot2::geom_line(ggplot2::aes(x = date, y = zoo::rollmean(admissions, 7, align = "right", fill = NA)), color = "#253494", size = 0.8) +
      ggplot2::scale_x_date(breaks = bimonthly, label = scales::label_date_short()) +
      ggplot2::labs(x = "Date", y = "Admissions") +
      ggplot2::theme_minimal()

    ggplot2::ggsave(
      filename = glue::glue("{plots_output_path}/national_admissions.png"),
      plot = admissions_plot,
      type = "cairo",
      width = 15, height = 10
    )

    # plot national level time-series of arrival admissions
    print(noquote("Plotting national hospital arrival admissions"))

    arrival_admissions_plot <- combined_data %>%
      dplyr::filter(date >= as.Date("2023-04-30")) %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(arrival_admissions = sum(arrival_admissions, na.rm = TRUE), .groups = "keep") %>%
      ggplot2::ggplot() +
      ggplot2::geom_col(ggplot2::aes(x = date, y = arrival_admissions), fill = "#3288BD", width = 1) +
      ggplot2::geom_line(ggplot2::aes(x = date, y = zoo::rollmean(arrival_admissions, 7, align = "right", fill = NA)), color = "#253494", size = 0.8) +
      ggplot2::scale_x_date(breaks = bimonthly, label = scales::label_date_short()) +
      ggplot2::labs(x = "Date", y = "Arrival admissions") +
      ggplot2::theme_minimal()

    ggplot2::ggsave(
      filename = glue::glue("{plots_output_path}/national_arrival_admissions.png"),
      plot = arrival_admissions_plot,
      type = "cairo",
      width = 15, height = 10
    )

    # plot admissions by age (stacked bar chart)
    print(noquote("Plotting national hospital admissions by age"))

    age_labs <- c("0-5", "6-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+")

    age_admissions_plot <- combined_data %>%
      dplyr::filter(date >= as.Date("2023-04-30")) %>%
      tidyr::pivot_longer(cols = starts_with("admissions_"), names_to = "age", values_to = "value") %>%
      dplyr::filter(age != "admissions_00_17" & age != "admissions_18_64" & age != "admissions_65_plus") %>%
      dplyr::group_by(date, age) %>%
      dplyr::summarise(admissions = sum(value, na.rm = TRUE), .groups = "keep") %>%
      dplyr::group_by(age) %>%
      dplyr::mutate(roll_admissions = zoo::rollmean(admissions, 7, fill = NA, align = "right")) %>%
      dplyr::ungroup() %>%
      ggplot2::ggplot() +
      ggplot2::geom_col(ggplot2::aes(x = date, y = roll_admissions, fill = factor(age, labels = age_labs)), position = "stack", width = 1) +
      ggplot2::scale_fill_brewer(palette = "Spectral") +
      ggplot2::scale_x_date(breaks = bimonthly, label = scales::label_date_short()) +
      ggplot2::labs(x = "Date", y = "Admissions", fill = "Age Band") +
      ggplot2::theme_minimal()

    ggplot2::ggsave(
      filename = glue::glue("{plots_output_path}/national_admissions_by_age.png"),
      plot = age_admissions_plot,
      type = "cairo",
      width = 15, height = 10
    )

    # plot admissions by age (percentage bars)
    print(noquote("Plotting percentage of national hospital admissions by age"))

    age_percentage_admissions_plot <- combined_data %>%
      dplyr::filter(date >= as.Date("2023-04-30")) %>%
      tidyr::pivot_longer(cols = starts_with("admissions_"), names_to = "age", values_to = "value") %>%
      dplyr::filter(age != "admissions_00_17" & age != "admissions_18_64" & age != "admissions_65_plus") %>%
      dplyr::group_by(date, age) %>%
      dplyr::summarise(admissions = sum(value, na.rm = TRUE), .groups = "keep") %>%
      dplyr::group_by(age) %>%
      dplyr::mutate(roll_admissions = zoo::rollmean(admissions, 7, fill = NA, align = "right")) %>%
      dplyr::ungroup() %>%
      ggplot2::ggplot() +
      ggplot2::geom_col(ggplot2::aes(x = date, y = roll_admissions, fill = factor(age, labels = age_labs)), position = "fill", width = 1) +
      ggplot2::scale_fill_brewer(palette = "Spectral") +
      ggplot2::scale_x_date(breaks = bimonthly, label = scales::label_date_short()) +
      ggplot2::labs(x = "Date", y = "Admissions", fill = "Age Band") +
      ggplot2::theme_minimal()

    ggplot2::ggsave(
      filename = glue::glue("{plots_output_path}/national_admissions_by_age_percentage.png"),
      plot = age_percentage_admissions_plot,
      type = "cairo",
      width = 15, height = 10
    )
  }
}
