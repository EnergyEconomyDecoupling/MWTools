data <- ilo_data_total.hours_broad %>%
  dplyr::filter(Country.name == "...")


plot <- ggplot2::ggplot() +
  ggplot2::geom_line(data = data, mapping = aes(x = Year, y = `Total.hours [hours/year]`, group = 1)) +
  ggplot2::facet_grid(rows = vars(Sector), cols = vars(Sex), scales = "free_y") +
  ggplot2::scale_x_discrete(breaks = seq(1950, 2020, by = 10)) +
  ggplot2::theme(panel.spacing = unit(0.65))

plot
