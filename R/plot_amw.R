# amw_plot <- function(amw_data, country, sector) {
#
#   plot_data <- amw_data %>%
#     dplyr::filter(ISO_Country_Code == country, sector == sector)
#
#   pfu_plot <- ggplot2::ggplot() +
#     ggplot2::geom_line(data = plot_data, mapping = aes(x = Year, y = `Energy [J]`, color = stage)) +
#     ggplot2::facet_wrap(facets = "Species", scales = "free_y") +
#     labs(color = "ECC Stage") +
#     labs(title = country, subtitle = sector)
#
#   wa_plot <- ggplot2::ggplot() +
#     ggplot2::geom_line(data = plot_data, aes(x = Year, y = Working_Animals, color = "Working")) +
#     ggplot2::geom_line(data = plot_data, aes(x = Year, y = Live_Animals, color = "Live")) +
#     ggplot2::facet_wrap(facets = "Species", scales = "free_y") +
#     labs(y = "Number of Animals", color = "Metric") +
#     labs(title = country, subtitle = sector)
#
#   cowplot::plot_grid(pfu_plot, wa_plot, ncol = 1)
#
# }

