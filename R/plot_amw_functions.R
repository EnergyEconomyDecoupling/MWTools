#' Title
#'
#' @param amw_pfu_df
#' @param amw_numbers_df
#' @param country
#' @param sector
#' @param year
#' @param country_code_col
#' @param species
#' @param sector_col
#' @param stage_col
#' @param energy_mj_year
#' @param working_animals_col
#' @param live_animals_col
#'
#' @return
#' @export
#'
#' @examples
plot_amw_summary <- function(amw_pfu_df,
                             amw_numbers_df,
                             country,
                             sector,
                             year = MWTools::mw_constants$year,
                             country_code_col = MWTools::conc_cols$country_code_col,
                             species = MWTools::mw_constants$species,
                             sector_col = MWTools::mw_constants$sector_col,
                             stage_col = MWTools::mw_constants$stage_col,
                             energy_mj_year = MWTools::mw_constants$energy_mj_year,
                             working_animals_col = MWTools::amw_analysis_constants$working_animals_col,
                             live_animals_col = MWTools::amw_analysis_constants$live_animals_col
                             ){


  amw_pfu_plot_data <- amw_pfu_df %>%
    dplyr::filter(.data[[country_code_col]] == country,
                  .data[[sector_col]] == sector) %>%
    as.data.frame()

  amw_numbers_plot_data <- amw_numbers_df %>%
    dplyr::filter(.data[[country_code_col]] == country,
                  .data[[sector_col]] == sector) %>%
    as.data.frame()


  pfu_plot <- ggplot2::ggplot() +
    ggplot2::geom_line(data = amw_pfu_plot_data, mapping = ggplot2::aes(x = .data[[year]], y = .data[[energy_mj_year]], color = .data[[stage_col]])) +
    ggplot2::facet_wrap(facets = species, scales = "free_y") +
    ggplot2::labs(color = "ECC Stage") +
    ggplot2::labs(title = as.character(country), subtitle = paste0(as.character(sector), " Working Animal Energy"))

  numbers_plot <- ggplot2::ggplot() +
    ggplot2::geom_line(data = amw_numbers_plot_data, ggplot2::aes(x = .data[[year]], y = .data[[working_animals_col]], color = paste0(as.character(sector), " Working"))) +
    ggplot2::geom_line(data = amw_numbers_plot_data, ggplot2::aes(x = .data[[year]], y = .data[[live_animals_col]], color = "Total Live")) +
    ggplot2::facet_wrap(facets = species, scales = "free_y") +
    ggplot2::labs(y = "Number of Animals", color = "Metric") +
    ggplot2::labs(title = as.character(country), subtitle = paste0(as.character(sector), " Number of Animals"))

  cowplot::plot_grid(pfu_plot, numbers_plot, ncol = 1)

}
