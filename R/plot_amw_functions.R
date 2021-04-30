#' Create a summary plot for animal muscle work
#'
#' Create a composite plot summarising animal muscle work for the selected sector and
#' country. The composite plot contains two sub-plots: 1) the primary, final, and useful
#' energy associated with animal muscle work by species, over time; and 2) the
#' number of live, and working animals by species, over time.
#'
#' @param amw_pfu_df A data frame containing the primary, final, and useful energy
#'                   associated with animal muscle work.Usually supplied by calling
#'                   the function `calc_amw_numbers`.
#' @param amw_numbers_df A data frame containing the total number of live animals,
#'                       and working animals by sector. Usually supplied by calling
#'                       the function `calc_amw_pfu`.
#' @param country A 3 letter ISO code representing the country desired for analysis.
#' @param sector The sector desired for analysis. One of "Total", "Agriculture, or
#'               "Transport".
#' @param year,country_code_col,species,sector_col,stage_col,energy_mj_year See `mw_constants`.
#' @param working_animals_col,live_animals_col See `amw_analysis_constants`.
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
    ggplot2::geom_line(data = amw_pfu_plot_data, mapping = ggplot2::aes(x = .data[[year]],
                                                                        y = .data[[energy_mj_year]],
                                                                        color = stringr::str_wrap(.data[[stage_col]], width = 10))) +
    ggplot2::facet_wrap(facets = species, scales = "free_y") +
    ggplot2::labs(color = "Stage") +
    ggplot2::labs(title = as.character(country), subtitle = paste0(as.character(sector), " - Working Animal Energy")) +
    ggplot2::scale_y_continuous(labels = scales::label_scientific(digits = 1, scale = 1)) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.key.size = grid::unit(0.1, "cm"))

  numbers_plot <- ggplot2::ggplot() +
    ggplot2::geom_line(data = amw_numbers_plot_data, mapping = ggplot2::aes(x = .data[[year]],
                                                                            y = .data[[working_animals_col]],
                                                                            color = stringr::str_wrap(paste0(as.character(sector), " Working"), width = 10))) +
    ggplot2::geom_line(data = amw_numbers_plot_data, mapping = ggplot2::aes(x = .data[[year]],
                                                                            y = .data[[live_animals_col]],
                                                                            color = "Total Live")) +
    ggplot2::facet_wrap(facets = species, scales = "free_y") +
    ggplot2::labs(y = "Number of Animals", color = "Metric") +
    ggplot2::labs(title = as.character(country), subtitle = paste0(as.character(sector), " - Number of Animals")) +
    ggplot2::scale_y_continuous(labels = scales::label_scientific(digits = 1, scale = 1)) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.key.size = grid::unit(0.1, "cm"))

  cowplot::plot_grid(pfu_plot, numbers_plot, ncol = 1)

}
