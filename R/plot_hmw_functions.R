#' Create a summary plot for human muscle work
#'
#' Create a composite plot summarising human muscle work for the selected sector and country.
#' The composite plot contains two sub-plots: 1) the primary, final, and useful
#' energy associated with human muscle work by sex, over time; and 2) the
#' number of humans, and working humans by sex, over time.
#'
#' @param hmw_pfu_df
#' @param country A 3 letter ISO code representing the country desired for analysis.
#' @param sector The sector desired for analysis. One of "Agriculture", "Industry", or
#'               "Services".
#' @param year,country_code_col,sector_col,stage_col,energy_mj_year See `mw_constants`.
#' @param sex_ilo_col,employed_persons_ilo_col See `ilo_cols`.
#'
#' @return
#' @export
#'
#' @examples
plot_hmw_summary <- function(hmw_pfu_df,
                             country,
                             sector,
                             year = MWTools::mw_constants$year,
                             country_code_col = MWTools::conc_cols$country_code_col,
                             stage_col = MWTools::mw_constants$stage_col,
                             sector_col = MWTools::mw_constants$sector_col,
                             energy_mj_year = MWTools::mw_constants$energy_mj_year,
                             sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
                             employed_persons_ilo_col = MWTools::ilo_cols$employed_persons_ilo_col) {


  hmw_pfu_plot_data <- hmw_pfu_df %>%
    dplyr::filter(.data[[country_code_col]] == country) %>%
    dplyr::filter(.data[[sector_col]] == sector) %>%
    as.data.frame()

  pfu_plot <- ggplot2::ggplot() +
    ggplot2::geom_line(data = hmw_pfu_plot_data, mapping = ggplot2::aes(x = .data[[year]],
                                                                        y = .data[[energy_mj_year]],
                                                                        color = stringr::str_wrap(.data[[stage_col]], width = 10))) +
    ggplot2::facet_wrap(facets = sex_ilo_col, scales = "free_y") +
    ggplot2::labs(color = "Stage") +
    ggplot2::labs(title = as.character(country), subtitle = paste0(as.character(sector), " - Working Human Energy")) +
    ggplot2::scale_y_continuous(labels = scales::label_scientific(digits = 1, scale = 1)) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.key.size = grid::unit(0.1, "cm"))

  numbers_plot <- ggplot2::ggplot() +
    ggplot2::geom_line(data = hmw_pfu_plot_data, mapping = ggplot2::aes(x = .data[[year]],
                                                                        y = .data[[employed_persons_ilo_col]],
                                                                        color = stringr::str_wrap(paste0(as.character(sector), " Working"), width = 10))) +
    ggplot2::facet_wrap(facets = sex_ilo_col, scales = "free_y") +
    ggplot2::labs(y = "Number of Humans", color = "Metric") +
    ggplot2::labs(title = as.character(country), subtitle = paste0(as.character(sector), " - Number of Humans")) +
    ggplot2::scale_y_continuous(labels = scales::label_scientific(digits = 1, scale = 1)) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.key.size = grid::unit(0.1, "cm"))

  cowplot::plot_grid(pfu_plot, numbers_plot, ncol = 1)

}
