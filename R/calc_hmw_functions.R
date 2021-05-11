#' Add the regional codes used on analysis of human muscle work.
#'
#' ...
#'
#' @param .df
#' @param concordance_path The path to the country code concordance information.
#'                         Set to the bundled information by default,
#'                         retrieved using the `fao_concordance_path` function.
#' @param country_code_col,hmw_region_code_col See `MWTools::conc_cols`.
#'
#' @return
#' @export
#'
#' @examples
add_hmw_region_codes <- function(.df,
                                 concordance_path = MWTools::fao_concordance_path(),
                                 country_code_col = MWTools::conc_cols$country_code_col,
                                 hmw_region_code_col = MWTools::conc_cols$hmw_region_code_col,
                                 sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
                                 sector_col = MWTools::mw_constants$sector_col,
                                 year = MWTools::mw_constants$year,
                                 yearly_working_hours_ilo_col = MWTools::ilo_cols$yearly_working_hours_ilo_col,
                                 employed_persons_ilo_col = MWTools::ilo_cols$employed_persons_ilo_col){

  hmw_region_codes <- readxl::read_xlsx(path = concordance_path,
                                        sheet = "Mapping") %>%
    dplyr::select(Country.code_ISO3, hmw_region_code_col) %>%
    magrittr::set_colnames(c(country_code_col, hmw_region_code_col))

  .df %>%
    dplyr::left_join(hmw_region_codes, by = country_code_col) %>%
    dplyr::relocate(hmw_region_code_col, .after = country_code_col) %>%
    magrittr::set_colnames(c(country_code_col, hmw_region_code_col, sex_ilo_col,
                             sector_col, year, employed_persons_ilo_col,
                             yearly_working_hours_ilo_col))


}


#' Fill missing data for the number of hours worked and employed persons based on the earliest value
#'
#'
#'
#' @param .df
#' @param country_code_col
#' @param sex_ilo_col
#' @param sector_col
#' @param year
#' @param yearly_working_hours_ilo_col
#' @param employed_persons_ilo_col
#'
#' @return
#' @export
#'
#' @examples
fill_ilo_data <- function(.df,
                          country_code_col = MWTools::conc_cols$country_code_col,
                          sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
                          sector_col = MWTools::mw_constants$sector_col,
                          year = MWTools::mw_constants$year,
                          yearly_working_hours_ilo_col = MWTools::ilo_cols$yearly_working_hours_ilo_col,
                          employed_persons_ilo_col = MWTools::ilo_cols$employed_persons_ilo_col){

  # Fills data for each Country, Sex, and Sector based on earliest year
  .df %>%
    dplyr::group_by(.data[[country_code_col]], .data[[sex_ilo_col]], .data[[sector_col]]) %>%
    dplyr::arrange(.data[[year]], .by_group = TRUE) %>%
    tidyr::fill(.data[[employed_persons_ilo_col]], .direction = "up") %>%
    tidyr::fill(.data[[yearly_working_hours_ilo_col]], .direction = "up") %>%
    dplyr::ungroup()

}


#' Calculate the total number of hours worked each year
#'
#' Calc...
#'
#' @param .df
#' @param yearly_working_hours_ilo_col,employed_persons_ilo_col See `MWTools::ilo_cols`.
#' @param total_working_hours_ilo_col See `MWTools::hmw_analysis_constants`
#'
#' @return
#' @export
#'
#' @examples
calc_total_hours_worked <- function(.df,
                                     yearly_working_hours_ilo_col = MWTools::ilo_cols$yearly_working_hours_ilo_col,
                                     employed_persons_ilo_col = MWTools::ilo_cols$employed_persons_ilo_col,
                                     total_working_hours_ilo_col = MWTools::hmw_analysis_constants$total_working_hours_ilo_col){

  .df %>%
    dplyr::mutate(
      "{total_working_hours_ilo_col}" := .data[[employed_persons_ilo_col]] * .data[[yearly_working_hours_ilo_col]]
    )

}





# # Filters data to only include sector data by ISIC-Rev.4
# ilo_data_total.hours_isic <- ilo_data_total.hours %>%
#   dplyr::filter(stringr::str_detect(Sector, pattern = fixed("(ISIC-Rev.4):"))) %>%
#   dplyr::mutate(Sector = stringr::str_replace(Sector, ".*?\\:\\s", ""))
#
# # Filters data to only include sector data by aggregate
# ilo_data_total.hours_agg <- ilo_data_total.hours %>%
#   dplyr::filter(stringr::str_detect(Sector, pattern = fixed("(Aggregate):"))) %>%
#   dplyr::mutate(Sector = stringr::str_replace(Sector, ".*?\\:\\s", ""))
#
# # Filters data to only include sector data by broad
# ilo_data_total.hours_broad <- ilo_data_total.hours %>%
#   dplyr::filter(stringr::str_detect(Sector, pattern = fixed("(Broad sector):"))) %>%
#   dplyr::mutate(Sector = stringr::str_replace(Sector, ".*?\\:\\s", ""))
