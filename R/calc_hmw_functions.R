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


fill_ilo_data <- function(.df,
                          country_code_col = MWTools::conc_cols$country_code_col,
                          sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
                          sector_col = MWTools::mw_constants$sector_col,
                          year = MWTools::mw_constants$year,
                          yearly_working_hours_ilo_col = MWTools::ilo_cols$yearly_working_hours_ilo_col,
                          employed_persons_ilo_col = MWTools::ilo_cols$employed_persons_ilo_col){

  # Fills data for each Country, Sex, and Sector based on earliest year
  .df %>%
    dplyr::group_by(country_code_col, sex_ilo_col, sector_col) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    tidyr::fill(employed_persons_ilo_col, .direction = "up") %>%
    tidyr::fill(yearly_working_hours_ilo_col, .direction = "up") %>%
    dplyr::ungroup()

}

# # Filter for GBR to check data has been filled
# GBR_filled <- ilo_data_filled %>%
#   dplyr::filter(Country.code == "GBR")
#
# # Calculates total number of hours worked per year
# ilo_data_total.hours <- ilo_data_filled %>%
#   dplyr::mutate("Total.hours [hours/year]" = `Employed.persons [persons]` * `Working.hours [hours/year]`)
#
# # Identify all unique sectors
# all_sectors <-unique(ilo_data$Sector) %>%
#   as.data.frame()
#
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
