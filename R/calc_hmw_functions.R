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


#' Retrieve data with sectors organised by "Broad sector"
#'
#'
#'
#' @param .df
#'
#' @return
#' @export
#'
#' @examples
#'
get_broad.sector_data <- function(.df){

  .df %>%
    dplyr::filter(stringr::str_detect(Sector, pattern = stringr::fixed("(Broad sector):"))) %>%
    dplyr::mutate(Sector = stringr::str_replace(Sector, ".*?\\:\\s", ""))

}


#' Retrieve data with sectors organised by "Aggregate"
#'
#'
#'
#' @param .df
#'
#' @return
#' @export
#'
#' @examples
#'
get_aggregate_data <- function(.df){

  .df %>%
    dplyr::filter(stringr::str_detect(Sector, pattern = stringr::fixed("(Aggregate):"))) %>%
    dplyr::mutate(Sector = stringr::str_replace(Sector, ".*?\\:\\s", ""))

}


#' Calculate the final energy consumed by human workers
#'
#' ...
#'
#' @param .df
#' @param hmw_analysis_data_path See `MWTools::hmw_analysis_data_path()`.
#' @param sector_col,year,unit See `MWTools::mw_constants`.
#' @param hmw_analysis_sector_col,hmw_sector_map_sheet,hmw_food_sheet,agriculture,industry,services,food_consumption_col See `MWTools::hmw_analysis_constants`.
#' @param sex_ilo_col See `MWTools::ilo_cols`
#' @param hmw_region_code_col See `MWTools::conc_cols`.
#'
#' @return
#' @export
#'
#' @examples
calc_hmw_final_energy <- function(.df,
                                  hmw_analysis_data_path = MWTools::hmw_analysis_data_path(),
                                  sector_col = MWTools::mw_constants$sector_col,
                                  year = MWTools::mw_constants$year,
                                  unit = MWTools::mw_constants$unit,
                                  sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
                                  hmw_analysis_sector_col = MWTools::hmw_analysis_constants$hmw_analysis_sector_col,
                                  hmw_sector_map_sheet = MWTools::hmw_analysis_constants$hmw_sector_map_sheet,
                                  hmw_food_sheet = MWTools::hmw_analysis_constants$hmw_food_sheet,
                                  agriculture = MWTools::hmw_analysis_constants$agriculture_broad.sector,
                                  industry = MWTools::hmw_analysis_constants$industry_broad.sector,
                                  services = MWTools::hmw_analysis_constants$services_broad.sector,
                                  food_consumption_col = MWTools::hmw_analysis_constants$food_consumption_col,
                                  hmw_region_code_col = MWTools::conc_cols$hmw_region_code_col){

  # Reads sheet in hmw_analysis_data which maps ILO broad sectors to sectors used in the analysis
  sector_mapping_data <- readxl::read_xlsx(path = hmw_analysis_data_path,
                                           sheet = hmw_sector_map_sheet) %>%
    magrittr::set_colnames(c(sector_col, hmw_analysis_sector_col))

  # Reads food consumption data
  food_data <- readxl::read_xlsx(path = hmw_analysis_data_path,
                                 sheet = hmw_food_sheet) %>%
    dplyr::select(-.data[[unit]]) %>%
    tidyr::pivot_longer(cols = "1960":"2020",
                        names_to = year,
                        values_to = food_consumption_col) %>%
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]])
    ) %>%
    magrittr::set_colnames(c(sex_ilo_col, hmw_analysis_sector_col, hmw_region_code_col, year, food_consumption_col))

  # Removes leading string "Sex: " from Sex data
  .df %>%
    dplyr::mutate(
      "{sex_ilo_col}" := stringr::str_replace(.data[[sex_ilo_col]], stringr::fixed("Sex: "), "")
      ) %>%

    # Filters a data frame containing ILO data to only include agriculture, industry, and services.
    dplyr::filter(.data[[sector_col]] %in% c(agriculture, industry, services)) %>%

    # Adds hmw analysis sectors
    dplyr::left_join(sector_mapping_data, by = sector_col) %>%
    dplyr::relocate(hmw_analysis_sector_col, .after = sector_col) %>%

    # Adds daily food consumption
    dplyr::left_join(food_data, by = c(sex_ilo_col, hmw_analysis_sector_col, hmw_region_code_col, year))


}


# calc_hmw_useful_energy <- function(.df){
#
#
#
# }





