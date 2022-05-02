#' Add the regional codes used on analysis of human muscle work.
#'
#' ...
#'
#' @param .df
#' @param concordance_path The path to the country code concordance information.
#'                         Set to the bundled information by default,
#'                         retrieved using the `fao_concordance_path` function.
#' @param country_col,hmw_region_code_col See `MWTools::conc_cols`.
#'
#' @return
#' @export
#'
#' @examples
add_hmw_region_codes <- function(.df,
                                 concordance_path = MWTools::fao_concordance_path(),
                                 country_col = MWTools::conc_cols$country_col,
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
    magrittr::set_colnames(c(country_col, hmw_region_code_col, sex_ilo_col,
                             sector_col, year, employed_persons_ilo_col,
                             yearly_working_hours_ilo_col))


}


#' Fill missing data for the number of hours worked and employed persons based on the earliest value
#'
#'
#'
#' @param .df
#' @param country_col
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
                          country_col = MWTools::conc_cols$country_col,
                          sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
                          sector_col = MWTools::mw_constants$sector_col,
                          year = MWTools::mw_constants$year,
                          yearly_working_hours_ilo_col = MWTools::ilo_cols$yearly_working_hours_ilo_col,
                          employed_persons_ilo_col = MWTools::ilo_cols$employed_persons_ilo_col){

  # Fills data for each Country, Sex, and Sector based on earliest year
  .df %>%
    dplyr::group_by(.data[[country_col]], .data[[sex_ilo_col]], .data[[sector_col]]) %>%
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
    ) %>%
    dplyr::select(-.data[[yearly_working_hours_ilo_col]])

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
get_broad.sector_data <- function(.df,
                                  sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
                                  sector_col = MWTools::mw_constants$sector_col){

  .df %>%
    dplyr::filter(stringr::str_detect(.data[[sector_col]], pattern = stringr::fixed("(Broad sector):"))) %>%
    dplyr::mutate(
      "{sector_col}" := stringr::str_replace(.data[[sector_col]], ".*?\\:\\s", "")
      ) %>%
    dplyr::filter(.data[[sex_ilo_col]] != "Total")

}


#' Add the human muscle work sectors used in the bundled analysis data
#'
#' ...
#'
#' @param .df
#' @param sector_col See `MWTools::mw_constants`.
#' @param hmw_analysis_data_path See `MWTools::hmw_analysis_data_path()`.
#' @param hmw_sector_map_sheet,hmw_analysis_sector_col,agriculture,industry,services See `MWTools::hmw_analysis_constants`.
#'
#' @return
#' @export
#'
#' @examples
#'
add_hmw_analysis_sectors <- function(.df,
                                     hmw_analysis_data_path = MWTools::hmw_analysis_data_path(),
                                     sector_col = MWTools::mw_constants$sector_col,
                                     hmw_sector_map_sheet = MWTools::hmw_analysis_constants$hmw_sector_map_sheet,
                                     hmw_analysis_sector_col = MWTools::hmw_analysis_constants$hmw_analysis_sector_col,
                                     agriculture = MWTools::hmw_analysis_constants$agriculture_broad.sector,
                                     industry = MWTools::hmw_analysis_constants$industry_broad.sector,
                                     services = MWTools::hmw_analysis_constants$services_broad.sector){

  # Reads sheet in hmw_analysis_data which maps ILO broad sectors to sectors used in the analysis
  sector_mapping_data <- readxl::read_xlsx(path = hmw_analysis_data_path,
                                           sheet = hmw_sector_map_sheet) %>%
    magrittr::set_colnames(c(sector_col, hmw_analysis_sector_col))

  # Adds hmw analysis sectors to data frame
  .df %>%
    dplyr::filter(.data[[sector_col]] %in% c(agriculture, industry, services)) %>%
    dplyr::left_join(sector_mapping_data, by = sector_col) %>%
    dplyr::relocate(hmw_analysis_sector_col, .after = sector_col)

}

#' Calculate the final energy consumed by human workers
#'
#' ...
#'
#' @param .df
#' @param hmw_analysis_data_path See `MWTools::hmw_analysis_data_path()`.
#' @param sector_col,year,unit See `MWTools::mw_constants`.
#' @param hmw_analysis_sector_col,hmw_food_sheet,food_consumption_col See `MWTools::hmw_analysis_constants`.
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
                                  exemplar_method_col = MWTools::mw_constants$exemplar_method_col,
                                  sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
                                  hmw_analysis_sector_col = MWTools::hmw_analysis_constants$hmw_analysis_sector_col,
                                  hmw_food_sheet = MWTools::hmw_analysis_constants$hmw_food_sheet,
                                  hmw_plate_waste_sheet = MWTools::hmw_analysis_constants$hmw_plate_waste_sheet,
                                  food_consumption_col = MWTools::hmw_analysis_constants$food_consumption_col,
                                  yearly_energy_consumption_pp_col = MWTools::hmw_analysis_constants$yearly_energy_consumption_pp_col,
                                  final_energy_col = MWTools::hmw_analysis_constants$final_energy_col,
                                  plate_waste_col = MWTools::hmw_analysis_constants$plate_waste_col,
                                  hmw_region_code_col = MWTools::conc_cols$hmw_region_code_col,
                                  kcal_to_mj = MWTools::unit_constants$kcal_to_mj,
                                  employed_persons_ilo_col = MWTools::ilo_cols$employed_persons_ilo_col){


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

  # Reads plate waste data
  plate_waste_data <- readxl::read_xlsx(path = hmw_analysis_data_path,
                                        sheet = hmw_plate_waste_sheet) %>%
    dplyr::select(-.data[[unit]], -.data[[exemplar_method_col]]) %>%
    tidyr::pivot_longer(cols = "1960":"2020",
                        names_to = year,
                        values_to = plate_waste_col) %>%
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]])
    )

  # Adds daily food consumption
  .df %>%
    dplyr::left_join(food_data, by = c(sex_ilo_col, hmw_analysis_sector_col, hmw_region_code_col, year)) %>%

    # Add plate waste
    dplyr::left_join(plate_waste_data, by = c(year, hmw_region_code_col)) %>%

    # Convert from kcal/day to MJ/year !!! Currently assuming every day worked - need to check ILO data for number of days worked !!!
    dplyr::mutate(
      "{yearly_energy_consumption_pp_col}" := .data[[food_consumption_col]] * kcal_to_mj * 365,
      "{final_energy_col}" := (.data[[employed_persons_ilo_col]] * .data[[yearly_energy_consumption_pp_col]]) / (1 - .data[[plate_waste_col]])
    ) %>%
    dplyr::select(-.data[[yearly_energy_consumption_pp_col]],-.data[[food_consumption_col]], -.data[[plate_waste_col]])

}


#' Title
#'
#' @param .df
#' @param hmw_analysis_data_path
#' @param year
#' @param unit
#' @param exemplar_method_col
#' @param hmw_harvest_waste_sheet
#' @param final_energy_col
#' @param primary_energy_col
#' @param hmw_harvest_waste_col
#'
#' @return
#' @export
#'
#' @examples
calc_hmw_primary_energy <- function(.df,
                                    hmw_analysis_data_path = MWTools::hmw_analysis_data_path(),
                                    year = MWTools::mw_constants$year,
                                    unit = MWTools::mw_constants$unit,
                                    exemplar_method_col = MWTools::mw_constants$exemplar_method_col,
                                    hmw_harvest_waste_sheet = MWTools::hmw_analysis_constants$hmw_harvest_waste_sheet,
                                    final_energy_col = MWTools::hmw_analysis_constants$final_energy_col,
                                    primary_energy_col = MWTools::hmw_analysis_constants$primary_energy_col,
                                    hmw_harvest_waste_col = MWTools::hmw_analysis_constants$hmw_harvest_waste_col,
                                    hmw_region_code_col = MWTools::conc_cols$hmw_region_code_col){

  # Read harvest waste data
  harvest_waste_data <- readxl::read_xlsx(path = hmw_analysis_data_path,
                                        sheet = hmw_harvest_waste_sheet) %>%
    dplyr::select(-.data[[unit]], -.data[[exemplar_method_col]]) %>%
    tidyr::pivot_longer(cols = "1960":"2020",
                        names_to = year,
                        values_to = hmw_harvest_waste_col) %>%
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]])
    )

  # Add harvest waste data and calculate primary energy
  .df %>%
    dplyr::left_join(harvest_waste_data, by = c(year, hmw_region_code_col)) %>%
    dplyr::mutate(
      "{primary_energy_col}" := .data[[final_energy_col]] / (1 - .data[[hmw_harvest_waste_col]])
    ) %>%
    dplyr::select(-.data[[hmw_harvest_waste_col]])


}



#' Title
#'
#' @param .df
#'
#' @return
#' @export
#'
#' @examples
calc_hmw_useful_energy <- function(.df,
                                   hmw_analysis_data_path = MWTools::hmw_analysis_data_path(),
                                   sector_col = MWTools::mw_constants$sector_col,
                                   year = MWTools::mw_constants$year,
                                   unit = MWTools::mw_constants$unit,
                                   sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
                                   hmw_power_sheet = MWTools::hmw_analysis_constants$hmw_power_sheet,
                                   hmw_analysis_sector_col = MWTools::hmw_analysis_constants$hmw_analysis_sector_col,
                                   power_col = MWTools::hmw_analysis_constants$power_col,
                                   hmw_region_code_col = MWTools::conc_cols$hmw_region_code_col,
                                   total_working_hours_ilo_col = MWTools::hmw_analysis_constants$total_working_hours_ilo_col,
                                   useful_energy_hmw_col = MWTools::hmw_analysis_constants$useful_energy_hmw_col,
                                   hours_to_seconds = MWTools::unit_constants$hours_to_seconds,
                                   joules_to_megajoules = MWTools::unit_constants$joules_to_megajoules
                                   ){

  # Reads power data
  power_data <- readxl::read_xlsx(path = hmw_analysis_data_path,
                                  sheet = hmw_power_sheet) %>%
    dplyr::select(-.data[[unit]]) %>%
    tidyr::pivot_longer(cols = "1960":"2020",
                        names_to = year,
                        values_to = power_col) %>%
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]])
    ) %>%
    magrittr::set_colnames(c(sex_ilo_col, hmw_analysis_sector_col, hmw_region_code_col, year, power_col))

  # Add power data to data frame
  .df %>%
    dplyr::left_join(power_data, by = c(sex_ilo_col, hmw_analysis_sector_col, hmw_region_code_col, year)) %>%
    dplyr::mutate(
      "{useful_energy_hmw_col}" := .data[[power_col]] * .data[[total_working_hours_ilo_col]] * hours_to_seconds * joules_to_megajoules
    ) %>%
    dplyr::select(-.data[[power_col]])

}

#' Title
#'
#' @param .df
#' @param final_energy_col
#' @param primary_energy_col
#' @param useful_energy_hmw_col
#' @param hmw_analysis_sector_col
#' @param energy_mj_year
#' @param stage_col
#'
#' @return
#' @export
#'
#' @examples
tidy_hmw_pfu <- function(.df,
                         year = MWTools::mw_constants$year,
                         sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
                         sector_col = MWTools::mw_constants$sector_col,
                         species = MWTools::mw_constants$species,
                         country_col = MWTools::conc_cols$country_col,
                         hmw_region_code_col = MWTools::conc_cols$hmw_region_code_col,
                         final_energy_col = MWTools::hmw_analysis_constants$final_energy_col,
                         primary_energy_col = MWTools::hmw_analysis_constants$primary_energy_col,
                         useful_energy_hmw_col = MWTools::hmw_analysis_constants$useful_energy_hmw_col,
                         hmw_analysis_sector_col = MWTools::hmw_analysis_constants$hmw_analysis_sector_col,
                         energy_col = MWTools::mw_constants$energy_col,
                         stage_col = MWTools::mw_constants$stage_col,
                         units_col = MWTools::mw_constants$units_col){

  .df %>%
    tidyr::pivot_longer(cols = c(final_energy_col, primary_energy_col, useful_energy_hmw_col),
                        names_to = stage_col,
                        values_to = energy_col) %>%
    dplyr::select(country_col, year, sex_ilo_col,
                  stage_col, sector_col, energy_col) %>%
    dplyr::mutate(
      "{stage_col}" := stringr::str_replace(.data[[stage_col]], stringr::fixed(" energy [MJ/year]"), "")
    ) %>%
    dplyr::mutate(
      "{sex_ilo_col}" := dplyr::case_when(
        .data[[sex_ilo_col]] == "Male" ~ "Human Males",
        .data[[sex_ilo_col]] == "Female" ~ "Human Females",
        TRUE ~ "Unknown sector column value"
      )
    ) %>%
    dplyr::rename("Species" = sex_ilo_col) %>%
    dplyr::mutate("{energy_col}" := .data[[energy_col]] * 0.000000000001) %>%
    dplyr::mutate("{units_col}" := "EJ", .before = energy_col)
}



#' Title
#'
#' @param .df
#' @param final_energy_col
#' @param primary_energy_col
#' @param useful_energy_hmw_col
#' @param energy_mj_year
#' @param stage_col
#'
#' @return
#' @export
#'
#' @examples
calc_hmw_pfu <- function(.df,
                         concordance_path = MWTools::fao_concordance_path(),
                         hmw_analysis_data_path = MWTools::hmw_analysis_data_path()){

  .df %>%
    add_hmw_region_codes(concordance_path = concordance_path) %>%
    fill_ilo_data() %>%
    calc_total_hours_worked() %>%
    get_broad.sector_data() %>%
    add_hmw_analysis_sectors(hmw_analysis_data_path = hmw_analysis_data_path) %>%
    calc_hmw_final_energy(hmw_analysis_data_path = hmw_analysis_data_path) %>%
    calc_hmw_primary_energy(hmw_analysis_data_path = hmw_analysis_data_path) %>%
    calc_hmw_useful_energy(hmw_analysis_data_path = hmw_analysis_data_path) %>%
    tidy_hmw_pfu()

}




