#' Add the regional codes used on analysis of human muscle work.
#'
#' ...
#'
#' @param .df The raw ILO data, retrieved from ILOSTAT.
#' @param concordance_path The path to the country code concordance information.
#'                         Set to the bundled information by default,
#'                         retrieved using the `fao_concordance_path` function.
#' @param country_col,country_code_col,hmw_region_code_col,mapping_sheet,country_code_iso3_col See `MWTools::conc_cols`.
#' @param sex_ilo_col,yearly_working_hours_ilo_col,employed_persons_ilo_col See `MWTools::ilo_cols`.
#' @param sector_col,year See `MWTools::mw_constants`.
#'
#' @export
#'
#' @examples
#' working_humans_data <- read.csv(file = MWTools::hmw_test_data_path()) %>%
#'   add_hmw_region_codes()
add_hmw_region_codes <- function(.df,
                                 concordance_path = MWTools::fao_concordance_path(),
                                 mapping_sheet = MWTools::conc_cols$mapping_sheet,
                                 country_col = MWTools::conc_cols$country_col,
                                 country_code_col = MWTools::conc_cols$country_code_col,
                                 hmw_region_code_col = MWTools::conc_cols$hmw_region_code_col,
                                 country_code_iso3_col = MWTools::conc_cols$country_code_iso3_col,
                                 sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
                                 sector_col = MWTools::mw_constants$sector_col,
                                 year = MWTools::mw_cols$year,
                                 yearly_working_hours_ilo_col = MWTools::ilo_cols$yearly_working_hours_ilo_col,
                                 employed_persons_ilo_col = MWTools::ilo_cols$employed_persons_ilo_col
                                 ){

  hmw_region_codes <- readxl::read_xlsx(path = concordance_path,
                                        sheet = mapping_sheet) %>%
    dplyr::select(dplyr::all_of(c(country_code_iso3_col, hmw_region_code_col))) %>%
    magrittr::set_colnames(c(country_code_col, hmw_region_code_col))

  .df %>%
    dplyr::left_join(hmw_region_codes, by = country_code_col) %>%
    dplyr::relocate(dplyr::all_of(hmw_region_code_col), .after = dplyr::all_of(country_code_col)) %>%
    magrittr::set_colnames(c(country_col, hmw_region_code_col, sex_ilo_col,
                             sector_col, year, employed_persons_ilo_col,
                             yearly_working_hours_ilo_col))


}


#' Fill missing data for the number of hours worked and employed persons based on the earliest value
#'
#'
#'
#' @param .df The ILO labor data with added region codes.
#'            Usually produced by calling the
#'            `add_hmw_region_codes` function in sequence on the raw FAO data.
#' @param country_col See `MWTools::conc_cols`.
#' @param sex_ilo_col,yearly_working_hours_ilo_col,employed_persons_ilo_col See `MWTools::ilo_cols`.
#' @param sector_col,year See `MWTools::mw_constants`.
#'
#' @export
#'
#' @examples
#' working_humans_data <- read.csv(file = MWTools::hmw_test_data_path()) %>%
#'   add_hmw_region_codes() %>%
#'   fill_ilo_data()
fill_ilo_data <- function(.df,
                          country_col = MWTools::conc_cols$country_col,
                          sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
                          sector_col = MWTools::mw_constants$sector_col,
                          year = MWTools::mw_cols$year,
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
#' @param .df The ILO labor data filled (held constant back to 1960 from the
#'            earliest year of data).
#'            Usually produced by calling the
#'            `add_hmw_region_codes`, and
#'            `fill_ilo_data` functions in sequence on the raw FAO data.
#' @param yearly_working_hours_ilo_col,employed_persons_ilo_col See `MWTools::ilo_cols`.
#' @param total_wk_hrs_ilo_col See `MWTools::hmw_analysis_constants`
#'
#'
#' @export
#'
#' @examples
#' working_hours_data <- read.csv(file = MWTools::hmw_test_data_path()) %>%
#'   add_hmw_region_codes() %>%
#'   fill_ilo_data() %>%
#'   calc_total_hours_worked()
calc_total_hours_worked <- function(.df,
                                     yearly_working_hours_ilo_col = MWTools::ilo_cols$yearly_working_hours_ilo_col,
                                     employed_persons_ilo_col = MWTools::ilo_cols$employed_persons_ilo_col,
                                     total_wk_hrs_ilo_col = MWTools::hmw_analysis_constants$total_wk_hrs_ilo_col){

  .df %>%
    dplyr::mutate(
      "{total_wk_hrs_ilo_col}" := .data[[employed_persons_ilo_col]] * .data[[yearly_working_hours_ilo_col]]
    ) %>%
    dplyr::select(-dplyr::all_of(yearly_working_hours_ilo_col))

}


#' Retrieve data with sectors organised by "Broad sector"
#'
#'
#'
#' @param .df A data frame containing the number of hours worked.
#'            Usually produced by calling the
#'            `add_hmw_region_codes`,
#'            `fill_ilo_data`, and
#'            `calc_total_hours_worked` functions in sequence on the raw FAO data.
#' @param sex_ilo_col See `MWTools::ilo_cols`.
#' @param sector_col See `mWTools::mw_constants`.
#'
#'
#' @export
#'
#' @examples
#' working_hours_sector_data <- read.csv(file = MWTools::hmw_test_data_path()) %>%
#'   add_hmw_region_codes() %>%
#'   fill_ilo_data() %>%
#'   calc_total_hours_worked() %>%
#'   get_broad.sector_data()
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

#' Title
#'
#' @param .df A data frame containing the number of hours worked by broad sector.
#'            Usually produced by calling the
#'            `add_hmw_region_codes`,
#'            `fill_ilo_data`,
#'            `calc_total_hours_worked`, and
#'            `get_broad.sector_data` functions in sequence on the raw FAO data.
#' @param hmw_analysis_data_path See `MWTools::hmw_analysis_data_path()`.
#' @param hmw_labor_map_sheet,labor_type_col,labor_split_col,total_wk_hrs_ilo_col See `MWTools::hmw_analysis_constants`.
#' @param unit,year See `MWTools::mw_cols`.
#' @param hmw_region_code_col See `MWTools::conc_cols`.
#' @param sex_ilo_col,employed_persons_ilo_col See `MWTools::ilo_cols`.
#' @param sector_col See `MWTools::mw_constants`.
#' @param agriculture,industry,services See `MWTools::mw_sectors`.
#'
#' @export
#'
#' @examples
#' working_hours_labor_type_data <- read.csv(file = MWTools::hmw_test_data_path()) %>%
#'   add_hmw_region_codes() %>%
#'   fill_ilo_data() %>%
#'   calc_total_hours_worked() %>%
#'   get_broad.sector_data() %>%
#'   split_labor_by_sector()
split_labor_by_sector <- function(.df,
                                  hmw_analysis_data_path = MWTools::hmw_analysis_data_path(),
                                  hmw_labor_map_sheet = MWTools::hmw_analysis_constants$hmw_labor_map_sheet,
                                  unit =  MWTools::mw_cols$unit,
                                  hmw_region_code_col = MWTools::conc_cols$hmw_region_code_col,
                                  sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
                                  sector_col = MWTools::mw_constants$sector_col,
                                  year = MWTools::mw_cols$year,
                                  labor_type_col = MWTools::hmw_analysis_constants$labor_type_col,
                                  labor_split_col = MWTools::hmw_analysis_constants$labor_split_col,
                                  employed_persons_ilo_col = MWTools::ilo_cols$employed_persons_ilo_col,
                                  total_wk_hrs_ilo_col = MWTools::hmw_analysis_constants$total_wk_hrs_ilo_col,
                                  agriculture = MWTools::mw_sectors$agriculture_broad.sector,
                                  industry = MWTools::mw_sectors$industry_broad.sector,
                                  services = MWTools::mw_sectors$services_broad.sector){

  sector_mapping_data <- readxl::read_xlsx(path = hmw_analysis_data_path,
                                           sheet = hmw_labor_map_sheet) %>%
    dplyr::select(-dplyr::all_of(c(unit))) %>%
    tidyr::pivot_longer(cols = -dplyr::all_of(c(sex_ilo_col,
                                                sector_col,
                                                labor_type_col,
                                                hmw_region_code_col)),
                        names_to = year,
                        values_to = labor_split_col) %>%
    dplyr::mutate("{year}" := as.numeric(.data[[year]]))

  .df %>%
    dplyr::filter(.data[[sector_col]] %in% c(agriculture,
                                             industry,
                                             services)) %>%
    dplyr::left_join(sector_mapping_data,
                     by = dplyr::all_of(c(hmw_region_code_col,
                                          sex_ilo_col,
                                          sector_col,
                                          year))) %>%
    dplyr::relocate(.data[[labor_type_col]], .after = dplyr::all_of(c(sector_col))) %>%
    dplyr::mutate("{employed_persons_ilo_col}" := .data[[employed_persons_ilo_col]] * .data[[labor_split_col]],
                  "{total_wk_hrs_ilo_col}" := .data[[total_wk_hrs_ilo_col]] * .data[[labor_split_col]]) %>%
    dplyr::select(-dplyr::all_of(c(labor_split_col)))



}

#' Calculate the final energy consumed by human workers
#'
#' ...
#'
#' @param .df A data frame containing the number of hours worked by sector and
#'            activity level.
#'            Usually produced by calling the
#'            `add_hmw_region_codes`,
#'            `fill_ilo_data`,
#'            `calc_total_hours_worked`,
#'            `get_broad.sector_data`, and
#'            `split_labor_by_sector` functions in sequence on the raw FAO data.
#' @param hmw_analysis_data_path See `MWTools::hmw_analysis_data_path()`.
#' @param sector_col,year,unit See `MWTools::mw_constants`.
#' @param hmw_food_sheet,hmw_plate_waste_sheet,food_consumption_col,energy_pppa_col,final_energy_col,plate_waste_col,labor_type_col See `MWTools::hmw_analysis_constants`.
#' @param sex_ilo_col,employed_persons_ilo_col See `MWTools::ilo_cols`
#' @param hmw_region_code_col See `MWTools::conc_cols`.
#' @param exemplar_method_col See `MWTools::mw_sectors`.
#' @param kcal_to_mj See `MWTools::unit_constants`.
#'
#'
#' @export
#'
#' @examples
#' final_energy_data <- read.csv(file = MWTools::hmw_test_data_path()) %>%
#'   add_hmw_region_codes() %>%
#'   fill_ilo_data() %>%
#'   calc_total_hours_worked() %>%
#'   get_broad.sector_data() %>%
#'   split_labor_by_sector() %>%
#'   calc_hmw_final_energy()
calc_hmw_final_energy <- function(
    .df,
    hmw_analysis_data_path = MWTools::hmw_analysis_data_path(),
    sector_col = MWTools::mw_constants$sector_col,
    year = MWTools::mw_cols$year,
    unit = MWTools::mw_cols$unit,
    exemplar_method_col = MWTools::mw_constants$exemplar_method_col,
    sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
    labor_type_col = MWTools::hmw_analysis_constants$labor_type_col,
    hmw_food_sheet = MWTools::hmw_analysis_constants$hmw_food_sheet,
    hmw_plate_waste_sheet = MWTools::hmw_analysis_constants$hmw_plate_waste_sheet,
    food_consumption_col = MWTools::hmw_analysis_constants$food_consumption_col,
    energy_pppa_col = MWTools::hmw_analysis_constants$energy_pppa_col,
    final_energy_col = MWTools::hmw_analysis_constants$final_energy_col,
    plate_waste_col = MWTools::hmw_analysis_constants$plate_waste_col,
    hmw_region_code_col = MWTools::conc_cols$hmw_region_code_col,
    kcal_to_mj = MWTools::unit_constants$kcal_to_mj,
    employed_persons_ilo_col = MWTools::ilo_cols$employed_persons_ilo_col){


  # Reads food consumption data
  food_data <- readxl::read_xlsx(path = hmw_analysis_data_path,
                                 sheet = hmw_food_sheet) %>%
    dplyr::select(-dplyr::all_of(unit)) %>%
    tidyr::pivot_longer(cols = -dplyr::all_of(c(sex_ilo_col, hmw_region_code_col, labor_type_col)),
                        names_to = year,
                        values_to = food_consumption_col) %>%
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]])
    ) %>%
    magrittr::set_colnames(c(sex_ilo_col, labor_type_col, hmw_region_code_col, year, food_consumption_col))

  # Reads plate waste data
  plate_waste_data <- readxl::read_xlsx(path = hmw_analysis_data_path,
                                        sheet = hmw_plate_waste_sheet) %>%
    dplyr::select(-dplyr::all_of(c(unit, exemplar_method_col))) %>%
    tidyr::pivot_longer(cols = -dplyr::all_of(c(hmw_region_code_col)),
                        names_to = year,
                        values_to = plate_waste_col) %>%
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]])
    )

  # Adds daily food consumption
  .df %>%
    dplyr::left_join(food_data, by = c(sex_ilo_col, labor_type_col, hmw_region_code_col, year)) %>%

    # Add plate waste
    dplyr::left_join(plate_waste_data, by = c(year, hmw_region_code_col)) %>%

    # Convert from kcal/day to MJ/year !!! Currently assuming every day worked - need to check ILO data for number of days worked !!!
    dplyr::mutate(
      "{energy_pppa_col}" := .data[[food_consumption_col]] * kcal_to_mj * 365,
      "{final_energy_col}" := (.data[[employed_persons_ilo_col]] * .data[[energy_pppa_col]]) / (1 - .data[[plate_waste_col]])
    ) %>%
    dplyr::select(-dplyr::all_of(c(energy_pppa_col, food_consumption_col, plate_waste_col)))

}


#' Calculate the primary energy consumed by human workers
#'
#' @param .df A data frame containing the final energy consumed by human
#'            workers.
#'            Usually produced by calling the
#'            `add_hmw_region_codes`,
#'            `fill_ilo_data`,
#'            `calc_total_hours_worked`,
#'            `get_broad.sector_data`,
#'            `split_labor_by_sector`, and
#'            `calc_hmw_final_energy` functions in sequence on the raw FAO data.
#' @param year,unit,exemplar_method_col See `MWTools::mw_constants`.
#' @param hmw_analysis_data_path,hmw_harvest_waste_sheet,final_energy_col,primary_energy_col,hmw_harvest_waste_col See `MWTools::hmw_analysis_constants`.
#' @param hmw_region_code_col See `MWTools::conc_cols`.
#'
#'
#' @export
#'
#' @examples
#' primary_energy_data <- read.csv(file = MWTools::hmw_test_data_path()) %>%
#'   add_hmw_region_codes() %>%
#'   fill_ilo_data() %>%
#'   calc_total_hours_worked() %>%
#'   get_broad.sector_data() %>%
#'   split_labor_by_sector() %>%
#'   calc_hmw_final_energy() %>%
#'   calc_hmw_primary_energy()
calc_hmw_primary_energy <- function(.df,
                                    year = MWTools::mw_cols$year,
                                    unit = MWTools::mw_cols$unit,
                                    exemplar_method_col = MWTools::mw_constants$exemplar_method_col,
                                    hmw_analysis_data_path = MWTools::hmw_analysis_data_path(),
                                    hmw_harvest_waste_sheet = MWTools::hmw_analysis_constants$hmw_harvest_waste_sheet,
                                    final_energy_col = MWTools::hmw_analysis_constants$final_energy_col,
                                    primary_energy_col = MWTools::hmw_analysis_constants$primary_energy_col,
                                    hmw_harvest_waste_col = MWTools::hmw_analysis_constants$hmw_harvest_waste_col,
                                    hmw_region_code_col = MWTools::conc_cols$hmw_region_code_col){

  # Read harvest waste data
  harvest_waste_data <- readxl::read_xlsx(path = hmw_analysis_data_path,
                                          sheet = hmw_harvest_waste_sheet) %>%
    dplyr::select(-dplyr::all_of(c(unit, exemplar_method_col))) %>%
    tidyr::pivot_longer(cols = -dplyr::all_of(c(hmw_region_code_col)),
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
    dplyr::select(-dplyr::all_of(hmw_harvest_waste_col))
}


#' Calculate the useful energy produced by human workers
#'
#' @param .df A data frame containing the final and useful energy consumed
#'            by human workers.
#'            Usually produced by calling the
#'            `add_hmw_region_codes`,
#'            `fill_ilo_data`,
#'            `calc_total_hours_worked`,
#'            `get_broad.sector_data`,
#'            `split_labor_by_sector`,
#'            `calc_hmw_final_energy`, and
#'            `calc_hmw_primary_energy` functions in sequence on the raw FAO data.
#' @param sector_col,year,unit See `MWTools::mw_constants`.
#' @param sex_ilo_col See `MWTools::ilo_cols`.
#' @param hmw_region_code_col See `MWTools::conc_cols`.
#' @param hmw_analysis_data_path,hmw_power_sheet,power_col,total_wk_hrs_ilo_col,useful_energy_hmw_col,hours_to_seconds,joules_to_megajoules,labor_type_col See `MWTools::hmw_analysis_constants`.
#'
#'
#' @export
#'
#' @examples
#' useful_energy_data <- read.csv(file = MWTools::hmw_test_data_path()) %>%
#'   add_hmw_region_codes() %>%
#'   fill_ilo_data() %>%
#'   calc_total_hours_worked() %>%
#'   get_broad.sector_data() %>%
#'   split_labor_by_sector() %>%
#'   calc_hmw_final_energy() %>%
#'   calc_hmw_primary_energy() %>%
#'   calc_hmw_useful_energy()
calc_hmw_useful_energy <- function(.df,
                                   sector_col = MWTools::mw_constants$sector_col,
                                   year = MWTools::mw_cols$year,
                                   unit = MWTools::mw_cols$unit,
                                   sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
                                   hmw_region_code_col = MWTools::conc_cols$hmw_region_code_col,
                                   hmw_analysis_data_path = MWTools::hmw_analysis_data_path(),
                                   hmw_power_sheet = MWTools::hmw_analysis_constants$hmw_power_sheet,
                                   labor_type_col = MWTools::hmw_analysis_constants$labor_type_col,
                                   power_col = MWTools::hmw_analysis_constants$power_col,
                                   total_wk_hrs_ilo_col = MWTools::hmw_analysis_constants$total_wk_hrs_ilo_col,
                                   useful_energy_hmw_col = MWTools::hmw_analysis_constants$useful_energy_hmw_col,
                                   hours_to_seconds = MWTools::unit_constants$hours_to_seconds,
                                   joules_to_megajoules = MWTools::unit_constants$joules_to_megajoules){

  # Reads power data
  power_data <- readxl::read_xlsx(path = hmw_analysis_data_path,
                                  sheet = hmw_power_sheet) %>%
    dplyr::select(-dplyr::all_of(unit)) %>%
    tidyr::pivot_longer(cols = -dplyr::all_of(c(sex_ilo_col, hmw_region_code_col, labor_type_col)),
                        names_to = year,
                        values_to = power_col) %>%
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]])
    ) %>%
    magrittr::set_colnames(c(sex_ilo_col, labor_type_col, hmw_region_code_col, year, power_col))

  # Add power data to data frame
  .df %>%
    dplyr::left_join(power_data, by = c(sex_ilo_col, labor_type_col, hmw_region_code_col, year)) %>%
    dplyr::mutate(
      "{useful_energy_hmw_col}" := .data[[power_col]] * .data[[total_wk_hrs_ilo_col]] * hours_to_seconds * joules_to_megajoules
    ) %>%
    dplyr::select(-dplyr::all_of(power_col))
}


#' Produce a tidy human muscle work data frame
#'
#' @param .df A data frame containing the final and useful energy consumed
#'            by human workers.
#'            Usually produced by calling the
#'            `add_hmw_region_codes`,
#'            `fill_ilo_data`,
#'            `calc_total_hours_worked`,
#'            `get_broad.sector_data`,
#'            `add_hmw_analysis_sectors`,
#'            `calc_hmw_final_energy`,
#'            `calc_hmw_primary_energy`, and
#'            `calc_hmw_useful_energy` functions in sequence on the raw FAO data.
#' @param year,sector_col,species,energy_col,stage_col,units_col See `MWTools::mw_constants`.
#' @param sex_ilo_col See `MWTools::ilo_cols`.
#' @param country_col,hmw_region_code_col See `MWTools::conc_cols`.
#' @param final_energy_col,primary_energy_col,useful_energy_hmw_col See `MWTools::hmw_analysis_constants`.
#'
#'
#' @export
#'
#' @examples
#' tidied_pfu_data <- read.csv(file = MWTools::hmw_test_data_path()) %>%
#'   add_hmw_region_codes() %>%
#'   fill_ilo_data() %>%
#'   calc_total_hours_worked() %>%
#'   get_broad.sector_data() %>%
#'   split_labor_by_sector() %>%
#'   calc_hmw_final_energy() %>%
#'   calc_hmw_primary_energy() %>%
#'   calc_hmw_useful_energy() %>%
#'   tidy_hmw_pfu()
tidy_hmw_pfu <- function(.df,
                         year = MWTools::mw_cols$year,
                         sector_col = MWTools::mw_constants$sector_col,
                         species = MWTools::mw_constants$species,
                         energy_col = MWTools::mw_cols$e_dot,
                         stage_col = MWTools::mw_constants$stage_col,
                         units_col = MWTools::mw_cols$unit,
                         sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
                         country_col = MWTools::conc_cols$country_col,
                         hmw_region_code_col = MWTools::conc_cols$hmw_region_code_col,
                         final_energy_col = MWTools::hmw_analysis_constants$final_energy_col,
                         primary_energy_col = MWTools::hmw_analysis_constants$primary_energy_col,
                         useful_energy_hmw_col = MWTools::hmw_analysis_constants$useful_energy_hmw_col){

  .df %>%
    tidyr::pivot_longer(cols = dplyr::all_of(c(final_energy_col, primary_energy_col, useful_energy_hmw_col)),
                        names_to = stage_col,
                        values_to = energy_col) %>%
    dplyr::select(dplyr::all_of(c(country_col, year, sex_ilo_col,
                                  stage_col, sector_col, energy_col))) %>%
    dplyr::mutate(
      "{stage_col}" := stringr::str_replace(.data[[stage_col]], stringr::fixed(" energy [MJ/year]"), "")
    ) %>%
    dplyr::mutate(
      "{sex_ilo_col}" := dplyr::case_when(
        .data[[sex_ilo_col]] == "Male" ~ "Human males",
        .data[[sex_ilo_col]] == "Female" ~ "Human females",
        TRUE ~ "Unknown sector column value"
      )
    ) %>%
    dplyr::rename("{species}" := .data[[sex_ilo_col]]) %>%
    dplyr::mutate("{energy_col}" := .data[[energy_col]] * 0.000000000001) %>%
    dplyr::mutate("{units_col}" := "EJ", .before = dplyr::all_of(energy_col))
}



#' Calculate primary, final, and useful human muscle work.
#'
#' @param .df The raw ILO data, retrieved from ILOSTAT.
#' @param concordance_path The path to the muscle work concordance information.
#' @param hmw_analysis_data_path The path to the human muscle work analysis data.
#'
#' @export
#'
#' @examples
#' pfu_energy_data <- read.csv(file = MWTools::hmw_test_data_path()) %>%
#'   calc_hmw_pfu()
calc_hmw_pfu <- function(.df,
                         concordance_path = MWTools::fao_concordance_path(),
                         hmw_analysis_data_path = MWTools::hmw_analysis_data_path()
                         ){

  .df %>%
    add_hmw_region_codes(concordance_path = concordance_path) %>%
    fill_ilo_data() %>%
    calc_total_hours_worked() %>%
    get_broad.sector_data() %>%
    split_labor_by_sector(hmw_analysis_data_path = hmw_analysis_data_path) %>%
    calc_hmw_final_energy(hmw_analysis_data_path = hmw_analysis_data_path) %>%
    calc_hmw_primary_energy(hmw_analysis_data_path = hmw_analysis_data_path) %>%
    calc_hmw_useful_energy(hmw_analysis_data_path = hmw_analysis_data_path) %>%
    tidy_hmw_pfu()

}




