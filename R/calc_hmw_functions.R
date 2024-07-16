#' Prepare a unified ILO dataframe from raw employment and working hours data
#'
#' Prepare a unified ILO dataframe from raw employment and working hours data,
#' usually obtained using the `Rilostat` R package.
#'
#' @param ilo_working_hours_data A dataframe containing raw ILO working hours data.
#' @param ilo_employment_data A dataframe containing raw ILO employment data.
#'
#' @export
#'
#' @examples
#' ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
#' ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())
#' prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
#'                   ilo_employment_data = ilo_employment_data)
prepareRawILOData <- function(ilo_working_hours_data, ilo_employment_data){

  # Establish constants
  country_code_col <- MWTools::conc_cols$country_code_col
  sex_ilo_col <- MWTools::ilo_cols$sex_ilo_col
  sector_col <- MWTools::mw_constants$sector_col
  year_col <- MWTools::mw_cols$year
  ref_area_col <- MWTools::ilo_cols$ref_area_col
  yearly_working_hours_ilo_col <- MWTools::ilo_cols$yearly_working_hours_ilo_col
  employed_persons_ilo_col <- MWTools::ilo_cols$employed_persons_ilo_col
  working_hours_code <- MWTools::ilo_codes$working_hours_code
  employment_code <- MWTools::ilo_codes$employment_code

  # Mean weekly hours actually worked per employed person by sex and economic activity:
  # HOW_TEMP_SEX_ECO_NB_A
  working_hours <- ilo_working_hours_data |>
    dplyr::select(dplyr::all_of(c("ref_area", "sex", "classif1", "time", "obs_value"))) |>
    magrittr::set_colnames(c(country_code_col, sex_ilo_col, sector_col, year_col, yearly_working_hours_ilo_col))

  # Employment by sex and economic activity (thousands): EMP_TEMP_SEX_ECO_NB_A
  employment <- ilo_employment_data |>
    dplyr::select(dplyr::all_of(c("ref_area", "sex", "classif1", "time", "obs_value"))) |>
    magrittr::set_colnames(c(country_code_col, sex_ilo_col, sector_col, year_col, employed_persons_ilo_col))

  # Convert Employed persons [1000 persons] to employed persons [persons] and
  # mean working hours [hours/week] to mean working hours [hours/year]
  ilo_hmw_data <- employment |>
    dplyr::left_join(working_hours, by = c(country_code_col, sex_ilo_col, sector_col, year_col)) |>
    dplyr::mutate(
      "{employed_persons_ilo_col}" := .data[[employed_persons_ilo_col]] * 1000
    ) |>
    dplyr::mutate(
      "{yearly_working_hours_ilo_col}" := .data[[yearly_working_hours_ilo_col]] * 52
    ) |>
    # Removes leading string "Sex: " from Sex data
    dplyr::mutate(
      "{sex_ilo_col}" := stringr::str_replace(.data[[sex_ilo_col]], stringr::fixed("Sex: "), "")
    ) |>
    dplyr::mutate(
      "{year_col}" := as.numeric(.data[[year_col]])
    ) |>
    # Manually change the ILO country code for Kosovo from KOS to XKX, this is the
    # only country code which does not correspond to the other datasets as
    # Kosovo has not formal country code
    dplyr::mutate(
      Country.code = dplyr::case_when(
        Country.code == "KOS" ~ "XKX",
        TRUE ~ as.character(Country.code)
      )
    )

  return(ilo_hmw_data)
}


#' Add the regional codes used on analysis of human muscle work.
#'
#' ...
#'
#' @param .df The raw ILO data, retrieved from ILOSTAT.
#' @param concordance_path The path to the country code concordance information.
#'                         Set to the bundled information by default,
#'                         retrieved using the `fao_concordance_path` function.
#' @param country_col,country_code_col,country_incl_col,hmw_region_code_col,mapping_sheet See `MWTools::conc_cols`.
#' @param sex_ilo_col,yearly_working_hours_ilo_col,employed_persons_ilo_col See `MWTools::ilo_cols`.
#' @param sector_col,year See `MWTools::mw_constants`.
#' @param yes_const See `MWTools::amw_analysis_constants`.
#'
#' @export
#'
#' @examples
#' ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
#' ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())
#' hmw_data <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
#'                               ilo_employment_data = ilo_employment_data)
#' working_humans_data <- hmw_data |>
#'   add_hmw_region_codes()
add_hmw_region_codes <- function(.df,
                                 concordance_path = MWTools::fao_concordance_path(),
                                 mapping_sheet = MWTools::conc_cols$mapping_sheet,
                                 country_col = MWTools::conc_cols$country_col,
                                 country_code_col = MWTools::conc_cols$country_code_col,
                                 country_incl_col = MWTools::conc_cols$country_incl_col,
                                 hmw_region_code_col = MWTools::conc_cols$hmw_region_code_col,
                                 sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
                                 sector_col = MWTools::mw_constants$sector_col,
                                 year = MWTools::mw_cols$year,
                                 yearly_working_hours_ilo_col = MWTools::ilo_cols$yearly_working_hours_ilo_col,
                                 employed_persons_ilo_col = MWTools::ilo_cols$employed_persons_ilo_col,
                                 yes_const = MWTools::amw_analysis_constants$yes_const
                                 ){

  hmw_region_codes <- readxl::read_xlsx(path = concordance_path,
                                        sheet = mapping_sheet) |>
    dplyr::select(dplyr::all_of(c(country_col, hmw_region_code_col, country_incl_col))) |>
    magrittr::set_colnames(c(country_code_col, hmw_region_code_col, country_incl_col))

  .df |>
    dplyr::left_join(hmw_region_codes, by = country_code_col) |>
    dplyr::relocate(dplyr::all_of(hmw_region_code_col), .after = dplyr::all_of(country_code_col)) |>
    dplyr::filter(.data[[country_incl_col]] == yes_const) |>
    dplyr::select(-dplyr::all_of(country_incl_col)) |>
    magrittr::set_colnames(c(country_col, hmw_region_code_col, sex_ilo_col,
                             sector_col, year, employed_persons_ilo_col,
                             yearly_working_hours_ilo_col))
}


#' Fill missing data for the number of hours worked and employed persons
#'
#' Fill missing values from the ILO for the number of employed persons and
#' yearly working hours by adding years absent from the raw data, removing
#' groups of data for which there are no values at all, then interpolating
#' and extrapolating groups of data for which there is at least one value.
#'
#' @param .df The ILO labor data with added region codes.
#'            Usually produced by calling the
#'            `add_hmw_region_codes` function in sequence on the raw FAO data.
#' @param country_col,hmw_region_code_col See `MWTools::conc_cols`.
#' @param sex_ilo_col,yearly_working_hours_ilo_col,employed_persons_ilo_col,employed_count,hours_count See `MWTools::ilo_cols`.
#' @param sector_col See `MWTools::mw_constants`.
#' @param year See `MWTools::mw_cols`.
#' @param col_1960,col_2020 See `MWTools::hmw_analysis_constants`.
#'
#' @export
#'
#' @examples
#' ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
#' ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())
#' hmw_data <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
#'                               ilo_employment_data = ilo_employment_data)
#' working_humans_data <- hmw_data |>
#'   add_hmw_region_codes() |>
#'   fill_ilo_data()
fill_ilo_data <- function(.df,
                          country_col = MWTools::conc_cols$country_col,
                          hmw_region_code_col = MWTools::conc_cols$hmw_region_code_col,
                          sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
                          sector_col = MWTools::mw_constants$sector_col,
                          year = MWTools::mw_cols$year,
                          yearly_working_hours_ilo_col = MWTools::ilo_cols$yearly_working_hours_ilo_col,
                          employed_persons_ilo_col = MWTools::ilo_cols$employed_persons_ilo_col,
                          hours_count = MWTools::ilo_cols$hours_count,
                          employed_count = MWTools::ilo_cols$employed_count,
                          col_1960 = MWTools::hmw_analysis_constants$col_1960,
                          col_2020 = MWTools::hmw_analysis_constants$col_2020){

  # Fills data for each Country, HMW region code, Sex, and Sector by
  # linearly interpolating and extrapolating the data.
  .df |>
    dplyr::group_by(.data[[country_col]], .data[[hmw_region_code_col]], .data[[sex_ilo_col]], .data[[sector_col]]) |>

    # Add a column containing the number of data points for each group of data
    # prior to adding na values for missing years
    dplyr::mutate("{employed_count}" := sum(!is.na(.data[[employed_persons_ilo_col]]))) |>
    dplyr::mutate("{hours_count}" := sum(!is.na(.data[[yearly_working_hours_ilo_col]]))) |>

    # Remove groups of data that only have one observation, as interpolation
    # and extrapolation is not possible from a single data point
    dplyr::filter(.data[[employed_count]] > 1 & .data[[hours_count]] > 1) |>
    # Remove columns that are no longer needed
    dplyr::select(-dplyr::any_of(c(employed_count, hours_count))) |>


    # Complete data frame by adding rows for missing years between 1960 and 2020
    tidyr::complete(Year = tidyr::full_seq(col_1960:col_2020, 1)) |>
    # Remove groups for which there is no data at all
    dplyr::filter(!all(is.na(.data[[employed_persons_ilo_col]]))) |>
    dplyr::filter(!all(is.na(.data[[yearly_working_hours_ilo_col]]))) |>
    # Fill missing values
    # Linear interpolation
    dplyr::mutate(
      "{employed_persons_ilo_col}" := zoo::na.approx(.data[[employed_persons_ilo_col]], na.rm = FALSE),
      "{yearly_working_hours_ilo_col}" := zoo::na.approx(.data[[yearly_working_hours_ilo_col]], na.rm = FALSE)
    ) |>
    # Holding constant
    tidyr::fill(dplyr::all_of(yearly_working_hours_ilo_col), .direction = "downup") |>
    tidyr::fill(dplyr::all_of(employed_persons_ilo_col), .direction = "downup") |>
    # Ungroup data
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
#' ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
#' ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())
#' hmw_data <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
#'                               ilo_employment_data = ilo_employment_data)
#' working_hours_data <- hmw_data |>
#'   add_hmw_region_codes() |>
#'   fill_ilo_data() |>
#'   calc_total_hours_worked()
calc_total_hours_worked <- function(.df,
                                     yearly_working_hours_ilo_col = MWTools::ilo_cols$yearly_working_hours_ilo_col,
                                     employed_persons_ilo_col = MWTools::ilo_cols$employed_persons_ilo_col,
                                     total_wk_hrs_ilo_col = MWTools::hmw_analysis_constants$total_wk_hrs_ilo_col){

  .df |>
    dplyr::mutate(
      "{total_wk_hrs_ilo_col}" := .data[[employed_persons_ilo_col]] * .data[[yearly_working_hours_ilo_col]]
    ) |>
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
#' @param ilo_agr_name,ilo_ind_name,ilo_ser_name,agr_name,ind_name,ser_name String names in `.df` and the outgoing data frame for the ILO economic sectors of interest to MW calculations.
#' @param ilo_female_name,ilo_male_name,female_name,male_name String names in `.df` and the outgoing data frame for the ILO sex identifiers of interest to MW calculations.
#'                                                            For now, we include only males and females.
#'                                                            There are instances of "other" that will need to be dealt with
#'                                                            in future releases.
#'
#' @export
#'
#' @examples
#' ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
#' ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())
#' hmw_data <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
#'                               ilo_employment_data = ilo_employment_data)
#' working_hours_sector_data <- hmw_data |>
#'   add_hmw_region_codes() |>
#'   fill_ilo_data() |>
#'   calc_total_hours_worked() |>
#'   get_broad.sector_data()
get_broad.sector_data <- function(.df,
                                  sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
                                  sector_col = MWTools::mw_constants$sector_col,
                                  ilo_agr_name = MWTools::hmw_sector_constants$ilo_agr_name,
                                  ilo_ind_name = MWTools::hmw_sector_constants$ilo_ind_name,
                                  ilo_ser_name = MWTools::hmw_sector_constants$ilo_ser_name,
                                  agr_name = MWTools::hmw_sector_constants$agr_name,
                                  ind_name = MWTools::hmw_sector_constants$ind_name,
                                  ser_name = MWTools::hmw_sector_constants$ser_name,
                                  ilo_female_name = MWTools::hmw_sex_constants$ilo_female_name,
                                  ilo_male_name = MWTools::hmw_sex_constants$ilo_male_name,
                                  female_name = MWTools::hmw_sex_constants$female_name,
                                  male_name = MWTools::hmw_sex_constants$male_name){

  .df |>
    dplyr::filter(.data[[sector_col]] %in% c(ilo_agr_name, ilo_ind_name, ilo_ser_name)) |>
    dplyr::mutate(
      "{sector_col}" := dplyr::case_when(
        .data[[sector_col]] == ilo_agr_name ~ agr_name,
        .data[[sector_col]] == ilo_ind_name ~ ind_name,
        .data[[sector_col]] == ilo_ser_name ~ ser_name,
        TRUE ~ NA_character_
      )
    ) |>
    # dplyr::filter(.data[[sex_ilo_col]] != "Total")
    dplyr::filter(.data[[sex_ilo_col]] %in% c(ilo_female_name, ilo_male_name)) |>
    dplyr::mutate(
      "{sex_ilo_col}" := dplyr::case_when(
        .data[[sex_ilo_col]] == ilo_female_name ~ female_name,
        .data[[sex_ilo_col]] == ilo_male_name ~ male_name,
        TRUE ~ NA_character_
      )
    )
}


#' Splits ILO labor data by sector
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
#' ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
#' ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())
#' hmw_data <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
#'                               ilo_employment_data = ilo_employment_data)
#' working_hours_labor_type_data <- hmw_data |>
#'   add_hmw_region_codes() |>
#'   fill_ilo_data() |>
#'   calc_total_hours_worked() |>
#'   get_broad.sector_data() |>
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
                                           sheet = hmw_labor_map_sheet) |>
    dplyr::select(-dplyr::all_of(c(unit))) |>
    tidyr::pivot_longer(cols = -dplyr::all_of(c(sex_ilo_col,
                                                sector_col,
                                                labor_type_col,
                                                hmw_region_code_col)),
                        names_to = year,
                        values_to = labor_split_col) |>
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]])
    )

  .df |>
    dplyr::filter(.data[[sector_col]] %in% c(agriculture,
                                             industry,
                                             services)) |>
    dplyr::left_join(sector_mapping_data,
                     by = c(hmw_region_code_col,
                            sex_ilo_col,
                            sector_col,
                            year),
                     # The left_join() results in multiple rows, as desired.
                     # But left_join() has been changed to give a warning when that occurs.
                     # multiple = "all" suppresses that warning in an approved way.
                     multiple = "all") |>
    dplyr::relocate(dplyr::all_of(labor_type_col), .after = dplyr::all_of(sector_col)) |>
    dplyr::mutate(
      "{employed_persons_ilo_col}" := .data[[employed_persons_ilo_col]] * .data[[labor_split_col]],
      "{total_wk_hrs_ilo_col}" := .data[[total_wk_hrs_ilo_col]] * .data[[labor_split_col]]
    ) |>
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
#' ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
#' ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())
#' hmw_data <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
#'                               ilo_employment_data = ilo_employment_data)
#' final_energy_data <- hmw_data |>
#'   add_hmw_region_codes() |>
#'   fill_ilo_data() |>
#'   calc_total_hours_worked() |>
#'   get_broad.sector_data() |>
#'   split_labor_by_sector() |>
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
                                 sheet = hmw_food_sheet) |>
    dplyr::select(-dplyr::all_of(unit)) |>
    tidyr::pivot_longer(cols = -dplyr::all_of(c(sex_ilo_col, hmw_region_code_col, labor_type_col)),
                        names_to = year,
                        values_to = food_consumption_col) |>
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]])
    ) |>
    magrittr::set_colnames(c(sex_ilo_col, labor_type_col, hmw_region_code_col, year, food_consumption_col))

  # Reads plate waste data
  plate_waste_data <- readxl::read_xlsx(path = hmw_analysis_data_path,
                                        sheet = hmw_plate_waste_sheet) |>
    dplyr::select(-dplyr::all_of(c(unit, exemplar_method_col))) |>
    tidyr::pivot_longer(cols = -dplyr::all_of(c(hmw_region_code_col)),
                        names_to = year,
                        values_to = plate_waste_col) |>
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]])
    )

  # Adds daily food consumption
  .df |>
    dplyr::left_join(food_data, by = c(sex_ilo_col, labor_type_col, hmw_region_code_col, year)) |>

    # Add plate waste
    dplyr::left_join(plate_waste_data, by = c(year, hmw_region_code_col)) |>

    # Convert from kcal/day to MJ/year !!! Currently assuming every day worked - need to check ILO data for number of days worked !!!
    dplyr::mutate(
      "{energy_pppa_col}" := .data[[food_consumption_col]] * kcal_to_mj * 365,
      "{final_energy_col}" := (.data[[employed_persons_ilo_col]] * .data[[energy_pppa_col]]) / (1 - .data[[plate_waste_col]])
    ) |>
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
#' ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
#' ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())
#' hmw_data <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
#'                               ilo_employment_data = ilo_employment_data)
#' primary_energy_data <- hmw_data |>
#'   add_hmw_region_codes() |>
#'   fill_ilo_data() |>
#'   calc_total_hours_worked() |>
#'   get_broad.sector_data() |>
#'   split_labor_by_sector() |>
#'   calc_hmw_final_energy() |>
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
                                          sheet = hmw_harvest_waste_sheet) |>
    dplyr::select(-dplyr::all_of(c(unit, exemplar_method_col))) |>
    tidyr::pivot_longer(cols = -dplyr::all_of(c(hmw_region_code_col)),
                        names_to = year,
                        values_to = hmw_harvest_waste_col) |>
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]])
    )

  # Add harvest waste data and calculate primary energy
  .df |>
    dplyr::left_join(harvest_waste_data, by = c(year, hmw_region_code_col)) |>
    dplyr::mutate(
      "{primary_energy_col}" := .data[[final_energy_col]] / (1 - .data[[hmw_harvest_waste_col]])
    ) |>
    dplyr::select(-dplyr::any_of(hmw_harvest_waste_col))
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
#' ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
#' ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())
#' hmw_data <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
#'                               ilo_employment_data = ilo_employment_data)
#' useful_energy_data <- hmw_data |>
#'   add_hmw_region_codes() |>
#'   fill_ilo_data() |>
#'   calc_total_hours_worked() |>
#'   get_broad.sector_data() |>
#'   split_labor_by_sector() |>
#'   calc_hmw_final_energy() |>
#'   calc_hmw_primary_energy() |>
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
                                  sheet = hmw_power_sheet) |>
    dplyr::select(-dplyr::all_of(unit)) |>
    tidyr::pivot_longer(cols = -dplyr::all_of(c(sex_ilo_col, hmw_region_code_col, labor_type_col)),
                        names_to = year,
                        values_to = power_col) |>
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]])
    ) |>
    magrittr::set_colnames(c(sex_ilo_col, labor_type_col, hmw_region_code_col, year, power_col))

  # Add power data to data frame
  .df |>
    dplyr::left_join(power_data, by = c(sex_ilo_col, labor_type_col, hmw_region_code_col, year)) |>
    dplyr::mutate(
      "{useful_energy_hmw_col}" := .data[[power_col]] * .data[[total_wk_hrs_ilo_col]] * hours_to_seconds * joules_to_megajoules
    ) |>
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
#' @param concordance_species See `MWTools::conc_cols`.
#' @param sex_ilo_col See `MWTools::ilo_cols`.
#' @param country_col,hmw_region_code_col See `MWTools::conc_cols`.
#' @param final_energy_col,primary_energy_col,useful_energy_hmw_col,labor_type_col See `MWTools::hmw_analysis_constants`.
#'
#'
#' @export
#'
#' @examples
#' ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
#' ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())
#' hmw_data <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
#'                               ilo_employment_data = ilo_employment_data)
#' tidied_pfu_data <- hmw_data |>
#'   add_hmw_region_codes() |>
#'   fill_ilo_data() |>
#'   calc_total_hours_worked() |>
#'   get_broad.sector_data() |>
#'   split_labor_by_sector() |>
#'   calc_hmw_final_energy() |>
#'   calc_hmw_primary_energy() |>
#'   calc_hmw_useful_energy() |>
#'   tidy_hmw_pfu()
tidy_hmw_pfu <- function(.df,
                         year = MWTools::mw_cols$year,
                         sector_col = MWTools::mw_constants$sector_col,
                         concordance_species = MWTools::conc_cols$species,
                         energy_col = MWTools::mw_cols$e_dot,
                         stage_col = MWTools::mw_constants$stage_col,
                         units_col = MWTools::mw_cols$unit,
                         sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
                         country_col = MWTools::conc_cols$country_col,
                         hmw_region_code_col = MWTools::conc_cols$hmw_region_code_col,
                         labor_type_col = MWTools::hmw_analysis_constants$labor_type_col,
                         final_energy_col = MWTools::hmw_analysis_constants$final_energy_col,
                         primary_energy_col = MWTools::hmw_analysis_constants$primary_energy_col,
                         useful_energy_hmw_col = MWTools::hmw_analysis_constants$useful_energy_hmw_col){

  .df |>
    tidyr::pivot_longer(cols = dplyr::all_of(c(final_energy_col, primary_energy_col, useful_energy_hmw_col)),
                        names_to = stage_col,
                        values_to = energy_col) |>
    dplyr::select(dplyr::all_of(c(country_col, year, sex_ilo_col,
                                  stage_col, sector_col, labor_type_col, energy_col))) |>
    dplyr::mutate(
      "{stage_col}" := stringr::str_replace(.data[[stage_col]], stringr::fixed(" energy [MJ/year]"), "")
    ) |>
    dplyr::mutate(
      "{sex_ilo_col}" := dplyr::case_when(
        .data[[sex_ilo_col]] == "Male" ~ "Human males",
        .data[[sex_ilo_col]] == "Female" ~ "Human females",
        TRUE ~ "Unknown sector column value"
      )
    ) |>
    dplyr::rename("{concordance_species}" := dplyr::all_of(sex_ilo_col)) |>
    dplyr::mutate(
      "{energy_col}" := .data[[energy_col]] * 0.000000000001,
      "{units_col}" := "EJ", .before = dplyr::all_of(energy_col)
    ) |>
    dplyr::group_by(
      dplyr::across({{ country_col }}),
      dplyr::across({{ year }}),
      dplyr::across({{ concordance_species }}),
      dplyr::across({{ stage_col }}),
      dplyr::across({{ sector_col }}),
      dplyr::across({{ units_col }})
    ) |>
    dplyr::summarise("{energy_col}" := sum(.data[[energy_col]])) |>
    dplyr::ungroup()
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
#' ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
#' ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())
#' hmw_data <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
#'                               ilo_employment_data = ilo_employment_data)
#' pfu_energy_data <- hmw_data |>
#'   calc_hmw_pfu()
calc_hmw_pfu <- function(.df,
                         concordance_path = MWTools::fao_concordance_path(),
                         hmw_analysis_data_path = MWTools::hmw_analysis_data_path()){

  .df |>
    add_hmw_region_codes(concordance_path = concordance_path) |>
    fill_ilo_data() |>
    calc_total_hours_worked() |>
    get_broad.sector_data() |>
    split_labor_by_sector(hmw_analysis_data_path = hmw_analysis_data_path) |>
    calc_hmw_final_energy(hmw_analysis_data_path = hmw_analysis_data_path) |>
    calc_hmw_primary_energy(hmw_analysis_data_path = hmw_analysis_data_path) |>
    calc_hmw_useful_energy(hmw_analysis_data_path = hmw_analysis_data_path) |>
    tidy_hmw_pfu()
}




