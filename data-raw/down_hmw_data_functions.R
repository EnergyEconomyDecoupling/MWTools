################################################################################
## ILO data ##
################################################################################

#' Retrieve ILO data for working hours and employed persons
#'
#' Use the function `get_ilostat` function from the `Rilostat` package to create a
#' tidy data frame containing mean yearly working hours and number of employed persons
#' by country, sex, sector, and year.
#'
#' @param country_name,sex_ilo_col,sector_col,year,yearly_working_hours_ilo_col,employed_persons_ilo_col See `MWtools::mw_constants()`.
#' @param working_hours_code,employment_code See `MWTools::ilo_codes`.
#'
#' @return
#' @export
#'
#' @examples
#' ilo_hmw_data <- get_ilo_hmw_data()
#'
get_ilo_hmw_data <- function(country_code_col = MWTools::conc_cols$country_code_col,
                             sex_ilo_col = MWTools::ilo_cols$sex_ilo_col,
                             sector_col = MWTools::mw_constants$sector_col,
                             year = MWTools::mw_constants$year,
                             ref_area_col = MWTools::ilo_cols$ref_area_col,
                             yearly_working_hours_ilo_col = MWTools::ilo_cols$yearly_working_hours_ilo_col,
                             employed_persons_ilo_col = MWTools::ilo_cols$employed_persons_ilo_col,
                             working_hours_code = MWTools::ilo_codes$working_hours_code,
                             employment_code = MWTools::ilo_codes$employment_code,
                             download_location = MWTools::extdata_path()
                             ){

  # Mean weekly hours actually worked per employed person by sex and economic activity: HOW_TEMP_SEX_ECO_NB_A
  working_hours <- Rilostat::get_ilostat(id = working_hours_code,
                                         quiet = TRUE) %>%
    Rilostat::label_ilostat(code = c(ref_area_col)) %>%
    dplyr::select(ref_area, sex.label, classif1.label, time, obs_value) %>% # Create constants
    magrittr::set_colnames(c(country_code_col, sex_ilo_col, sector_col, year, yearly_working_hours_ilo_col))

  # Employment by sex and economic activity (thousands): EMP_TEMP_SEX_ECO_NB_A
  employment <- Rilostat::get_ilostat(id = employment_code,
                                      quiet = TRUE) %>%
    Rilostat::label_ilostat(code = c(ref_area_col)) %>%
    dplyr::select(ref_area, sex.label, classif1.label, time, obs_value) %>%
    magrittr::set_colnames(c(country_code_col, sex_ilo_col, sector_col, year, employed_persons_ilo_col))

  # Convert Employed persons [1000 persons] to employed persons [persons] and
  # mean working hours [hours/week] to mean working hours [hours/year]
  ilo_hmw_data <- employment %>%
    dplyr::left_join(working_hours, by = c(country_code_col, sex_ilo_col, sector_col, year)) %>%
    dplyr::mutate(
      "{employed_persons_ilo_col}" := .data[[employed_persons_ilo_col]] * 1000
    ) %>%
    dplyr::mutate(
      "{yearly_working_hours_ilo_col}" := .data[[yearly_working_hours_ilo_col]] * 52
    )

  write.csv(ilo_hmw_data, file = file.path(download_location, "ilo_hmw_data.csv"))

}

# Call get_ilo_hmw_data() function, activated when sourcing down_hmw_data_functions.R
get_ilo_hmw_data()
