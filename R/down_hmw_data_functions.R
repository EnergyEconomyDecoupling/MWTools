################################################################################
## ILO data ##
################################################################################

#' Retrieve ILO data for working hours and employed persons
#'
#' Use the `Rilostat` package to create a data frame containing mean yearly
#' working hours and number of employed persons by country, sex, sector, and year.
#'
#' @param country_name,sex,sector,year,working_hours,employed_persons See `MWtools::mw_constants()`.
#' @param working_hours_code,employment_code See `MWTools::ilo_codes`.
#'
#' @return
#' @export
#'
#' @examples
#' ilo_hmw_data <- get_ilo_hmw_data()
#'
get_ilo_hmw_data <- function(country_name = MWTools::mw_constants$country_name,
                             sex = MWTools::ilo_cols$sex_ilo_col,
                             sector = MWTools::ilo_cols$sector_ilo_col,
                             year = MWTools::mw_constants$year,
                             working_hours = MWTools::ilo_cols$working_hours_ilo_col,
                             employed_persons = MWTools::ilo_cols$employed_persons_ilo_col,
                             working_hours_code = MWTools::ilo_codes$working_hours_code,
                             employment_code = MWTools::ilo_codes$employment_code
                             ){

  # Mean weekly hours actually worked per employed person by sex and economic activity: HOW_TEMP_SEX_ECO_NB_A
  working_hours <- Rilostat::get_ilostat(id = working_hours_code) %>%
    Rilostat::label_ilostat() %>%
    dplyr::select(ref_area.label, sex.label, classif1.label, time, obs_value) %>%
    magrittr::set_colnames(c(country_name, sex, sector, year, working_hours))

  # Employment by sex and economic activity (thousands): EMP_TEMP_SEX_ECO_NB_A
  employment <- Rilostat::get_ilostat(id = employment_code) %>%
    Rilostat::label_ilostat() %>%
    dplyr::select(ref_area.label, sex.label, classif1.label, time, obs_value) %>%
    magrittr::set_colnames(c(country_name, sex, sector, year, employed_persons))

  # Convert Employed persons [1000 persons] to employed persons [persons] and
  # mean working hours [hours/week] to mean working hours [hours/year]
  ilo_hmw_data <- employment %>%
    dplyr::left_join(working_hours, by = c(country_name, sex, sector, year)) %>%
    dplyr::mutate(
      "{employed_persons}" := .data[[employed_persons]] * 1000
    ) %>%
    dplyr::mutate(
      "{working_hours}" := .data[[working_hours]] * 52
    )

  return(ilo_hmw_data)

  }
