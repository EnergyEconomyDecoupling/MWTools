################################################################################
## ILO data ##
################################################################################

#' Retrieve ILO data for working hours and employed persons
#'
#' Use the `Rilostat` package to create a data frame containing mean yearly
#' working hours and number of employed persons by country, sex, sector, and year.
#'
#' @param country_name,sex,sector,year,working_hours,employed persons See `MWtools::mw_constants()`.
#'
#' @return
#' @export
#'
#' @examples
#' ilo_hmw_data <- get_ilo_hmw_data()
#'
get_ilo_hmw_data <- function(country_name = MWTools::mw_constants$country_name,
                             sex = MWTools::mw_constants$sex,
                             sector = MWTools::mw_constants$sector,
                             year = MWTools::mw_constants$year,
                             working_hours = MWTools::mw_constants$working_hours,
                             employed_persons = MWTools::mw_constants$employed_persons
                             ){

  # Mean weekly hours actually worked per employed person by sex and economic activity: HOW_TEMP_SEX_ECO_NB_A
  working_hours <- Rilostat::get_ilostat(id = "HOW_TEMP_SEX_ECO_NB_A") %>%
    Rilostat::label_ilostat() %>%
    dplyr::select(ref_area.label, sex.label, classif1.label, time, obs_value) %>%
    magrittr::set_colnames(c(country_name, sex, sector, year, working_hours))

  # Employment by sex and economic activity (thousands): EMP_TEMP_SEX_ECO_NB_A
  employment <- Rilostat::get_ilostat(id = "EMP_TEMP_SEX_ECO_NB_A") %>%
    Rilostat::label_ilostat() %>%
    dplyr::select(ref_area.label, sex.label, classif1.label, time, obs_value) %>%
    magrittr::set_colnames(c(country_name, sex, sector, year, employed_persons))

  # Convert Employed persons [1000 persons] to employed persons [persons] and
  # mean working hours [hours/week] to mean working hours [hours/year]
  ilo_hmw_data <- employment %>%
    left_join(working_hours, by = c(country_name, sex, sector, year)) %>%
    dplyr::mutate(
      "{employed_persons}" = .data[[employed_persons]] * 1000,
      .keep = "unused"
    ) %>%
    dplyr::mutate(
      "{working_hours}" = .data[[working_hours]] * 52,
      .keep = "unused"
    )

  return(ilo_hmw_data)

  }

ilo_data_rev.units <- get_ilo_hmw_data()


# Fills data for each Country, Sex, and Sector based on earliest year
## Should we fill without grouping by country? (fills by nearest alphabetical country)?
ilo_data_filled <- ilo_data_rev.units %>%
  dplyr::group_by(Country.name, Sex, Sector) %>%
  dplyr::arrange(Year, .by_group = TRUE) %>%
  tidyr::fill(`Employed.persons [persons]`, .direction = "up") %>%
  tidyr::fill(`Working.hours [hours/year]`, .direction = "up") %>%
  dplyr::ungroup()

# Calculates total number of hours worked per year
ilo_data_total.hours <- ilo_data_filled %>%
  dplyr::mutate("Total.hours [hours/year]" = `Employed.persons [persons]` * `Working.hours [hours/year]`)

# Filters data to only include sector data by ISIC
ilo_data_total.hours_isic <- ilo_data_total.hours %>%
  dplyr::filter(stringr::str_detect(Sector, pattern = fixed("(ISIC-Rev.4):"))) %>%
  dplyr::mutate(Sector = stringr::str_replace(Sector, ".*?\\:\\s", ""))

# Filters data to only include sector data by aggregate
ilo_data_total.hours_agg <- ilo_data_total.hours %>%
  dplyr::filter(stringr::str_detect(Sector, pattern = fixed("(Aggregate):"))) %>%
  dplyr::mutate(Sector = stringr::str_replace(Sector, ".*?\\:\\s", ""))

# Filters data to only include sector data by broad
ilo_data_total.hours_broad <- ilo_data_total.hours %>%
  dplyr::filter(stringr::str_detect(Sector, pattern = fixed("(Broad sector):"))) %>%
  dplyr::mutate(Sector = stringr::str_replace(Sector, ".*?\\:\\s", ""))




