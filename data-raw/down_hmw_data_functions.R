# This script uses the R package Rilostat to download data for the number of
# employed persons and number of working hours, and then forms a data frame to
# to bundle in this package.

library(Rilostat)

# Establish constants
country_code_col <- MWTools::conc_cols$country_code_col
sex_ilo_col <- MWTools::ilo_cols$sex_ilo_col
sector_col <- MWTools::mw_constants$sector_col
year <- MWTools::mw_cols$year
ref_area_col <- MWTools::ilo_cols$ref_area_col
yearly_working_hours_ilo_col <- MWTools::ilo_cols$yearly_working_hours_ilo_col
employed_persons_ilo_col <- MWTools::ilo_cols$employed_persons_ilo_col
working_hours_code <- MWTools::ilo_codes$working_hours_code
employment_code <- MWTools::ilo_codes$employment_code

# Mean weekly hours actually worked per employed person by sex and economic activity:
# HOW_TEMP_SEX_ECO_NB_A
working_hours <- Rilostat::get_ilostat(id = working_hours_code,
                                       quiet = TRUE) %>%
  Rilostat::label_ilostat(code = c(ref_area_col)) %>%
  dplyr::select(ref_area_col, `sex.label`, `classif1.label`, time, obs_value) %>% # Create constants
  magrittr::set_colnames(c(country_code_col, sex_ilo_col, sector_col, year, yearly_working_hours_ilo_col))

# Employment by sex and economic activity (thousands): EMP_TEMP_SEX_ECO_NB_A
employment <- Rilostat::get_ilostat(id = employment_code,
                                    quiet = TRUE) %>%
  Rilostat::label_ilostat(code = c(ref_area_col)) %>%
  dplyr::select(ref_area_col, sex.label, classif1.label, time, obs_value) %>%
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
  ) %>%
  # Removes leading string "Sex: " from Sex data
  dplyr::mutate(
    "{sex_ilo_col}" := stringr::str_replace(.data[[sex_ilo_col]], stringr::fixed("Sex: "), "")
  ) %>%
  dplyr::mutate(
    "{year}" := as.numeric(.data[[year]])
  ) %>%
  # Manually change the ILO country code for Kosovo from KOS to XKX, this is the
  # only country code which does not correspond to the other datasets as
  # Kosovo has not formal country code
  dplyr::mutate(
    Country.code = dplyr::case_when(
      Country.code == "KOS" ~ "XKX",
      TRUE ~ as.character(Country.code)
    )
  )

# Save downloaded file
# saveRDS(object = ilo_hmw_data,
#         file = PFUSetup::get_abs_paths()$ilo_data_path)

