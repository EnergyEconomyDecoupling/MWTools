# ilo_data_rev.units <- get_ilo_hmw_data()
#
#
# # Fills data for each Country, Sex, and Sector based on earliest year
# ## Should we fill without grouping by country? (fills by nearest alphabetical country)?
# ilo_data_filled <- ilo_data_rev.units %>%
#   dplyr::group_by(Country.name, Sex, Sector) %>%
#   dplyr::arrange(Year, .by_group = TRUE) %>%
#   tidyr::fill(`Employed.persons [persons]`, .direction = "up") %>%
#   tidyr::fill(`Working.hours [hours/year]`, .direction = "up") %>%
#   dplyr::ungroup()
#
# # Calculates total number of hours worked per year
# ilo_data_total.hours <- ilo_data_filled %>%
#   dplyr::mutate("Total.hours [hours/year]" = `Employed.persons [persons]` * `Working.hours [hours/year]`)
#
# # Filters data to only include sector data by ISIC
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
