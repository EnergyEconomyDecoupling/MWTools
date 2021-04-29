# ilo_data <- get_ilo_hmw_data()
#
#
# # Fills data for each Country, Sex, and Sector based on earliest year
# ## Should we fill without grouping by country? (fills by nearest alphabetical country)?
# ilo_data_filled <- ilo_data %>%
#   dplyr::group_by(Country.code, Sex, Sector) %>%
#   dplyr::arrange(Year, .by_group = TRUE) %>%
#   tidyr::fill(`Employed.persons [persons]`, .direction = "up") %>%
#   tidyr::fill(`Working.hours [hours/year]`, .direction = "up") %>%
#   dplyr::ungroup()
#
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
