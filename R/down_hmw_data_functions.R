################################################################################
## ILO data ##
################################################################################

# Retrieve table of contents
ilo_toc <- Rilostat::get_ilostat_toc()

# Launch bundled shiny app for data exploration
Rilostat::runExplorer()

# Mean weekly hours actually worked per employed person by sex and economic activity: HOW_TEMP_SEX_ECO_NB_A
working_hours <- Rilostat::get_ilostat(id = "HOW_TEMP_SEX_ECO_NB_A")

working_hours_lab <- working_hours %>%
  Rilostat::label_ilostat()

working_hours_trimmed <- working_hours_lab %>%
  dplyr::select(ref_area.label, sex.label, classif1.label, time, obs_value) %>%
  magrittr::set_colnames(c("Country.name", "Sex", "Sector", "Year", "Working.hours [hours/week]"))

# Employment by sex and economic activity (thousands): EMP_TEMP_SEX_ECO_NB_A
# 1947-2020
employment <- Rilostat::get_ilostat(id = "EMP_TEMP_SEX_ECO_NB_A")

employment_lab <- employment %>%
  Rilostat::label_ilostat()

employment_trimmed <- employment_lab %>%
  dplyr::select(ref_area.label, sex.label, classif1.label, time, obs_value) %>%
  magrittr::set_colnames(c("Country.name", "Sex", "Sector", "Year", "Employed.persons [1000 persons]"))

# Creates a data frame containing both employed persons and weekly working hours
ilo_data <- employment_trimmed %>% left_join(working_hours_trimmed, by = c("Country.name", "Sex", "Sector", "Year"))

# Alters the units for Employed.persons and Working.hours
ilo_data_rev.units <- ilo_data %>%
  dplyr::mutate(
    "Employed.persons [persons]" = (`Employed.persons [1000 persons]` * 1000),
    .keep = "unused"
  ) %>%
  dplyr::mutate(
    "Working.hours [hours/year]" = (`Working.hours [hours/week]` * 52),
    .keep = "unused"
  )

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


unique(ilo_data$Country.name)

############

USA <- ilo_data_total.hours %>%
  dplyr::filter(Country.name == "United States")

USA_sectors <- unique(USA$Sector) %>% as.data.frame()

USA_isic <- USA %>%
  dplyr::filter(stringr::str_detect(Sector, pattern = fixed("(ISIC-Rev.4):")))

USA_aggregate <- USA %>%
  dplyr::filter(stringr::str_detect(Sector, pattern = fixed("(Aggregate):")))

USA_broad <- USA %>%
  dplyr::filter(stringr::str_detect(Sector, pattern = fixed("(Broad sector):"))) %>%
  dplyr::mutate(Sector = stringr::str_replace(Sector, ".*?\\:\\s", ""))


USA_plot <- ggplot2::ggplot() +
  ggplot2::geom_line(data = USA_broad, mapping = aes(x = Year, y = `Total.hours [hours/year]`, group = 1)) +
  ggplot2::facet_grid(rows = vars(Sector), cols = vars(Sex), scales = "free_y") +
  ggplot2::scale_x_discrete(breaks = seq(1950, 2020, by = 10)) +
  ggplot2::theme(panel.spacing = unit(0.65))

USA_plot

Test <- ilo_data_total.hours %>%
  dplyr::filter(Country.name %in% c("United States", "United Kingdom", "Ghana", "Portugal")) %>%




################################################################################
## World Bank data ##
################################################################################


# # This script uses the World Bank package wbstats to download data via API's,
# # and wrangles data into a tidy format
#
# # Loads required packages
# library(wbstats)
# library(tidyverse)
#
# download_hmw_data <- function () {
#
#   # Creates a dataframe containing total population by country
#   population <- wbstats::wb_data("SP.POP.TOTL") %>%
#     dplyr::select(2:5)
#
#   # Downloads data for Population ages 15-64 (% of total population)
#   population_15_64 <- wbstats::wb_data("SP.POP.1564.TO.ZS") %>%
#     dplyr::select(2:5)
#
#   # Downloads data for "Labor force participation rate, total (% of total population ages 15-64) (modeled ILO estimate)"
#   participation_rate <- wbstats::wb_data("SL.TLF.ACTI.ZS") %>%
#     dplyr::select(2:5)
#
#   # Downloads data for "Employment in agriculture (% of total employment) (modeled ILO estimate)"
#   agr_workers_per <- wbstats::wb_data("SL.AGR.EMPL.ZS") %>%
#     dplyr::select(2:5)
#
#   # Downloads data for "Employment in services (% of total employment) (modeled ILO estimate)"
#   srv_workers_per <- wbstats::wb_data("SL.SRV.EMPL.ZS") %>%
#     dplyr::select(2:5)
#
#   # Downloads data for "Employment in industry (% of total employment) (modeled ILO estimate)"
#   ind_workers_per <- wbstats::wb_data("SL.IND.EMPL.ZS") %>%
#     dplyr::select(2:5)
#
#   # Combines data into a single data frame
#   human_labor_data <- population %>%
#     merge(population_15_64) %>%
#     merge(participation_rate) %>%
#     merge(agr_workers_per) %>%
#     merge(srv_workers_per) %>%
#     merge(ind_workers_per) %>%
#     magrittr::set_colnames(c("ISO_Country_Code",
#                              "Country",
#                              "Year",
#                              "Total_Population",
#                              "WorkingAge_Population_Per",
#                              "Participation_Rate_Per",
#                              "Agriculture", "Services", "Industry"))
# }


