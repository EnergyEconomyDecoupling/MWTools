# This script uses the World Bank package wbstats to download data via API's,
# and wrangles data into a tidy format

# Loads required packages
library(wbstats)
library(tidyverse)

download_hmw_data <- function () {

  # Creates a dataframe containing total population by country
  population <- wbstats::wb_data("SP.POP.TOTL") %>%
    dplyr::select(2:5)

  # Downloads data for Population ages 15-64 (% of total population)
  population_15_64 <- wbstats::wb_data("SP.POP.1564.TO.ZS") %>%
    dplyr::select(2:5)

  # Downloads data for "Labor force participation rate, total (% of total population ages 15-64) (modeled ILO estimate)"
  participation_rate <- wbstats::wb_data("SL.TLF.ACTI.ZS") %>%
    dplyr::select(2:5)

  # Downloads data for "Employment in agriculture (% of total employment) (modeled ILO estimate)"
  agr_workers_per <- wbstats::wb_data("SL.AGR.EMPL.ZS") %>%
    dplyr::select(2:5)

  # Downloads data for "Employment in services (% of total employment) (modeled ILO estimate)"
  srv_workers_per <- wbstats::wb_data("SL.SRV.EMPL.ZS") %>%
    dplyr::select(2:5)

  # Downloads data for "Employment in industry (% of total employment) (modeled ILO estimate)"
  ind_workers_per <- wbstats::wb_data("SL.IND.EMPL.ZS") %>%
    dplyr::select(2:5)

  # Combines data into a single data frame
  human_labor_data <- population %>%
    merge(population_15_64) %>%
    merge(participation_rate) %>%
    merge(agr_workers_per) %>%
    merge(srv_workers_per) %>%
    merge(ind_workers_per) %>%
    magrittr::set_colnames(c("ISO_Country_Code",
                             "Country",
                             "Year",
                             "Total_Population",
                             "WorkingAge_Population_Per",
                             "Participation_Rate_Per",
                             "Agriculture", "Services", "Industry"))
}


