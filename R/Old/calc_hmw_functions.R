# This script uses the World Bank data downloaded using the down_hmw_data_functions.R
# script to calculate human muscle work data

# Loads required packages
library(tidyverse)


# This function filters the hmw data to only include countries in the PFU database
# as determined by the country_mapping_2020.xlsx file

trim_tidy_hmw_data <- function(.df) {

  # Creates a filepath to the country_mapping concordance file
  country_mapping_path <- paste(PFUSetup::get_abs_paths()$project_path,
                                "/Mapping/Country_Mapping_2020.xlsx", sep = "")

  # Reads the WB_PFU concordance sheet of the country mapping file, which maps
  # World bank country names to ISO country codes by year in accordance with
  # the IEA data
  country_mapping <- readxl::read_excel(country_mapping_path,
                                        sheet = "WB_PFU") %>%
    tibble::tibble()

  # Reads the IEA_PFU concordance sheet of the country mapping file,
  # which contains associated continent codes (Region.code)
  continent_mapping <- readxl::read_excel(country_mapping_path,
                                        sheet = "IEA_PFU") %>%
    tibble::tibble()

  # Selects relevant columns, and removes countries which are not in the IEA data
  concordance_iea <- country_mapping %>%
    dplyr::select(c("ISO_Country_Code", "2018")) %>% # This will need to updated with each update of the IEA data! or use max_year from _drake.R
    magrittr::set_colnames(c("ISO_Country_Code", "PFU_Country_Code")) %>%
    dplyr::filter(PFU_Country_Code != "")

  concordance_continent <- continent_mapping %>%
    dplyr::select(c("Region.code", "2018")) %>% # This will need to updated with each update of the IEA data! or use max_year from _drake.R
    magrittr::set_colnames(c("Continent", "PFU_Country_Code"))


  # Reshapes data for the percentage of workers in each sector from wide to long format
  # Creates a tibble from a hmw df which only contains iea countries,
  # adds the PFU codes associated with the ISO codes,
  # and adds the continent codes for each country
  .df %>%
    reshape2::melt(measure.vars = c("Agriculture", "Services", "Industry"),
                   value.name = "Percentage_Workers_Sector",
                   variable.name = "Sector") %>%
    dplyr::right_join(concordance_iea, by = "ISO_Country_Code") %>%
    merge(concordance_continent, by = "PFU_Country_Code") %>%
    dplyr::relocate("Continent", .before = "ISO_Country_Code") %>%
    dplyr::relocate("PFU_Country_Code", .after = "ISO_Country_Code")

}

# This function calculates the number of workers by sector
calc_workers_sector <- function(.df) {

  # Calculates working population in a new column, and the number of workers by sector
  .df %>%
    dplyr::mutate("Working_Population" = Total_Population * (WorkingAge_Population_Per/100) * (Participation_Rate_Per/100)) %>%
    dplyr::group_by(Sector) %>%
    dplyr::mutate("Workers_Sector" = Working_Population * (Percentage_Workers_Sector/100)) %>%
    dplyr::ungroup()
}



