
# I need to read the mapping data in from a drake target
# I need to create a new path in PFU setup

tidy_trim_amw_data <- function (.df) {

  # Creates a filepath to the country_mapping concordance file
  country_mapping_path <- paste(PFUSetup::get_abs_paths()$project_path,
                                "/Mapping/Country_Mapping_2020.xlsx", sep = "")

  # Reads the FAO_PFU concordance sheet of the country mapping file, which maps
  # World bank country names to ISO country codes by year in accordance with
  # the IEA data
  country_mapping <- readxl::read_excel(country_mapping_path,
                                        sheet = "FAO_PFU") %>%
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

  # Creates a tibble from a amw df which only contains iea countries,
  # adds the PFU codes associated with the ISO codes,
  # and adds the continent codes for each country
  .df %>%
    dplyr::right_join(concordance_iea, by = "ISO_Country_Code") %>%
    merge(concordance_continent, by = "PFU_Country_Code") %>%
    dplyr::relocate("Continent", .before = "ISO_Country_Code") %>%
    dplyr::relocate("PFU_Country_Code", .after = "ISO_Country_Code")

}



calc_working_animals <- function(.df) {

  country_mapping_path <- paste(PFUSetup::get_abs_paths()$project_path,
                                "/Mapping/Country_Mapping_2020.xlsx", sep = "")

  MW_mapping <- readxl::read_excel(country_mapping_path,
                                   sheet = "MW_PFU") %>%
    tibble::tibble() %>%
    dplyr::select(MW_region_code, `2018`) %>%
    magrittr::set_colnames(c("MW_region_code", "ISO_Country_Code"))

  PS_mapping_path <- paste(PFUSetup::get_abs_paths()$project_path,
                           "/Muscle work/amw_master_data.xlsx", sep = "")

  working_animal_mapping <- readxl::read_excel(PS_mapping_path,
                                   sheet = "DA_perc") %>%
    tibble::tibble() %>%
    dplyr::select(-MW_region, -`Exemplar/Method`) %>%
    tidyr::pivot_longer(cols = `1960`:`2019`,
                        names_to = "Year",
                        values_to = "Prop_Working_Animal")

  working_animal_mapping$Year <- as.numeric(working_animal_mapping$Year)

  .df %>%
    dplyr::left_join(MW_mapping, by = "ISO_Country_Code") %>%
    dplyr::relocate(MW_region_code, .before = Country) %>%
    dplyr::left_join(working_animal_mapping, by = c("Species", "MW_region_code", "Year")) %>%
    dplyr::mutate(Working_Animals = Live_Animals * Prop_Working_Animal)

}

calc_work_split <- function(.df) {

  PS_mapping_path <- paste(PFUSetup::get_abs_paths()$project_path,
                           "/Muscle work/amw_master_data.xlsx", sep = "")


  end_use <- readxl::read_excel(PS_mapping_path, sheet = "DA_enduse") %>%
    dplyr::select(-`Method/Source`, -Metric, -MW_region) %>%
    tidyr::pivot_longer(cols = `1960`:`2019`,
                        names_to = "Year",
                        values_to = "Prop_working_animals_ag") %>%
    dplyr::mutate("Prop_working_animals_tr" = 1 - `Prop_working_animals_ag`)

  end_use$Year <- as.double(end_use$Year)

  .df %>%
    dplyr::left_join(end_use, by = c("Species", "MW_region_code", "Year"))

}

calc_working_animals_split <- function(.df) {

  .df %>%
    dplyr::mutate(Working_animals_ag = Working_Animals * Prop_working_animals_ag) %>%
    dplyr::mutate(Working_animals_tr = Working_Animals * Prop_working_animals_tr)

}


calc_yearly_feed <- function(.df) {

  PS_mapping_path <- paste(PFUSetup::get_abs_paths()$project_path,
                           "/Muscle work/amw_master_data.xlsx", sep = "")

  feed <- readxl::read_excel(PS_mapping_path, sheet = "DA_feed") %>%
    dplyr::select(-`Method/Source`)

  working_days <- readxl::read_excel(PS_mapping_path, sheet = "DA_days_hours") %>%
    dplyr::select(-`Method/Source`, -`Working hours [hour]`) %>%
    dplyr::mutate(`Non-Working days [day]` = 365 - `Working days [day]`)

  yearly_feed <- feed %>%
    left_join(working_days, by = c("Species", "Region")) %>%
    dplyr::mutate("Working Day Feed [MJ/year]" = `Working Day Feed [MJ/day]` * `Working days [day]`,
                  .keep = "unused") %>%
    dplyr::mutate("Non-Working Day Feed [MJ/year]" = `Non-Working Day Feed [MJ/day]` * `Non-Working days [day]`,
                  .keep = "unused") %>%
    dplyr::mutate("Yearly Feed [MJ/year]" = `Working Day Feed [MJ/year]` + `Non-Working Day Feed [MJ/year]`,
                  .keep = "unused") %>%
    magrittr::set_colnames(c("Species", "MW_region_code", "year_feed_per_animal"))

  .df %>%
    dplyr::left_join(yearly_feed, by = c("Species", "MW_region_code"))
}


calc_final_energy <- function(.df) {

  trough_waste <- 0.1

  # ge_de <-

  .df %>%
    dplyr::mutate("final_energy_total" = (Working_Animals * year_feed_per_animal) * (1/(1 - trough_waste))) %>% # (ge_de) *
    dplyr::mutate("final_energy_ag" = (Working_animals_ag * year_feed_per_animal) * (1/(1 - trough_waste))) %>%
    dplyr::mutate("final_energy_tr" = (Working_animals_tr * year_feed_per_animal) * (1/(1 - trough_waste)))
}



calc_primary_energy <- function(.df) {

  harvest_waste <- 0.45

  .df %>%
    dplyr::mutate("primary_energy_total" = final_energy_total / harvest_waste) %>% # (ge_de) *
    dplyr::mutate("primary_energy_ag" = final_energy_ag / harvest_waste) %>%
    dplyr::mutate("primary_energy_tr" = final_energy_tr / harvest_waste)

}




calc_useful_work <- function(.df) {

  PS_mapping_path <- paste(PFUSetup::get_abs_paths()$project_path,
                           "/Muscle work/amw_master_data.xlsx", sep = "")

  power <- readxl::read_excel(PS_mapping_path, sheet = "DA_power") %>%
    dplyr::select(-`Method/Source`) %>%
    magrittr::set_colnames(c("Species", "MW_region_code", "power_per_animal"))

  working_time <- readxl::read_excel(PS_mapping_path, sheet = "DA_days_hours") %>%
    dplyr::select(-`Method/Source`, -`Working days [day]`) %>%
    magrittr::set_colnames(c("Species", "MW_region_code", "Working time [hour]")) %>%
    dplyr::mutate("working_time_per_animal" = `Working time [hour]` * 3600, .keep = "unused")

  .df %>%
    dplyr::left_join(power, by = c("Species", "MW_region_code")) %>%
    dplyr::left_join(working_time, by = c("Species", "MW_region_code")) %>%
    dplyr::mutate("useful_work_total" = `Working_Animals` * `power_per_animal` * `working_time_per_animal`) %>%
    dplyr::mutate("useful_work_ag" = `Working_animals_ag` * `power_per_animal` * `working_time_per_animal`) %>%
    dplyr::mutate("useful_work_tr" = `Working_animals_tr` * `power_per_animal` * `working_time_per_animal`) %>%
    tibble::as_tibble()

}
