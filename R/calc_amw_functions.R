
# I need to read the mapping data in from a drake target
# I need to create a new path in PFU setup

tidy_trim_amw_data <- function (.df, mw_mapping_path) {

  # Creates a filepath to the mw_mapping concordance file
  mw_mapping_path <- mw_mapping_path

  # Reads the FAO_PFU concordance sheet of the country mapping file, which maps
  # FAO country names to ISO country codes by year in accordance with
  # the IEA data
  FAO_mapping <- readxl::read_excel(mw_mapping_path,
                                        sheet = "FAO_PFU") %>%
    tibble::tibble()

  # Reads the IEA_PFU concordance sheet of the country mapping file,
  # which contains associated continent codes (Region.code)
  continent_mapping <- readxl::read_excel(mw_mapping_path,
                                          sheet = "MW_PFU") %>%
    tibble::tibble()

  # Selects relevant columns, and removes countries which are not in the IEA data
  concordance_iea <- FAO_mapping %>%
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



calc_working_animals <- function(.df, mw_mapping_path, amw_path) {

  mw_mapping_path <- mw_mapping_path

  MW_mapping <- readxl::read_excel(mw_mapping_path,
                                   sheet = "MW_PFU") %>%
    tibble::tibble() %>%
    dplyr::select(MW_region_code, `2018`) %>%
    magrittr::set_colnames(c("MW_region_code", "ISO_Country_Code"))

  PS_mapping_path <- amw_path

  working_animal_mapping <- readxl::read_excel(amw_path,
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

calc_work_split <- function(.df, amw_path) {

  amw_path <- amw_path

  end_use <- readxl::read_excel(amw_path, sheet = "DA_enduse") %>%
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


calc_yearly_feed <- function(.df, amw_path) {

  amw_path <- amw_path

  feed <- readxl::read_excel(amw_path, sheet = "DA_feed") %>%
    dplyr::select(-`Method/Source`)

  working_days <- readxl::read_excel(amw_path, sheet = "DA_days_hours") %>%
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

  trough_waste <- 0.1 # Trough waste ~ 10%

  ge_de <- (12.8/11) # Gross energy to digestible energy ratio

  .df %>%
    dplyr::mutate("final_energy_total" = (Working_Animals * year_feed_per_animal) * (ge_de) * (1/(1 - trough_waste))) %>%
    dplyr::mutate("final_energy_ag" = (Working_animals_ag * year_feed_per_animal) * (ge_de) * (1/(1 - trough_waste))) %>%
    dplyr::mutate("final_energy_tr" = (Working_animals_tr * year_feed_per_animal) * (ge_de) * (1/(1 - trough_waste)))
}



calc_primary_energy <- function(.df) {

  harvest_waste <- 0.45

  .df %>%
    dplyr::mutate("primary_energy_total" = final_energy_total / harvest_waste) %>% # (ge_de) *
    dplyr::mutate("primary_energy_ag" = final_energy_ag / harvest_waste) %>%
    dplyr::mutate("primary_energy_tr" = final_energy_tr / harvest_waste)

}




calc_useful_work <- function(.df, amw_path) {

  amw_path <- amw_path

  power <- readxl::read_excel(amw_path, sheet = "DA_power") %>%
    dplyr::select(-`Method/Source`) %>%
    magrittr::set_colnames(c("Species", "MW_region_code", "power_per_animal"))

  working_time <- readxl::read_excel(amw_path, sheet = "DA_days_hours") %>%
    dplyr::select(-`Method/Source`, -`Working days [day]`) %>%
    magrittr::set_colnames(c("Species", "MW_region_code", "Working time [hour]")) %>%
    dplyr::mutate("working_time_per_animal" = `Working time [hour]` * 3600, .keep = "unused")

  .df %>%
    dplyr::left_join(power, by = c("Species", "MW_region_code")) %>%
    dplyr::left_join(working_time, by = c("Species", "MW_region_code")) %>%
    dplyr::mutate("useful_energy_total" = `Working_Animals` * `power_per_animal` * `working_time_per_animal` / 1000000) %>%
    dplyr::mutate("useful_energy_ag" = `Working_animals_ag` * `power_per_animal` * `working_time_per_animal` / 1000000) %>%
    dplyr::mutate("useful_energy_tr" = `Working_animals_tr` * `power_per_animal` * `working_time_per_animal` / 1000000) %>%
    tibble::as_tibble()

}

tidy_amw_df <- function(.df) {

  .df %>%
    dplyr::select(Continent, ISO_Country_Code, Year, Species,
                  Live_Animals, Working_Animals,
                  final_energy_total:primary_energy_tr,
                  useful_energy_total:useful_energy_tr) %>%
    tidyr::pivot_longer(cols = final_energy_total:useful_energy_tr,
                        names_to = c("stage", "sector"),
                        names_sep = "_energy_",
                        values_to = "Energy [J]")
}

calc_amw_pfu <- function(amw_data_path, mw_mapping_path, amw_path) {

  read_amw_data(amw_data_path) %>%
    tidy_trim_amw_data(mw_mapping_path) %>%
    calc_working_animals(mw_mapping_path, amw_path) %>%
    calc_work_split(amw_path) %>%
    calc_working_animals_split() %>%
    calc_yearly_feed(amw_path) %>%
    calc_final_energy() %>%
    calc_primary_energy() %>%
    calc_useful_work(amw_path) %>%
    tidy_amw_df()

}

