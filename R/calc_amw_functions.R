mw_mapping_path <- "C:\\Users\\earzm\\Dropbox\\Fellowship 1960-2015 PFU database\\Mapping\\MW_mapping.xlsx"


add_iso_codes <- function(fao_df){

  iso3_fao_names <- FAOSTAT::FAOregionProfile %>%
    dplyr::select(ISO3_CODE, FAO_TABLE_NAME) %>%
    magrittr::set_colnames(c("Country.code", "Country.name"))

  fao_df_with_iso3 <- data_tidy %>% # change
    dplyr::left_join(iso3_fao_names, by = "Country.name") %>%
    dplyr::relocate("Country.code", .before = "Country.name")

  return(fao_df_with_iso3)

}


add_mw.region_codes <- function(fao_df_with_iso3, mw_mapping_path){

  mw.region_to_iso3 <- readxl::read_xlsx(path = mw_mapping_path,
                                         sheet = "MW_PFU") %>%
    dplyr::select(MW_region_code, `2019`) %>%
    magrittr::set_colnames(c("MW.Region.code", "Country.code"))

  fao_df_with_region <- fao_df_with_iso3 %>%
    dplyr::left_join(mw.region_to_iso3, by = "Country.code") %>%
    dplyr::relocate("MW.Region.code", .before = "Country.code")

  return(fao_df_with_region) # Doesn't include Afghanistan etc need complete concordance!


}


calc_working_animals <- function(working_species_data,
                                 mw_mapping_path,
                                 amw_analysis_path,
                                 year = MWTools::mw_constants$year,
                                 species = MWTools::mw_constants$species,
                                 prop_working_animal = MWTools::amw_analysis_constants$prop_working_animal,
                                 da_perc = MWTools::amw_analysis_constants$da_perc,
                                 working_animals_col = MWTools::amw_analysis_constants$working_animals_col,
                                 live_animals_col= MWTools::amw_analysis_constants$live_animals_col){

  # Reads the amw analysis excel file to determine the percentage of live animals
  # that are working animals.
  working_species_mapping <- readxl::read_excel(amw_analysis_path,
                                                sheet = da_perc) %>%
    tibble::tibble() %>%
    dplyr::select(-MW_region, -`Exemplar/Method`) %>% # add constants
    tidyr::pivot_longer(cols = `1960`:`2019`,
                        names_to = year,
                        values_to = prop_working_animal) %>% # ad constants
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]])
    )

  working_species_data %>%
    dplyr::left_join(working_species_mapping, by = c(species, "MW_region_code", year)) %>%
    dplyr::mutate(
      "{working_animals_col}" := .data[[live_animals_col]] * .data[[prop_working_animal]]
      )

}

calc_sector_split <- function(.df, amw_path) {

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
    dplyr::left_join(working_days, by = c("Species", "Region")) %>%
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

  tidy_data <- .df %>%
    dplyr::select(Continent, ISO_Country_Code, Year, Species,
                  Live_Animals, Working_Animals,
                  final_energy_total:primary_energy_tr,
                  useful_energy_total:useful_energy_tr) %>%
    tidyr::pivot_longer(cols = final_energy_total:useful_energy_tr,
                        names_to = c("stage", "sector"),
                        names_sep = "_energy_",
                        values_to = "Energy [J]")

  # tidy_data$stage <- factor(tidy_data$stage, levels = c("primary", "final", "useful"))

}

#' Calculate primary, final, and useful working animal energy
#'
#' This function calculates the total number of working animals and primary,
#' final, and useful working animal energy by country, and for six species:
#' Asses, Buffaloes, Camelids, Cattle, Horses, and Mules.
#'
#' @param amw_data_path
#' @param mw_mapping_path
#' @param amw_path
#'
#' @return
#' @export
#'
#' @examples
#' data <- calc_amw_pfu(amw_data_path, mw_mapping_path, amw_path) # Add paths here.
#'
calc_amw_pfu <- function(amw_data_path, mw_mapping_path, amw_path) {

  amw_pfu_data <- read_amw_data(amw_data_path) %>%
    tidy_trim_amw_data(mw_mapping_path) %>%
    calc_working_animals(mw_mapping_path, amw_path) %>%
    calc_work_split(amw_path) %>%
    calc_working_animals_split() %>%
    calc_yearly_feed(amw_path) %>%
    calc_final_energy() %>%
    calc_primary_energy() %>%
    calc_useful_work(amw_path) %>%
    tidy_amw_df()

  return(amw_pfu_data)

}

