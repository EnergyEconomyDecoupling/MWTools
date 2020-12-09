# This script reads FAO data for the number of live animals which has been
# manually downloaded and placed in dropbox

library(tidyverse)

read_amw_data <- function (amw_data_path) {

  # Creates a file path to the FAO data in dropbox.
  # This data contains the number of live animals, per species, per country, by year
  amw_data_path <- amw_data_path

  # Reads the livestock data .csv file and creates a tibble
  animals_data_raw <- readr::read_csv(amw_data_path) %>%
    tibble::tibble()

  # Selects and renames the relevant columns
  animals_data_trimmed <- animals_data_raw %>%
    dplyr::select(c("Area Code", "Area", "Item", "Year", "Value")) %>%
    magrittr::set_colnames(c("ISO_Country_Code", "Country", "Species", "Year", "Live_Animals"))

  # Filters out "non-working" species
  working_species_animals <- animals_data_trimmed %>%
    dplyr::filter(Species %in% c("Asses",
                                 "Camels",
                                 "Cattle",
                                 "Horses",
                                 "Mules",
                                 "Buffaloes",
                                 "Camelids, other")) %>%
    tidyr::pivot_wider(names_from = Species,
                       values_from = Live_Animals) %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(Camelids = Camels + `Camelids, other`,
                  .keep = "unused") %>%
    tidyr::pivot_longer(cols = c("Asses", "Buffaloes", "Camelids", "Cattle", "Horses", "Mules"),
                        names_to = "Species",
                        values_to = "Live_Animals")

}
