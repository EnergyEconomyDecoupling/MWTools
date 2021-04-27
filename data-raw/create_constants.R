### SOURCE this file when adding new constants ###
### Document constants in R/Data.R script ###

#
# Constants for use in MWTools package
#
mw_constants <- list(country_name = "Country.name",
                     sex = "Sex",
                     sector = "Sector",
                     year = "Year",
                     working_hours = "Working.hours",
                     employed_persons = "Employed.persons",
                     species = "Species",
                     unit = "Unit",
                     value = "Value")

usethis::use_data(mw_constants, overwrite = TRUE)

#
# Working animal species
#
mw_species <- list(asses = "Asses",
                   camels = "Camels",
                   cattle = "Cattle",
                   horses = "Horses",
                   mules = "Mules",
                   buffaloes = "Buffaloes",
                   camelids_other = "Camelids, other",
                   camelids = "Camelids",
                   humans = "Humans")

usethis::use_data(mw_species, overwrite = TRUE)

#
# FAO data columns
#
fao_cols <- list(area_fao_col = "area",
                 item_fao_col = "item",
                 year_fao_col = "year",
                 unit_fao_col = "unit",
                 value_fao_col = "value")

usethis::use_data(fao_cols, overwrite = TRUE)

#
# FAO data codes
#
fao_codes <- list(live_animals_code = "QA")

usethis::use_data(fao_codes, overwrite = TRUE)
