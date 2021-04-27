### SOURCE this file when adding new constants ###
### Document constants in R/Data.R script ###

#
# Constants for use in MWTools package
#

mw_constants <- list(country_name = "Country.name",
                     year = "Year",
                     species = "Species",
                     unit = "Unit",
                     value = "Value")

usethis::use_data(mw_constants, overwrite = TRUE)


#
# ILO data columns
#
ilo_cols <- list(sex_ilo_col = "Sex",
                 sector_ilo_col = "Sector",
                 working_hours_ilo_col = "Working.hours",
                 employed_persons_ilo_col = "Employed.persons")

usethis::use_data(ilo_cols, overwrite = TRUE)

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

#
# ILO data codes
#
ilo_codes <- list(working_hours_code = "HOW_TEMP_SEX_ECO_NB_A",
                  employment_code = "EMP_TEMP_SEX_ECO_NB_A")

usethis::use_data(ilo_codes, overwrite = TRUE)

#
# Animal Muscle Work (amw) analysis data constants
#
amw_analysis_constants <- list(prop_working_animal = "Prop_Working_Animal",
                               da_perc = "DA_perc",
                               working_animals_col = "Working.animals",
                               live_animals_col = "Live.animals")

usethis::use_data(amw_analysis_constants, overwrite = TRUE)


