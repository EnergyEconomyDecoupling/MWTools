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
                     employed_persons = "Employed.persons"
                     )

usethis::use_data(mw_constants, overwrite = TRUE)
