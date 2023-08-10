# This script uses the R package {FAOSTAT} to download data for the number of live
# animals and the R package {Rilostat} to download data for the number of
# employed persons and number of working hours

library(FAOSTAT)
library(Rilostat)

# Establish the version folder in which to save the raw data
version <- "v1.2"

# Establish FAO constants
download_location <- tempdir()
live_animals_code <- "QCL"
# download_filename <- ""

# Download .zip file containing data for the number of live animals into a specified folder
fao_amw_data <- FAOSTAT::get_faostat_bulk(code = live_animals_code,
                                          data_folder = download_location)

# Save downloaded file
saveRDS(object = fao_amw_data,
        file = PFUSetup::get_abs_paths(version = version)$fao_data_path)


# Establish ILO constants
working_hours_code <- MWTools::ilo_codes$working_hours_code
employment_code <- MWTools::ilo_codes$employment_code

ilo_working_hours_data <- Rilostat::get_ilostat(id = working_hours_code,
                                               quiet = TRUE) |>
  Rilostat::label_ilostat(code = c("ref_area"))

ilo_employment_data <- Rilostat::get_ilostat(id = employment_code,
                                             quiet = TRUE) |>
  Rilostat::label_ilostat(code = c("ref_area"))

saveRDS(object = ilo_employment_data,
        file = PFUSetup::get_abs_paths(version = version)$ilo_employment_data_path)

saveRDS(object = ilo_working_hours_data,
        file = PFUSetup::get_abs_paths(version = version)$ilo_working_hours_data_path)
