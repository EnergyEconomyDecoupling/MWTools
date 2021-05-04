# This script uses the R package FAOSTAT to download data for the number of live
# animals in the form of a

# Establish constants
download_location <- tempdir()
live_animals_code <- MWTools::fao_codes$live_animals_code
download_filename <- "Production_Livestock_E_All_Data_(Normalized).zip"

# Download .zip file containing data for the number of live animals into a specified folder
fao_amw_data <- FAOSTAT::get_faostat_bulk(code = live_animals_code, data_folder = download_location)

# Use amw data in package
usethis::use_data(fao_amw_data, overwrite = TRUE)

# Remove file from temporary directory
file.remove(file.path(download_location, download_filename))
