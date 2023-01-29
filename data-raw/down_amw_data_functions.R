# This script uses the R package FAOSTAT to download data for the number of live
# animals in the form of a

library(FAOSTAT)

# Establish constants
download_location <- tempdir()
# live_animals_code <- MWTools::fao_codes$live_animals_code
live_animals_code <- "QCL"
download_filename <- ""

# Download .zip file containing data for the number of live animals into a specified folder
fao_amw_data <- FAOSTAT::get_faostat_bulk(code = live_animals_code, data_folder = download_location)

# Save downloaded file
# saveRDS(object = fao_amw_data,
#         file = PFUSetup::get_abs_paths()$fao_data_path)
