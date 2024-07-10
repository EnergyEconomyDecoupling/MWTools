library(FAOSTAT)
library(Rilostat)

# Download FAO Data, Save to Dropbox --------------------------------------
# Establish the version folder in which to save the raw data
version <- "v1.2"

# Establish FAO constants
download_location <- tempdir()
live_animals_code <- "QCL"

# Download .zip file containing data for the number of live animals into a specified folder
fao_amw_data <- FAOSTAT::get_faostat_bulk(code = live_animals_code,
                                          data_folder = download_location)

# Save downloaded file
saveRDS(object = fao_amw_data,
        file = PFUSetup::get_abs_paths(version = version)[["fao_data_path"]])


# Prepare FAO Test Data ---------------------------------------------------
fao_amw_data <- readRDS(file = PFUSetup::get_abs_paths(version = version)[["fao_data_path"]])

amw_test_data <- fao_amw_data |>
  dplyr::filter(area %in% c("China, mainland", "China"))

write.csv(x = amw_test_data, file = "./inst/extdata/test_data/test_amw_data.csv")


# Download ILO Data, Save to Dropbox --------------------------------------
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


# Prepare ILO Test Data ---------------------------------------------------
ilo_working_hours_data_path <- PFUSetup::get_abs_paths(version = version)[["ilo_working_hours_data_path"]]
ilo_employment_data_path <- PFUSetup::get_abs_paths(version = version)[["ilo_employment_data_path"]]

ilo_working_hours_test_data <- readr::read_rds(ilo_working_hours_data_path) |>
  dplyr::filter(ref_area == "GBR")

ilo_employment_test_data <- readr::read_rds(ilo_employment_data_path) |>
  dplyr::filter(ref_area == "GBR")

write.csv(x = ilo_working_hours_test_data, file = "inst/extdata/test_data/test_ilo_working_hours_data.csv")
write.csv(x = ilo_employment_test_data, file = "inst/extdata/test_data/test_ilo_employment_data.csv")


