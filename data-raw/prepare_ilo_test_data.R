ilo_workingHours_data_path <- PFUSetup::get_abs_paths(version = "v1.2")[["ilo_workingHours_data_path"]]
ilo_employment_data_path <- PFUSetup::get_abs_paths(version = "v1.2")[["ilo_employment_data_path"]]


ilo_workingHours_test_data <- readr::read_rds(ilo_workingHours_data_path) |>
  dplyr::filter(ref_area == "GBR")


ilo_employment_test_data <- readr::read_rds(ilo_employment_data_path) |>
  dplyr::filter(ref_area == "GBR")


write.csv(x = ilo_workingHours_test_data, file = "inst/extdata/test_data/test_ilo_workingHours_data.csv")
write.csv(x = ilo_employment_test_data, file = "inst/extdata/test_data/test_ilo_employment_data.csv")
