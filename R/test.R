data <- calc_amw_pfu(amw_data_path = paste(PFUSetup::get_abs_paths()$project_path,
                                           "/Data/FAO Data/FAOSTAT_data_10-9-2020.csv", sep = ""),
                     mw_mapping_path = paste(PFUSetup::get_abs_paths()$project_path,
                                             "/Mapping/MW_mapping.xlsx", sep = ""),
                     amw_path = paste(PFUSetup::get_abs_paths()$project_path,
                                      "/Muscle work/amw_master_data.xlsx", sep = ""))


plot <- amw_plot(.data = data, country = "IND", sector = "total")
plot
