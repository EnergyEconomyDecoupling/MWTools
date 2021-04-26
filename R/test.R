amw_data_path <- paste(PFUSetup::get_abs_paths()$project_path,
                       "/Data/FAO Data/FAOSTAT_data_10-9-2020.csv", sep = "")

mw_mapping_path <- paste(PFUSetup::get_abs_paths()$project_path,
                         "/Mapping/MW_mapping.xlsx", sep = "")

amw_path <- paste(PFUSetup::get_abs_paths()$project_path,
                  "/Muscle work/amw_master_data.xlsx", sep = "")

data <- read_amw_data(amw_data_path) %>%
  tidy_trim_amw_data(mw_mapping_path) %>%
  calc_working_animals(mw_mapping_path, amw_path) %>%
  calc_work_split(amw_path) %>%
  calc_working_animals_split() %>%
  calc_yearly_feed(amw_path) %>%
  calc_final_energy() %>%
  calc_primary_energy() %>%
  calc_useful_work(amw_path) %>%
  tidy_amw_df()



data <- calc_amw_pfu(amw_data_path = paste(PFUSetup::get_abs_paths()$project_path,
                                           "/Data/FAO Data/FAOSTAT_data_10-9-2020.csv", sep = ""),
                     mw_mapping_path = paste(PFUSetup::get_abs_paths()$project_path,
                                             "/Mapping/MW_mapping.xlsx", sep = ""),
                     amw_path = paste(PFUSetup::get_abs_paths()$project_path,
                                      "/Muscle work/amw_master_data.xlsx", sep = ""))

# List unique continents
unique(data$Continent)

# List unique countries
unique(data$ISO_Country_Code)


plot_data <- data %>%
  dplyr::filter(ISO_Country_Code == "CHN", sector == "total") %>%
  as.data.frame()


pfu_plot <- ggplot2::ggplot() +
  ggplot2::geom_line(data = plot_data, mapping =  aes(x = Year, y = `Energy [J]`, color = stage)) +
  ggplot2::facet_wrap(facets = "Species", scales = "free_y") +
  labs(color = "ECC Stage") +
  labs(title = "CHN", subtitle = "total")

wa_plot <- ggplot2::ggplot() +
  ggplot2::geom_line(data = plot_data, aes(x = Year, y = Working_Animals, color = "Working")) +
  ggplot2::geom_line(data = plot_data, aes(x = Year, y = Live_Animals, color = "Live")) +
  ggplot2::facet_wrap(facets = "Species", scales = "free_y") +
  labs(y = "Number of Animals", color = "Metric") +
  labs(title = "CHN", subtitle = "total")

cowplot::plot_grid(pfu_plot, wa_plot, ncol = 1)
