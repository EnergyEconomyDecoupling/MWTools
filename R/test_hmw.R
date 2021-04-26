
hmw_data <- download_hmw_data()

hmw_data_trimmed <- hmw_data %>%
  trim_tidy_hmw_data()

hmw_data_1 <- hmw_data_trimmed %>%
  calc_workers_sector()

GBR <- hmw_data_1 %>%
  dplyr::filter(ISO_Country_Code == "GBR")
