test_that("tidy_fao_live_animals works", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  live_animals <- test_data %>%
    tidy_fao_live_animals()

  expect_true(!is.null(live_animals))

  # expect_true(all(unique(live_animals[[MWTools::mw_constants$species]]) == c("Asses", "Buffaloes", "Camels", "Cattle", "Horses", "Mules and hinnies")))

  expect_equal(unique(live_animals[[MWTools::mw_constants$country_name]]), c("China", "China, mainland"))

  expect_equal(colnames(live_animals),
               c(MWTools::mw_constants$country_name,
                 MWTools::mw_constants$species,
                 MWTools::mw_cols$year,
                 MWTools::mw_constants$value))

  expect_equal(nrow(live_animals), 1827)

})


test_that("add_concordance_codes works", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  live_animals_w.codes <- test_data %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes()

  expect_true(!is.null(live_animals_w.codes))

  expect_equal(unique(live_animals_w.codes$Country.code), c("CHN", "CHNM"))

  expect_equal(colnames(live_animals_w.codes),
               c(MWTools::conc_cols$country_code_col,
                 MWTools::conc_cols$amw_region_code_col,
                 MWTools::mw_constants$country_name,
                 MWTools::mw_constants$species,
                 MWTools::mw_cols$year,
                 MWTools::mw_constants$value,
                 MWTools::conc_cols$country_incl_col))

  expect_equal(nrow(live_animals_w.codes), 1827)

})


test_that("trim_fao_data works", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  live_animals_trimmed <- test_data %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes() %>%
    trim_fao_data()

  expect_true(!is.null(live_animals_trimmed))

  expect_equal(unique(live_animals_trimmed$Country.code), "CHNM")

  expect_equal(nrow(live_animals_trimmed), 882)

})


test_that("get_working_species works with 'Camelids, other'", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path) |>
    dplyr::filter(.data[[MWTools::mw_constants$species]] != MWTools::mw_species$camelids_other)

  # "Camelids, other" are present for 2019 only by default, then removed in
  # tidy_fao_live_animals() as 1 data point cannot be interpolated or extrapolated,
  # add dummy test data
  test_data_camelids <- test_data |>
    dplyr::filter(.data[[MWTools::mw_constants$species]] != MWTools::mw_species$camelids_other) |>
    dplyr::filter(.data[[MWTools::mw_constants$species]] == MWTools::mw_species$camels) |>
    dplyr::mutate(
      "{MWTools::mw_constants$species}" := dplyr::case_when(
        .data[[MWTools::mw_constants$species]] == MWTools::mw_species$camels ~ MWTools::mw_species$camelids_other,
        TRUE ~ .data[[MWTools::mw_constants$species]]
      )
    )

  test_data <- rbind(test_data, test_data_camelids)

  live_animals_w.species <- test_data %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes() %>%
    trim_fao_data() %>%
    get_working_species()

  expect_true(!is.null(live_animals_w.species))

  expect_equal(unique(live_animals_w.species[[MWTools::conc_cols$country_code_col]]), "CHNM")

  expect_equal(nrow(live_animals_w.species), 378)

  expect_equal(unique(live_animals_w.species[[MWTools::mw_constants$species]]),
               c(MWTools::mw_species$asses,
                 MWTools::mw_species$buffaloes,
                 MWTools::mw_species$cattle,
                 MWTools::mw_species$horses,
                 MWTools::mw_species$mules,
                 MWTools::mw_species$camelids))
})


test_that("get_working_species works without 'Camelids, other'", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  live_animals_w.species <- test_data %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes() %>%
    trim_fao_data() %>%
    dplyr::filter(.data[[MWTools::mw_constants$species]] != MWTools::mw_species$camelids_other) %>% # "Camelids, other" are absent by default
    get_working_species()

  expect_true(!is.null(live_animals_w.species))

  expect_equal(unique(live_animals_w.species[[MWTools::conc_cols$country_code_col]]), "CHNM")

  expect_equal(nrow(live_animals_w.species), 378)

  expect_equal(unique(live_animals_w.species[[MWTools::mw_constants$species]]),
               c(MWTools::mw_species$asses,
                 MWTools::mw_species$buffaloes,
                 MWTools::mw_species$cattle,
                 MWTools::mw_species$horses,
                 MWTools::mw_species$mules,
                 MWTools::mw_species$camelids))
})


test_that("get_working_species works without 'Camelids, other' or 'Camels'.", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  live_animals_w.species <- test_data %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes() %>%
    trim_fao_data() %>%
    dplyr::filter(.data[[MWTools::mw_constants$species]] != MWTools::mw_species$camelids_other) %>%
    dplyr::filter(.data[[MWTools::mw_constants$species]] != MWTools::mw_species$camels) %>%
    get_working_species()

  expect_true(!is.null(live_animals_w.species))

  expect_equal(unique(live_animals_w.species[[MWTools::conc_cols$country_code_col]]), "CHNM")

  expect_equal(nrow(live_animals_w.species), 378)

  expect_equal(unique(live_animals_w.species[[MWTools::mw_constants$species]]),
               c(MWTools::mw_species$asses,
                 MWTools::mw_species$buffaloes,
                 MWTools::mw_species$cattle,
                 MWTools::mw_species$horses,
                 MWTools::mw_species$mules,
                 MWTools::mw_species$camelids))
})


test_that("calc_working_animals works", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  working_animals <- test_data %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes() %>%
    trim_fao_data() %>%
    get_working_species() %>%
    calc_working_animals()

  expect_true(!is.null(working_animals))

  expect_equal(nrow(working_animals), 378)

  expect_equal(unique(live_animals_w.species[[MWTools::mw_constants$species]]),
               c(MWTools::mw_species$asses,
                 MWTools::mw_species$buffaloes,
                 MWTools::mw_species$cattle,
                 MWTools::mw_species$horses,
                 MWTools::mw_species$mules,
                 MWTools::mw_species$camelids_other))

  expect_equal(colnames(working_animals),
               c(MWTools::conc_cols$country_code_col,
                 MWTools::conc_cols$amw_region_code_col,
                 MWTools::mw_cols$year,
                 MWTools::conc_cols$species,
                 MWTools::amw_analysis_constants$live_animals_col,
                 MWTools::amw_analysis_constants$prop_working_animals_col,
                 MWTools::amw_analysis_constants$working_animals_total_col))
})


test_that("calc_sector_split works", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  working_animals_s.split <- test_data %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes() %>%
    trim_fao_data() %>%
    get_working_species() %>%
    calc_working_animals() %>%
    calc_sector_split()

  expect_true(!is.null(working_animals_s.split))

  expect_equal(nrow(working_animals_s.split), 378)

  expect_equal(colnames(working_animals_s.split),
               c(MWTools::conc_cols$country_code_col,
                 MWTools::conc_cols$amw_region_code_col,
                 MWTools::mw_cols$year,
                 MWTools::conc_cols$species,
                 MWTools::amw_analysis_constants$live_animals_col,
                 MWTools::amw_analysis_constants$prop_working_animals_col,
                 MWTools::amw_analysis_constants$working_animals_total_col,
                 MWTools::amw_analysis_constants$prop_wkg_anmls_ag_col,
                 MWTools::amw_analysis_constants$prop_wkg_anmls_tr_col,
                 MWTools::amw_analysis_constants$working_animals_ag_col,
                 MWTools::amw_analysis_constants$working_animals_tr_col))
})


test_that("calc_yearly_feed works", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  working_animals_w.feed <- test_data %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes() %>%
    trim_fao_data() %>%
    get_working_species() %>%
    calc_working_animals() %>%
    calc_sector_split() %>%
    calc_yearly_feed()

  expect_true(!is.null(working_animals_w.feed))

  expect_equal(nrow(working_animals_w.feed), 378)

  expect_equal(colnames(working_animals_w.feed), c("Country.code", "AMW.Region.code",
                                                   "Year", "Species", "Live.animals",
                                                   "Prop.Working.animals", "Working.animals.total",
                                                   "Prop.Working.animals.Ag", "Prop.Working.animals.Tr",
                                                   "Working.animals.Ag", "Working.animals.Tr",
                                                   "Total.yearly.feed [MJ/year per animal]"))
})


test_that("calc_final_energy works", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  working_animals_w.finalenergy <- test_data %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes() %>%
    trim_fao_data() %>%
    get_working_species() %>%
    calc_working_animals() %>%
    calc_sector_split() %>%
    calc_yearly_feed() %>%
    calc_final_energy()

  expect_true(!is.null(working_animals_w.finalenergy))

  expect_equal(nrow(working_animals_w.finalenergy), 378)

  expect_equal(colnames(working_animals_w.finalenergy), c("Country.code", "AMW.Region.code",
                                                          "Year", "Species", "Live.animals",
                                                          "Prop.Working.animals", "Working.animals.total",
                                                          "Prop.Working.animals.Ag", "Prop.Working.animals.Tr",
                                                          "Working.animals.Ag", "Working.animals.Tr",
                                                          "Total.yearly.feed [MJ/year per animal]",
                                                          "Final.energy.total [MJ/year]", "Final.energy.Ag [MJ/year]",
                                                          "Final.energy.Tr [MJ/year]"))
})


test_that("calc_primary_energy",{

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  working_animals_w.primaryenergy <- test_data %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes() %>%
    trim_fao_data() %>%
    get_working_species() %>%
    calc_working_animals() %>%
    calc_sector_split() %>%
    calc_yearly_feed() %>%
    calc_final_energy() %>%
    calc_primary_energy()

  expect_true(!is.null(working_animals_w.primaryenergy))

  expect_equal(nrow(working_animals_w.primaryenergy), 378)

  expect_equal(colnames(working_animals_w.primaryenergy), c("Country.code", "AMW.Region.code",
                                                            "Year", "Species", "Live.animals",
                                                            "Prop.Working.animals", "Working.animals.total",
                                                            "Prop.Working.animals.Ag", "Prop.Working.animals.Tr",
                                                            "Working.animals.Ag", "Working.animals.Tr",
                                                            "Total.yearly.feed [MJ/year per animal]",
                                                            "Final.energy.total [MJ/year]", "Final.energy.Ag [MJ/year]",
                                                            "Final.energy.Tr [MJ/year]", "Primary.energy.total [MJ/year]",
                                                            "Primary.energy.Ag [MJ/year]", "Primary.energy.Tr [MJ/year]"))
})


test_that("calc_useful_energy",{

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  working_animals_w.usefulenergy <- test_data %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes() %>%
    trim_fao_data() %>%
    get_working_species() %>%
    calc_working_animals() %>%
    calc_sector_split() %>%
    calc_yearly_feed() %>%
    calc_final_energy() %>%
    calc_primary_energy() %>%
    calc_useful_energy()

  expect_true(!is.null(working_animals_w.usefulenergy))

  expect_equal(nrow(working_animals_w.usefulenergy), 378)

  expect_equal(colnames(working_animals_w.usefulenergy), c("Country.code", "AMW.Region.code",
                                                           "Year", "Species", "Live.animals",
                                                           "Prop.Working.animals", "Working.animals.total",
                                                           "Prop.Working.animals.Ag", "Prop.Working.animals.Tr",
                                                           "Working.animals.Ag", "Working.animals.Tr",
                                                           "Total.yearly.feed [MJ/year per animal]",
                                                           "Final.energy.total [MJ/year]", "Final.energy.Ag [MJ/year]",
                                                           "Final.energy.Tr [MJ/year]", "Primary.energy.total [MJ/year]",
                                                           "Primary.energy.Ag [MJ/year]", "Primary.energy.Tr [MJ/year]",
                                                           "Power.per.animal [W]", "Working.seconds [seconds per animal]",
                                                           "Useful.energy.total [MJ/year]", "Useful.energy.Ag [MJ/year]",
                                                           "Useful.energy.Tr [MJ/year]"))
})


test_that("tidy_pfu_data works", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  tidy_pfu_data <- test_data %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes() %>%
    trim_fao_data() %>%
    get_working_species() %>%
    calc_working_animals() %>%
    calc_sector_split() %>%
    calc_yearly_feed() %>%
    calc_final_energy() %>%
    calc_primary_energy() %>%
    calc_useful_energy() %>%
    tidy_pfu_data()

  expect_true(!is.null(tidy_pfu_data))

  expect_equal(nrow(tidy_pfu_data), 2268)

  expect_equal(colnames(tidy_pfu_data), c("Country", "Year", "Species",
                                          "Stage", "Sector", "Unit",
                                          "Edot"))

})


test_that("calc_amw_pfu", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  tidy_pfu_data <- test_data %>%
    calc_amw_pfu()

  expect_true(!is.null(tidy_pfu_data))

  expect_equal(nrow(tidy_pfu_data), 2268)

  expect_equal(colnames(tidy_pfu_data), c("Country", "Year", "Species",
                                          "Stage", "Sector", "Unit",
                                          "Edot"))
})



