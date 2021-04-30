test_that("get_working_species works", {

  test_data_path <- amw_test_data_path()

  live_animals_w.species <- tidy_fao_live_animals(data_folder = test_data_path) %>%
    add_concordance_codes() %>%
    trim_fao_data() %>%
    get_working_species()

  expect_true(!is.null(live_animals_w.species))

  expect_equal(unique(live_animals_w.species$Country.code), "CHN")

  expect_equal(nrow(live_animals_w.species), 354)

  expect_equal(unique(live_animals_w.species$Species),
               c("Asses", "Buffaloes", "Camelids", "Cattle", "Horses", "Mules"))

})


test_that("calc_working_animals works", {

  test_data_path <- amw_test_data_path()

  working_animals <- tidy_fao_live_animals(data_folder = test_data_path) %>%
    add_concordance_codes() %>%
    trim_fao_data() %>%
    get_working_species() %>%
    calc_working_animals()

  expect_true(!is.null(working_animals))

  expect_equal(nrow(working_animals), 354)

  expect_equal(colnames(working_animals), c("Country.code", "MW.Region.code",
                                            "Country.name", "Year", "Species",
                                            "Live.animals", "Prop.Working.animals",
                                            "Working.animals.total"))
})


test_that("calc_sector_split works", {

  test_data_path <- amw_test_data_path()

  working_animals_s.split <- tidy_fao_live_animals(data_folder = test_data_path) %>%
    add_concordance_codes() %>%
    trim_fao_data() %>%
    get_working_species() %>%
    calc_working_animals() %>%
    calc_sector_split()

  expect_true(!is.null(working_animals_s.split))

  expect_equal(nrow(working_animals_s.split), 354)

  expect_equal(colnames(working_animals_s.split), c("Country.code", "MW.Region.code",
                                                    "Country.name", "Year", "Species",
                                                    "Live.animals", "Prop.Working.animals",
                                                    "Working.animals.total", "Prop.Working.animals.Ag",
                                                    "Prop.Working.animals.Tr", "Working.animals.Ag",
                                                    "Working.animals.Tr"))
})

test_that("tidy_numbers_data works", {

  test_data_path <- amw_test_data_path()

  working_animals_tidy <- tidy_fao_live_animals(data_folder = test_data_path) %>%
    add_concordance_codes() %>%
    trim_fao_data() %>%
    get_working_species() %>%
    calc_working_animals() %>%
    calc_sector_split() %>%
    tidy_numbers_data()

  expect_true(!is.null(working_animals_tidy))

  expect_equal(nrow(working_animals_tidy), 1062)

  expect_equal(colnames(working_animals_tidy), c("MW.Region.code", "Country.code",
                                                    "Year", "Species", "Sector",
                                                    "Live.animals", "Working.animals"))
})

test_that("calc_amw_numbers works", {

  test_data_path <- amw_test_data_path()

  working_animals_tidy <- calc_amw_numbers(data_folder = test_data_path)

  expect_true(!is.null(working_animals_tidy))

  expect_equal(nrow(working_animals_tidy), 1062)

  expect_equal(colnames(working_animals_tidy), c("MW.Region.code", "Country.code",
                                                    "Year", "Species", "Sector",
                                                    "Live.animals", "Working.animals"))
})

test_that("calc_yearly_feed works", {

  test_data_path <- amw_test_data_path()

  working_animals_w.feed <- calc_amw_numbers(data_folder = test_data_path) %>%
    calc_yearly_feed()

  expect_true(!is.null(working_animals_w.feed))

  expect_equal(nrow(working_animals_w.feed), 1062)

  expect_equal(colnames(working_animals_w.feed), c("MW.Region.code", "Country.code",
                                                   "Year", "Species", "Sector",
                                                   "Live.animals", "Working.animals",
                                                   "Total.yearly.feed [MJ/year per animal]"))
})

test_that("calc_final_energy works", {

  test_data_path <- amw_test_data_path()

  working_animals_w.finalenergy <- calc_amw_numbers(data_folder = test_data_path) %>%
    calc_yearly_feed() %>%
    calc_final_energy() # Failing


  expect_true(!is.null(working_animals_w.finalenergy))

  expect_equal(nrow(working_animals_w.finalenergy), 1062)

  expect_equal(colnames(working_animals_w.feed), c("MW.Region.code", "Country.code",
                                                   "Year", "Species", "Sector",
                                                   "Live.animals", "Working.animals",
                                                   "Total.yearly.feed [MJ/year per animal]"))
})




# working_animals_w.finalenergy <- calc_amw_numbers(data_folder = test_data_path) %>%
#   calc_yearly_feed() %>%
#   dplyr::mutate(
#     final_energy_total = (working_animals_total_col * total_yearly_feed_col) * as.numeric(ge_de_ratio) * (1/(1 - as.numeric(trough_waste)))
#   ) %>%
#   dplyr::mutate(
#     final_energy_ag = (.data[[working_animals_ag_col]] * .data[[total_yearly_feed_col]]) * as.numeric(ge_de_ratio) * (1/(1 - as.numeric(trough_waste)))
#   ) %>%
#   dplyr::mutate(
#     final_energy_tr = (.data[[working_animals_tr_col]] * .data[[total_yearly_feed_col]]) * as.numeric(ge_de_ratio) * (1/(1 - as.numeric(trough_waste)))
#   )










