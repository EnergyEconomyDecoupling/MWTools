test_that("tidy_fao_live_animals works", {

  test_data_path <- amw_test_data_path()

  live_animals <- tidy_fao_live_animals(data_folder = test_data_path)

  expect_true(!is.null(live_animals))

  expect_equal(unique(live_animals$Country.name), c("China, mainland", "China"))

  expect_equal(colnames(live_animals_w.codes), c("Country.name", "Species", "Year",
                                                 "Value"))

  expect_equal(nrow(live_animals), 2184)

})


test_that("add_concordance_codes works", {

  test_data_path <- amw_test_data_path()

  live_animals_w.codes <- tidy_fao_live_animals(data_folder = test_data_path) %>%
    add_concordance_codes()

  expect_true(!is.null(live_animals_w.codes))

  expect_equal(unique(live_animals_w.codes$Country.code), c("CHN", "-"))

  expect_equal(colnames(live_animals_w.codes), c("Country.code", "MW.Region.code",
                                                 "Country.name", "Species", "Year",
                                                 "Value", "Country.incl.", "Country.code_PFU"))

  expect_equal(nrow(live_animals_w.codes), 2184)

})


test_that("trim_fao_data works", {

  test_data_path <- amw_test_data_path()

  live_animals_trimmed <- tidy_fao_live_animals(data_folder = test_data_path) %>%
    add_concordance_codes() %>%
    trim_fao_data()

  expect_true(!is.null(live_animals_trimmed))

  expect_equal(unique(live_animals_trimmed$Country.code), "CHN")

  expect_equal(nrow(live_animals_trimmed), 1092)

})
