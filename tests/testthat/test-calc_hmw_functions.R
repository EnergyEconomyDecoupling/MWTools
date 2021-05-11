test_that("add_amw_region_codes works", {

  test_data <- read.csv(file = hmw_test_data_path())

  test_data_w.codes <- test_data %>%
    add_hmw_region_codes()

  expect_true(unique(test_data_w.codes$HMW.Region.code) == "WE")

  expect_equal(colnames(test_data_w.codes), c("Country.code", "HMW.Region.code", "Sex",
                                              "Sector", "Year", "Employed.persons [persons]",
                                              "Working.hours [hours/year]"))


})


test_that("fill_ilo_data works", {

  test_data <-

})
