test_that("add_hmw_region_codes works", {

  hmw_data <- read.csv(file = hmw_test_data_path())

  hmw_data_w.codes <- hmw_data %>%
    add_hmw_region_codes()

  expect_true(!is.null(hmw_data_w.codes))

  expect_true(unique(hmw_data_w.codes$HMW.Region.code) == "WE")

  expect_equal(colnames(hmw_data_w.codes), c("Country.code", "HMW.Region.code", "Sex",
                                              "Sector", "Year", "Employed.persons [persons]",
                                              "Working.hours [hours/year]"))


})


test_that("fill_ilo_data works", {

  hmw_data <- read.csv(file = hmw_test_data_path())

  hmw_data_filled <- hmw_data %>%
    add_hmw_region_codes() %>%
    fill_ilo_data()

  expect_true(!is.null(hmw_data_filled))

  expect_equal(colnames(hmw_data_filled), c("Country.code", "HMW.Region.code", "Sex",
                                            "Sector", "Year", "Employed.persons [persons]",
                                            "Working.hours [hours/year]"))

  expect_equal(hmw_data_filled[1, "Employed.persons [persons]"] %>% as.numeric(), 151000)

  expect_equal(hmw_data_filled[1, "Working.hours [hours/year]"] %>% as.numeric(), 1653.6)


})


test_that("calc_total_hours_worked works", {

  hmw_data <- read.csv(file = hmw_test_data_path())

  hmw_data_totalhours <- hmw_data %>%
    add_hmw_region_codes() %>%
    fill_ilo_data() %>%
    calc_total_hours_worked()

  expect_true(!is.null(hmw_data_totalhours))

  expect_equal(colnames(hmw_data_totalhours), c("Country.code", "HMW.Region.code", "Sex",
                                                "Sector", "Year", "Employed.persons [persons]",
                                                "Working.hours [hours/year]", "Total.hours [hours/year]"))

  expect_equal(hmw_data_totalhours[1, "Total.hours [hours/year]"] %>% as.numeric(), 249693600)

})
