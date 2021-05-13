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
                                                "Total.hours [hours/year]"))

  expect_equal(hmw_data_totalhours[1, "Total.hours [hours/year]"] %>% as.numeric(), 249693600)

})


test_that("get_broad.sector_data works",{

  hmw_data <- read.csv(file = hmw_test_data_path())

  hmw_data_broad.sector <- hmw_data %>%
    add_hmw_region_codes() %>%
    fill_ilo_data() %>%
    calc_total_hours_worked() %>%
    get_broad.sector_data()

  expect_true(!is.null(hmw_data_broad.sector))

  expect_equal(unique(hmw_data_broad.sector$Sector), c("Agriculture", "Industry",
                                                       "Non-agriculture", "Not classified",
                                                       "Services", "Total"))


})


test_that("add_hmw_analysis_sectors works",{

  hmw_data <- read.csv(file = hmw_test_data_path())

  hmw_data_w.hmwsectors <- hmw_data %>%
    add_hmw_region_codes() %>%
    fill_ilo_data() %>%
    calc_total_hours_worked() %>%
    get_broad.sector_data() %>%
    add_hmw_analysis_sectors()

  expect_true(!is.null(hmw_data_w.hmwsectors))

})

test_that("tidy_ilo_data works",{

  hmw_data <- read.csv(file = hmw_test_data_path())

  tidy_hmw_data <- hmw_data %>%
    tidy_ilo_data()

  expect_true(!is.null(tidy_hmw_data))

})


test_that("calc_hmw_final_energy works",{

  hmw_data <- read.csv(file = hmw_test_data_path())

  hmw_data_finalenergy <- hmw_data %>%
    tidy_ilo_data() %>%
    calc_hmw_final_energy()

  expect_true(!is.null(hmw_data_finalenergy))


})

test_that("calc_hmw_primary_energy works",{

  hmw_data <- read.csv(file = hmw_test_data_path())

  hmw_data_primaryenergy <- hmw_data %>%
    tidy_ilo_data() %>%
    calc_hmw_final_energy() %>%
    calc_hmw_primary_energy()

  expect_true(!is.null(hmw_data_primaryenergy))


})


test_that("calc_hmw_useful_energy works",{

  hmw_data <- read.csv(file = hmw_test_data_path())

  hmw_data_usefulenergy <- hmw_data %>%
    tidy_ilo_data() %>%
    calc_hmw_final_energy() %>%
    calc_hmw_primary_energy() %>%
    calc_hmw_useful_energy()

  expect_true(!is.null(hmw_data_usefulenergy))

})


test_that("calc_hmw_pfu works",{

  hmw_data <- read.csv(file = hmw_test_data_path())

  hmw_data_pfu <- hmw_data %>%
    calc_hmw_pfu()

  expect_true(!is.null(hmw_data_pfu))


})

