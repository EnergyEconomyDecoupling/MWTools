test_that("prepareRawILOData() works", {
  test_ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
  test_ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())

  rawILOData <- prepareRawILOData(ilo_working_hours_data = test_ilo_working_hours_data,
                                  ilo_employment_data = test_ilo_employment_data)

  expected_colnames <- c("Country.code", "Sex", "Sector", "Year", "Employed.persons [persons]", "Working.hours [hours/year]")
  expected_nrow <- 4501
  expected_countrycodes <- "GBR"

  expect_true(!is.null(rawILOData))
  expect_equal(expected_colnames, colnames(rawILOData))
  expect_equal(expected_nrow, nrow(rawILOData))
  expect_equal(expected_countrycodes, unique(rawILOData$Country.code))
})


test_that("add_hmw_region_codes() works", {
  test_ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
  test_ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())

  hmw_data <- prepareRawILOData(ilo_working_hours_data = test_ilo_working_hours_data,
                                ilo_employment_data = test_ilo_employment_data)

  hmw_data_w.codes <- hmw_data %>%
    add_hmw_region_codes()
  expect_true(!is.null(hmw_data_w.codes))
  expect_true(unique(hmw_data_w.codes$HMW.Region.code) == "WE")
  expect_equal(colnames(hmw_data_w.codes), c("Country", "HMW.Region.code", "Sex",
                                             "Sector", "Year", "Employed.persons [persons]",
                                             "Working.hours [hours/year]"))
  expect_equal(nrow(hmw_data_w.codes), 4501)
  expect_equal(nrow(dplyr::filter(hmw_data_w.codes,
                                  Country == "GBR",
                                  Sex == "Female",
                                  Sector == "Economic activity (Aggregate): Construction",
                                  Year == 1983)),
               1) # was 0 for v1.1
})


test_that("fill_ilo_data() works", {
  test_ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
  test_ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())

  hmw_data <- prepareRawILOData(ilo_working_hours_data = test_ilo_working_hours_data,
                                ilo_employment_data = test_ilo_employment_data)

  hmw_data_filled <- hmw_data %>%
    add_hmw_region_codes() %>%
    fill_ilo_data()
  expect_true(!is.null(hmw_data_filled))
  expect_equal(nrow(dplyr::filter(hmw_data_filled ,
                                  Sector == "Economic activity (ISIC-Rev.2): 0. Activities not Adequately Defined")),
               0)
  expect_equal(nrow(hmw_data_filled), 10187) # v1.1 10248
  expect_equal(colnames(hmw_data_filled), c("Country", "HMW.Region.code", "Sex",
                                            "Sector", "Year", "Employed.persons [persons]",
                                            "Working.hours [hours/year]"))
  expect_equal(hmw_data_filled[1, "Employed.persons [persons]"] %>% as.numeric(), 151000)
  expect_equal(hmw_data_filled[1, "Working.hours [hours/year]"] %>% as.numeric(), 1589.64)
})


test_that("calc_total_hours_worked() works", {
  test_ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
  test_ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())

  hmw_data <- prepareRawILOData(ilo_working_hours_data = test_ilo_working_hours_data,
                                ilo_employment_data = test_ilo_employment_data)

  hmw_data_totalhours <- hmw_data %>%
    add_hmw_region_codes() %>%
    fill_ilo_data() %>%
    calc_total_hours_worked()
  expect_true(!is.null(hmw_data_totalhours))
  expect_equal(colnames(hmw_data_totalhours), c("Country", "HMW.Region.code", "Sex",
                                                "Sector", "Year", "Employed.persons [persons]",
                                                "Total.hours [hours/year]"))
  expect_equal(hmw_data_totalhours[1, "Total.hours [hours/year]"] %>% as.numeric(), 240035640)
})


test_that("get_broad.sector_data() works",{
  test_ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
  test_ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())

  hmw_data <- prepareRawILOData(ilo_working_hours_data = test_ilo_working_hours_data,
                                ilo_employment_data = test_ilo_employment_data)

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


test_that("split_labor_by_sector() works",{
  test_ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
  test_ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())

  hmw_data <- prepareRawILOData(ilo_working_hours_data = test_ilo_working_hours_data,
                                ilo_employment_data = test_ilo_employment_data)

  hmw_data_w.laborsplit <- hmw_data %>%
    add_hmw_region_codes() %>%
    fill_ilo_data() %>%
    calc_total_hours_worked() %>%
    get_broad.sector_data() %>%
    split_labor_by_sector()
  expect_true(!is.null(hmw_data_w.laborsplit))
})


test_that("calc_hmw_final_energy() works",{
  test_ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
  test_ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())

  hmw_data <- prepareRawILOData(ilo_working_hours_data = test_ilo_working_hours_data,
                                ilo_employment_data = test_ilo_employment_data)

  hmw_data_finalenergy <- hmw_data %>%
    add_hmw_region_codes() %>%
    fill_ilo_data() %>%
    calc_total_hours_worked() %>%
    get_broad.sector_data() %>%
    split_labor_by_sector() %>%
    calc_hmw_final_energy()
  expect_true(!is.null(hmw_data_finalenergy))
})


test_that("calc_hmw_primary_energy() works",{
  test_ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
  test_ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())

  hmw_data <- prepareRawILOData(ilo_working_hours_data = test_ilo_working_hours_data,
                                ilo_employment_data = test_ilo_employment_data)

  hmw_data_primaryenergy <- hmw_data %>%
    add_hmw_region_codes() %>%
    fill_ilo_data() %>%
    calc_total_hours_worked() %>%
    get_broad.sector_data() %>%
    split_labor_by_sector() %>%
    calc_hmw_final_energy() %>%
    calc_hmw_primary_energy()
  expect_true(!is.null(hmw_data_primaryenergy))
})


test_that("calc_hmw_useful_energy() works",{
  test_ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
  test_ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())

  hmw_data <- prepareRawILOData(ilo_working_hours_data = test_ilo_working_hours_data,
                                ilo_employment_data = test_ilo_employment_data)

  hmw_data_usefulenergy <- hmw_data %>%
    add_hmw_region_codes() %>%
    fill_ilo_data() %>%
    calc_total_hours_worked() %>%
    get_broad.sector_data() %>%
    split_labor_by_sector() %>%
    calc_hmw_final_energy() %>%
    calc_hmw_primary_energy() %>%
    calc_hmw_useful_energy()
  expect_true(!is.null(hmw_data_usefulenergy))
  # As of 11 May 2022, we are getting some NA energy values,
  # which is probably not correct.
  # So build a test for this.
  expect_false(any(is.na(hmw_data_usefulenergy$`Useful energy [MJ/year]`)))
})


test_that("tidy_hmw_data() works",{
  test_ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
  test_ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())

  hmw_data <- prepareRawILOData(ilo_working_hours_data = test_ilo_working_hours_data,
                                ilo_employment_data = test_ilo_employment_data)

  tidy_hmw_data <- hmw_data %>%
    add_hmw_region_codes() %>%
    fill_ilo_data() %>%
    calc_total_hours_worked() %>%
    get_broad.sector_data() %>%
    split_labor_by_sector() %>%
    calc_hmw_final_energy() %>%
    calc_hmw_primary_energy() %>%
    calc_hmw_useful_energy() %>%
    tidy_hmw_pfu()
  expect_true(!is.null(tidy_hmw_data))
  # As of 11 May 2022, we are getting some NA energy values,
  # which is probably not correct.
  # So build a test for this.
  expect_false(any(is.na(tidy_hmw_data[[MWTools::mw_cols$e_dot]])))
})


test_that("calc_hmw_pfu() works",{
  test_ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
  test_ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())

  hmw_data <- prepareRawILOData(ilo_working_hours_data = test_ilo_working_hours_data,
                                ilo_employment_data = test_ilo_employment_data)

  hmw_data_pfu <- hmw_data %>%
    calc_hmw_pfu()
  expect_true(!is.null(hmw_data_pfu))
  # As of 11 May 2022, we are getting some NA energy values,
  # which is probably not correct.
  # So build a test for this.
  expect_false(any(is.na(hmw_data_pfu[[MWTools::mw_cols$e_dot]])))
})


test_that("calc_hmw_pfu() no longer throws a warning", {
  # These function calls are in a chain that throws a warning.
  # Go through them one-by-one to find the problem.

  concordance_path <- MWTools::fao_concordance_path()
  hmw_analysis_data_path <- MWTools::hmw_analysis_data_path()

  test_ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
  test_ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())

  pfu_energy_data <- prepareRawILOData(ilo_working_hours_data = test_ilo_working_hours_data,
                                       ilo_employment_data = test_ilo_employment_data)


  expect_no_warning(pfu_energy_data %>%
                      add_hmw_region_codes(concordance_path = concordance_path))

  expect_no_warning(pfu_energy_data %>%
                      add_hmw_region_codes(concordance_path = concordance_path) %>%
                      fill_ilo_data())

  expect_no_warning(pfu_energy_data %>%
                      add_hmw_region_codes(concordance_path = concordance_path) %>%
                      fill_ilo_data() %>%
                      calc_total_hours_worked())

  expect_no_warning(pfu_energy_data %>%
                      add_hmw_region_codes(concordance_path = concordance_path) %>%
                      fill_ilo_data() %>%
                      calc_total_hours_worked() %>%
                      get_broad.sector_data())

  expect_no_warning(pfu_energy_data %>%
                      add_hmw_region_codes(concordance_path = concordance_path) %>%
                      fill_ilo_data() %>%
                      calc_total_hours_worked() %>%
                      get_broad.sector_data() %>%
                      split_labor_by_sector(hmw_analysis_data_path = hmw_analysis_data_path))

  expect_no_warning(pfu_energy_data %>%
                      add_hmw_region_codes(concordance_path = concordance_path) %>%
                      fill_ilo_data() %>%
                      calc_total_hours_worked() %>%
                      get_broad.sector_data() %>%
                      split_labor_by_sector(hmw_analysis_data_path = hmw_analysis_data_path) %>%
                      calc_hmw_final_energy(hmw_analysis_data_path = hmw_analysis_data_path))

  expect_no_warning(pfu_energy_data %>%
                      add_hmw_region_codes(concordance_path = concordance_path) %>%
                      fill_ilo_data() %>%
                      calc_total_hours_worked() %>%
                      get_broad.sector_data() %>%
                      split_labor_by_sector(hmw_analysis_data_path = hmw_analysis_data_path) %>%
                      calc_hmw_final_energy(hmw_analysis_data_path = hmw_analysis_data_path) %>%
                      calc_hmw_primary_energy(hmw_analysis_data_path = hmw_analysis_data_path))

  expect_no_warning(pfu_energy_data %>%
                      add_hmw_region_codes(concordance_path = concordance_path) %>%
                      fill_ilo_data() %>%
                      calc_total_hours_worked() %>%
                      get_broad.sector_data() %>%
                      split_labor_by_sector(hmw_analysis_data_path = hmw_analysis_data_path) %>%
                      calc_hmw_final_energy(hmw_analysis_data_path = hmw_analysis_data_path) %>%
                      calc_hmw_primary_energy(hmw_analysis_data_path = hmw_analysis_data_path) %>%
                      calc_hmw_useful_energy(hmw_analysis_data_path = hmw_analysis_data_path))

  expect_no_warning(pfu_energy_data %>%
                      add_hmw_region_codes(concordance_path = concordance_path) %>%
                      fill_ilo_data() %>%
                      calc_total_hours_worked() %>%
                      get_broad.sector_data() %>%
                      split_labor_by_sector(hmw_analysis_data_path = hmw_analysis_data_path) %>%
                      calc_hmw_final_energy(hmw_analysis_data_path = hmw_analysis_data_path) %>%
                      calc_hmw_primary_energy(hmw_analysis_data_path = hmw_analysis_data_path) %>%
                      calc_hmw_useful_energy(hmw_analysis_data_path = hmw_analysis_data_path) %>%
                      tidy_hmw_pfu())

  # The whole function should also not produce a warning.
  expect_no_warning(pfu_energy_data %>%
                      calc_hmw_pfu(concordance_path = concordance_path, hmw_analysis_data_path = hmw_analysis_data_path))

})

