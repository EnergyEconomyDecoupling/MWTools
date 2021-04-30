test_that("fao_concordance_path works", {

  test_path <- fao_concordance_path()
  expect_true(endsWith(test_path, file.path("extdata",
                                            "FAO_ISO_MW_Mapping.xlsx")))

})

test_that("amw_analysis_data_path works", {

  test_path <- amw_analysis_data_path()
  expect_true(endsWith(test_path, file.path("extdata",
                                            "amw_analysis_data.xlsx")))

})

test_that("amw_test_data_path works", {

  test_path <- amw_test_data_path()
  expect_true(endsWith(test_path, file.path("extdata",
                                            "test_data")))

})

test_that("hmw_analysis_data_path works", {

  test_path <- hmw_analysis_data_path()
  expect_true(endsWith(test_path, file.path("extdata",
                                            "hmw_analysis_data.xlsx")))

})

