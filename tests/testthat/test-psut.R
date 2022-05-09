test_that("prep_mw_df() works as expected", {
  hmw_df <- hmw_test_data_path() %>%
    read.csv() %>%
    calc_hmw_pfu() %>%
    specify_product() %>%
    MWTools::specify_primary_production()

  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu() %>%
    specify_product() %>%
    MWTools::specify_primary_production()

  hmw_specified <- hmw_df %>%
    prep_mw_df()
  amw_specified <- amw_df %>%
    prep_mw_df()

})
