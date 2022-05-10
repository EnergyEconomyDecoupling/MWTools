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

  prep_mw_df(hmw_df, amw_df)

})
