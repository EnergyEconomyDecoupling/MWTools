test_that("prep_mw_df() works as expected", {
  hmw_df <- hmw_test_data_path() %>%
    read.csv() %>%
    calc_hmw_pfu()
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu()
  res <- specify_product(hmw_df, amw_df) %>%
    MWTools::specify_primary_production() %>%
    specify_useful_products() %>%
    specify_fu_machines() %>%
    specify_last_stages() %>%
    MWTools::prep_psut()


})
