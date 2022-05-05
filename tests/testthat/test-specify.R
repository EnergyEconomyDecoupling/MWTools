test_that("specify_product() works as expected", {
  # Create a sample input data frame
  hmw_df <- hmw_test_data_path() %>%
    read.csv() %>%
    calc_hmw_pfu()
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu()

  specified_hmw <- hmw_df %>%
    specify_product()
  specified_amw <- amw_df %>%
    specify_product()

  expect_equal(unique(specified_hmw[[IEATools::iea_cols$product]]),
               c(MWTools::mw_products$food,
                 MWTools::mw_products$biomass,
                 MWTools::mw_products$hu_mech))
  expect_equal(unique(specified_amw[[IEATools::iea_cols$product]]),
               c(MWTools::mw_products$an_mech,
                 MWTools::mw_products$an_p,
                 MWTools::mw_products$feed,
                 MWTools::mw_products$biomass))
})


