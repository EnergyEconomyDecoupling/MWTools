test_that("specify_product() works as expected", {
  # Create a sample input data frame
  hmw_df <- hmw_test_data_path() %>%
    read.csv() %>%
    calc_hmw_pfu()
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu()

  specified_mw <- specify_product(hmw_df, amw_df)

  expect_equal(unique(specified_hmw[[MWTools::mw_constants$product]]),
               c(MWTools::mw_products$food,
                 MWTools::mw_products$biomass,
                 MWTools::mw_products$hu_mech,
                 RCLabels::paste_pref_suff(pref = MWTools::mw_products$biomass,
                                           suff = MWTools::mw_sectors$resources_sector,
                                           notation = RCLabels::from_notation)))
  expect_equal(unique(specified_amw[[MWTools::mw_constants$product]]),
               c(MWTools::mw_products$an_mech,
                 MWTools::mw_products$an_p,
                 MWTools::mw_products$feed,
                 MWTools::mw_products$biomass,
                 RCLabels::paste_pref_suff(pref = MWTools::mw_products$biomass,
                                           suff = MWTools::mw_sectors$resources_sector,
                                           notation = RCLabels::from_notation)))
})


test_that("specify_primary_production() works as expected", {
  hmw_df <- hmw_test_data_path() %>%
    read.csv() %>%
    calc_hmw_pfu()
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu()

  specified_mw <- specify_product(hmw_df, amw_df) %>%
    MWTools::specify_primary_production()

  specified_amw <- amw_df %>%
    MWTools::specify_primary_production()

  nrow_primary_hmw_biomass <- specified_hmw %>%
    dplyr::filter(.data[[MWTools::mw_constants$product]] == MWTools::mw_products$biomass) %>%
    nrow()
  nrow_primary_hmw_biomass_from_resources <- specified_hmw %>%
    dplyr::filter(.data[[MWTools::mw_constants$product]] == RCLabels::paste_pref_suff(pref = MWTools::mw_products$biomass,
                                                                                   suff = "Resources",
                                                                                   notation = RCLabels::from_notation)) %>%
    nrow()
  # We should make 1 row of Biomass [from Resources] for every row of biomass.
  expect_equal(nrow_primary_hmw_biomass, nrow_primary_hmw_biomass_from_resources)
})


test_that("specify_useful_products() works as expected", {
  hmw_df <- hmw_test_data_path() %>%
    read.csv() %>%
    calc_hmw_pfu()
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu()
  res <- specify_product(hmw_df, amw_df) %>%
    MWTools::specify_primary_production() %>%
    specify_useful_products()

})

