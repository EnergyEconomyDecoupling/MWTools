test_that("specify_product() works as expected", {
  # Create a sample input data frame
  hmw_df <- hmw_test_data_path() %>%
    read.csv() %>%
    calc_hmw_pfu()
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu()

  specified_mw <- specify_product(hmw_df, amw_df)

  expect_equal(unique(specified_mw[[MWTools::mw_constants$product]]),
               c(MWTools::mw_products$food,
                 MWTools::mw_products$biomass,
                 MWTools::mw_products$hu_mech,
                 MWTools::mw_products$an_mech,
                 MWTools::mw_products$an_p,
                 MWTools::mw_products$feed))
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

  nrow_primary_hmw_biomass <- specified_mw %>%
    dplyr::filter(.data[[MWTools::mw_constants$product]] == MWTools::mw_products$biomass) %>%
    nrow()
  nrow_primary_hmw_biomass_from_resources <- specified_mw %>%
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

  # Check that all useful products are specified
  check <- res %>%
    dplyr::filter(.data[[MWTools::mw_constants$stage_col]] == MWTools::all_stages$useful) %>%
    dplyr::mutate(
      .suff = RCLabels::get_pref_suff(.data[[MWTools::mw_constants$product]],
                                      which = "suff",
                                      notation = RCLabels::from_notation),
      .OK = .data[[".suff"]] == .data[[MWTools::mw_constants$species]]
    )
  expect_true(all(check$.OK))
})


test_that("specify_fu_machines() works as expected", {
  hmw_df <- hmw_test_data_path() %>%
    read.csv() %>%
    calc_hmw_pfu()
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu()
  res <- specify_product(hmw_df, amw_df) %>%
    MWTools::specify_primary_production() %>%
    specify_useful_products() %>%
    specify_fu_machines()

  # Check that all final-to-useful machines are specified
  check <- res %>%
    dplyr::filter(.data[[MWTools::mw_constants$stage_col]] == MWTools::all_stages$useful) %>%
    dplyr::mutate(
      .suff = RCLabels::get_pref_suff(.data[[MWTools::mw_constants$species]],
                                      which = "suff",
                                      notation = RCLabels::arrow_notation),
      .OK = .data[[".suff"]] == RCLabels::get_pref_suff(.data[[MWTools::mw_constants$product]],
                                                        which = "pref",
                                                        notation = RCLabels::from_notation)
    )
  expect_true(all(check$.OK))
})


test_that("specify_last_stages() works as expected", {
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
    specify_last_stages()

})

