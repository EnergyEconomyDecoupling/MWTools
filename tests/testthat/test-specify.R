test_that("specify_energy_type_method() works as expected", {
  test_ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
  test_ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())

  hmw_df <- prepareRawILOData(ilo_working_hours_data = test_ilo_working_hours_data,
                              ilo_employment_data = test_ilo_employment_data) %>%
    calc_hmw_pfu()
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu()
  res <- specify_energy_type_method(hmw_df, amw_df)

  expect_true(MWTools::mw_cols$energy_type %in% colnames(res))
  expect_true(all(res[[MWTools::mw_cols$energy_type]] == MWTools::energy_types$e))

  expect_true(MWTools::mw_cols$method %in% colnames(res))
  expect_true(all(res[[MWTools::mw_cols$method]] == MWTools::methods$pcm))
})



test_that("specify_product() works as expected", {
  # Create a sample input data frame
  test_ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
  test_ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())

  hmw_df <- prepareRawILOData(ilo_working_hours_data = test_ilo_working_hours_data,
                              ilo_employment_data = test_ilo_employment_data) %>%
    calc_hmw_pfu()
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu()

  specified_mw <- specify_energy_type_method(hmw_df, amw_df) %>%
    specify_product()

  expect_equal(unique(specified_mw[[MWTools::mw_cols$product]]),
               c(MWTools::mw_products$food,
                 MWTools::mw_products$biomass,
                 MWTools::mw_products$hu_mech,
                 MWTools::mw_products$an_mech,
                 MWTools::mw_products$an_p,
                 MWTools::mw_products$feed))
})


test_that("specify_primary_production() works as expected", {
  test_ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
  test_ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())

  hmw_df <- prepareRawILOData(ilo_working_hours_data = test_ilo_working_hours_data,
                              ilo_employment_data = test_ilo_employment_data) %>%
    calc_hmw_pfu()
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu()

  specified_mw <- specify_energy_type_method(hmw_df, amw_df) %>%
    specify_product() %>%
    MWTools::specify_primary_production()

  nrow_primary_hmw_biomass <- specified_mw %>%
    dplyr::filter(.data[[MWTools::mw_cols$product]] == MWTools::mw_products$biomass) %>%
    nrow()
  nrow_primary_hmw_biomass_from_resources <- specified_mw %>%
    dplyr::filter(.data[[MWTools::mw_cols$product]] == RCLabels::paste_pref_suff(pref = MWTools::mw_products$biomass,
                                                                                   suff = "Resources",
                                                                                   notation = RCLabels::from_notation)) %>%
    nrow()
  # We should make 1 row of Biomass [from Resources] for every row of biomass.
  expect_equal(nrow_primary_hmw_biomass, nrow_primary_hmw_biomass_from_resources)
})


test_that("specify_useful_products() works as expected", {
  test_ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
  test_ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())

  hmw_df <- prepareRawILOData(ilo_working_hours_data = test_ilo_working_hours_data,
                              ilo_employment_data = test_ilo_employment_data) %>%
    calc_hmw_pfu()
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu()
  res <- specify_energy_type_method(hmw_df, amw_df) %>%
    specify_product() %>%
    MWTools::specify_primary_production() %>%
    specify_useful_products()

  # Check that all useful products are specified
  check <- res %>%
    dplyr::filter(.data[[MWTools::mw_constants$stage_col]] == MWTools::all_stages$useful) %>%
    dplyr::mutate(
      .suff = RCLabels::get_pref_suff(.data[[MWTools::mw_cols$product]],
                                      which = "suff",
                                      notation = RCLabels::from_notation),
      .OK = .data[[".suff"]] == .data[[MWTools::mw_constants$species]]
    )
  expect_true(all(check$.OK))
})


test_that("specify_fu_machines() works as expected", {
  test_ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
  test_ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())

  hmw_df <- prepareRawILOData(ilo_working_hours_data = test_ilo_working_hours_data,
                              ilo_employment_data = test_ilo_employment_data) %>%
    calc_hmw_pfu()
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu()
  res <- specify_energy_type_method(hmw_df, amw_df) %>%
    specify_product() %>%
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
      .OK = .data[[".suff"]] == RCLabels::get_pref_suff(.data[[MWTools::mw_cols$product]],
                                                        which = "pref",
                                                        notation = RCLabels::from_notation)
    )
  expect_true(all(check$.OK))
})


test_that("specify_last_stages() works as expected", {
  test_ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
  test_ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())

  hmw_df <- prepareRawILOData(ilo_working_hours_data = test_ilo_working_hours_data,
                              ilo_employment_data = test_ilo_employment_data) %>%
    calc_hmw_pfu()
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu()
  res <- specify_energy_type_method(hmw_df, amw_df) %>%
    specify_product() %>%
    MWTools::specify_primary_production() %>%
    specify_useful_products() %>%
    specify_fu_machines() %>%
    specify_last_stages()

  ls_final <- res %>%
    dplyr::filter(.data[[MWTools::mw_cols$last_stage]] == MWTools::last_stages$final) %>%
    dplyr::mutate(
      "{MWTools::mw_cols$last_stage}" := NULL
    )

  ls_useful_pf <- res %>%
    dplyr::filter(.data[[MWTools::mw_cols$last_stage]] == MWTools::last_stages$useful) %>%
    dplyr::filter(.data[[MWTools::mw_constants$stage_col]] != MWTools::last_stages$useful) %>%
    dplyr::mutate(
      "{MWTools::mw_cols$last_stage}" := NULL
    )

  expect_equal(ls_final, ls_useful_pf)
})


test_that("specify_ktoe() works as expected", {
  test_ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
  test_ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())

  EJ_df <- prepareRawILOData(ilo_working_hours_data = test_ilo_working_hours_data,
                             ilo_employment_data = test_ilo_employment_data) %>%
    calc_hmw_pfu() %>%
    dplyr::rename(
      E.dot_EJ := Edot
    ) %>%
    dplyr::mutate(
      "{MWTools::mw_cols$unit}" := NULL
    )
  ktoe_df <- EJ_df %>%
    specify_ktoe(energy_col = "E.dot_EJ") %>%
    dplyr::rename(
      E.dot_ktoe = E.dot_EJ
    ) %>%
    dplyr::mutate(
      "{MWTools::mw_cols$unit}" := NULL
    )
  expect_true(all(ktoe_df[[MWTools::mw_cols$unit]] == "ktoe"))
  check <- dplyr::full_join(EJ_df, ktoe_df, by = c("Country", "Year", "Species", "Stage", "Sector"))

  check <- EJ_df %>%
    dplyr::mutate(
      new_ktoe := .data[["E.dot_EJ"]] * MWTools::unit_constants$EJ_to_ktoe,
    ) %>%
    dplyr::full_join(ktoe_df, by = c("Country", "Year", "Species", "Stage", "Sector")) %>%
    dplyr::mutate(
      diff = new_ktoe - E.dot_ktoe
    )
  expect_true(all(check$diff == 0))
})


test_that("specify_TJ() works as expected", {
  test_ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
  test_ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())

  EJ_df <- prepareRawILOData(ilo_working_hours_data = test_ilo_working_hours_data,
                             ilo_employment_data = test_ilo_employment_data) %>%
    calc_hmw_pfu() %>%
    dplyr::rename(
      E.dot_EJ := Edot
    ) %>%
    dplyr::mutate(
      "{MWTools::mw_cols$unit}" := NULL
    )
  TJ_df <- EJ_df %>%
    specify_TJ(energy_col = "E.dot_EJ") %>%
    dplyr::rename(
      E.dot_TJ = E.dot_EJ
    ) %>%
    dplyr::mutate(
      "{MWTools::mw_cols$unit}" := NULL
    )
  expect_true(all(TJ_df[[MWTools::mw_cols$unit]] == "TJ"))
  check <- dplyr::full_join(EJ_df, TJ_df, by = c("Country", "Year", "Species", "Stage", "Sector"))

  check <- EJ_df %>%
    dplyr::mutate(
      new_TJ := .data[["E.dot_EJ"]] * MWTools::unit_constants$EJ_to_TJ,
    ) %>%
    dplyr::full_join(TJ_df, by = c("Country", "Year", "Species", "Stage", "Sector")) %>%
    dplyr::mutate(
      diff = new_TJ - E.dot_TJ
    )
  expect_true(all(check$diff == 0))
})
