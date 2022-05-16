test_that("add_row_col_meta() works as expected", {
  hmw_df <- hmw_test_data_path() %>%
    read.csv() %>%
    calc_hmw_pfu()
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu()
  res <- specify_energy_type_method(hmw_df, amw_df) %>%
    specify_product() %>%
    specify_ktoe() %>%
    MWTools::specify_primary_production() %>%
    specify_useful_products() %>%
    specify_fu_machines() %>%
    specify_last_stages() %>%
    MWTools::add_row_col_meta()

  # Verify that the matnames column is present a filled. No NA values allowed.
  expect_false(any(res[[MWTools::mat_meta_cols$matnames]] %>% is.null()))
  expect_false(any(res[[MWTools::mat_meta_cols$rownames]] %>% is.null()))
  expect_false(any(res[[MWTools::mat_meta_cols$colnames]] %>% is.null()))
  expect_false(any(res[[MWTools::mat_meta_cols$rowtypes]] %>% is.null()))
  expect_false(any(res[[MWTools::mat_meta_cols$coltypes]] %>% is.null()))
  # All R and V matrices are Industry x Product
  res %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$matnames]] %in% c(MWTools::psut_cols$R, MWTools::psut_cols$V)) %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$rowtypes]] != MWTools::row_col_types$industry) %>%
    nrow() %>%
    expect_equal(0)
  res %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$matnames]] %in% c(MWTools::psut_cols$R, MWTools::psut_cols$V)) %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$coltypes]] != MWTools::row_col_types$product) %>%
    nrow() %>%
    expect_equal(0)
  # All U and Y matrices are Product x Industry
  res %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$matnames]] %in% c(MWTools::psut_cols$U, MWTools::psut_cols$Y)) %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$rowtypes]] != MWTools::row_col_types$product) %>%
    nrow() %>%
    expect_equal(0)
  res %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$matnames]] %in% c(MWTools::psut_cols$U, MWTools::psut_cols$Y)) %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$coltypes]] != MWTools::row_col_types$industry) %>%
    nrow() %>%
    expect_equal(0)

  R_matrix_rows <- res %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$matnames]] == MWTools::psut_cols$R)
  # All row names in R matrix will be "Resources [of Biomass]"
  R_matrix_rows %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$rownames]] != "Resources [of Biomass]") %>%
    nrow() %>%
    expect_equal(0)
  # All column names in R matrix will be "Biomass [from Resources]"
  R_matrix_rows %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$colnames]] != "Biomass [from Resources]") %>%
    nrow() %>%
    expect_equal(0)

  # Check Farms.
  farm_rows <- res %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$rownames]] == MWTools::mw_sectors$farms |
                    .data[[MWTools::mat_meta_cols$colnames]] == MWTools::mw_sectors$farms)
  # Farms use exclusively Biomass [from Resources]
  farm_rows %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$colnames]] == MWTools::mw_sectors$farms) %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$rownames]] != "Biomass [from Resources]") %>%
    nrow() %>%
    expect_equal(0)
  # Farms make exclusively Biomass
  farm_rows %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$rownames]] == MWTools::mw_sectors$farms) %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$colnames]] != MWTools::mw_products$biomass) %>%
    nrow() %>%
    expect_equal(0)

  # Food production and Feed production use exclusively Biomass
  food_feed_prod_rows <- res %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$colnames]] %in% c("Food production", "Feed production") |
                    .data[[MWTools::mat_meta_cols$rownames]] %in% c("Food production", "Feed production"))
  food_feed_prod_rows %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$matnames]] == MWTools::psut_cols$U) %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$rownames]] != "Biomass") %>%
    nrow() %>%
    expect_equal(0)
  # Food production makes only Food.
  food_feed_prod_rows %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$matnames]] == MWTools::psut_cols$V &
                    .data[[MWTools::mat_meta_cols$rownames]] == MWTools::mw_sectors$food_production) %>%
    dplyr::filter(.data[[MWTools::mw_cols$product]] != MWTools::mw_products$food) %>%
    nrow() %>%
    expect_equal(0)
  # Feed production makes only Feed.
  food_feed_prod_rows %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$matnames]] == MWTools::psut_cols$V &
                    .data[[MWTools::mat_meta_cols$rownames]] == MWTools::mw_sectors$feed_production) %>%
    dplyr::filter(.data[[MWTools::mw_cols$product]] != MWTools::mw_products$feed) %>%
    nrow() %>%
    expect_equal(0)
  # The final-to-useful machines (which contain the string "->") use only food or feed
  fu_machines <- res %>%
    dplyr::filter(grepl(" -> ", .data[[MWTools::mat_meta_cols$rownames]]) |
                    grepl(" -> ", .data[[MWTools::mat_meta_cols$colnames]]))
  fu_machines %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$matnames]] == MWTools::psut_cols$U) %>%
    dplyr::filter(!(.data[[MWTools::mat_meta_cols$rownames]] %in% c(MWTools::mw_products$food, MWTools::mw_products$feed))) %>%
    nrow() %>%
    expect_equal(0)
  # The final-to-useful machines (which contain the string "->") make only HuMech, AnMech, or AnP
  fu_machines %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$matnames]] == MWTools::psut_cols$V) %>%
    dplyr::mutate(
      .check_col_pref = RCLabels::get_pref_suff(.data[[MWTools::mat_meta_cols$colnames]], which = "pref", notation = RCLabels::from_notation)
    ) %>%
    dplyr::filter(!(.data[[".check_col_pref"]] %in% c(MWTools::mw_products$hu_mech,
                                                                              MWTools::mw_products$an_mech,
                                                                              MWTools::mw_products$an_p))) %>%
    nrow() %>%
    expect_equal(0)

  # The final demand matrix (Y) should have only Industry, Services, Transport, or Agriculture as columns.
  res %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$matnames]] == MWTools::psut_cols$Y) %>%
    dplyr::filter(!(.data[[MWTools::mat_meta_cols$colnames]] %in% c(MWTools::mw_sectors$industry_broad.sector,
                                                                    MWTools::mw_sectors$services_broad.sector,
                                                                    MWTools::mw_sectors$transport_sector,
                                                                    MWTools::mw_sectors$agriculture_broad.sector))) %>%
    nrow() %>%
    expect_equal(0)
})


test_that("collapse_to_psut() works as expected", {
  hmw_df <- hmw_test_data_path() %>%
    read.csv() %>%
    calc_hmw_pfu() %>%
    # Keep only a few years for speed.
    dplyr::filter(Year %in% 2000:2002)
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu() %>%
    # Keep only a few years for speed.
    dplyr::filter(Year %in% 2000:2002)
  res <- specify_energy_type_method(hmw_df, amw_df) %>%
    specify_product() %>%
    specify_ktoe() %>%
    MWTools::specify_primary_production() %>%
    specify_useful_products() %>%
    specify_fu_machines() %>%
    specify_last_stages() %>%
    MWTools::add_row_col_meta() %>%
    MWTools::collapse_to_psut()

  # Ensure that every entry in the matrix columns is a matrix
  for (mat in c(MWTools::psut_cols$R,
                MWTools::psut_cols$U,
                MWTools::psut_cols$V,
                MWTools::psut_cols$Y)) {
    res[[mat]] %>%
      lapply(function(x) {
        is.matrix(x)
      }) %>%
      unlist() %>%
      all() %>%
      expect_true()
  }

  # Ensure that data are in the right place
  # by taking a few samples.
  expected1 <- hmw_df %>%
    dplyr::filter(.data[[MWTools::mw_cols$country]] == "GBR",
                  .data[[MWTools::mw_cols$year]] == 2000,
                  .data[[MWTools::mw_constants$species]] == "Human females",
                  .data[[MWTools::mw_constants$sector_col]] == "Agriculture",
                  .data[[MWTools::mw_constants$stage_col]] == MWTools::all_stages$useful) %>%
    magrittr::extract2(MWTools::mw_cols$e_dot) %>%
    magrittr::multiply_by(MWTools::unit_constants$EJ_to_ktoe)
  actual1 <- res %>%
    dplyr::filter(Country == "GBR", Year == 2000, Last.stage == "Useful") %>%
    magrittr::extract2(MWTools::psut_cols$Y) %>%
    magrittr::extract2(1) %>%
    matsbyname::select_rows_byname(retain_pattern = RCLabels::make_or_pattern("HuMech [from Human females]")) %>%
    matsbyname::select_cols_byname(retain_pattern = RCLabels::make_or_pattern("Agriculture")) %>%
    magrittr::extract2(1, 1)
  expect_equal(actual1, expected1)

  expected2 <- amw_df %>%
    dplyr::filter(.data[[MWTools::mw_cols$country]] == "CHN",
                  .data[[MWTools::mw_cols$year]] == 2002,
                  # .data[[MWTools::mw_constants$species]] == "Asses",
                  .data[[MWTools::mw_constants$sector_col]] == "Transport",
                  .data[[MWTools::mw_constants$stage_col]] == MWTools::all_stages$final) %>%
    # Several Species provide Transport.
    # Sum them before comparing to the entry in the Y matrix.
    magrittr::extract2(MWTools::mw_cols$e_dot) %>%
    sum() %>%
    magrittr::multiply_by(MWTools::unit_constants$EJ_to_ktoe)
  actual2 <- res %>%
    dplyr::filter(Country == "CHN", Year == 2002, Last.stage == MWTools::all_stages$final) %>%
    magrittr::extract2(MWTools::psut_cols$Y) %>%
    # Grab the first matrix in the column.
    magrittr::extract2(1) %>%
    magrittr::extract2(MWTools::mw_products$feed, MWTools::mw_sectors$transport_sector)
  expect_equal(actual2, expected2)
})


test_that("collapse_to_psut() works for inputs to Farms", {
  # This is an important test, because it checks whether aggregation
  # works at the level of the tidy data frame
  # (prior to forming matrices).
  hmw_df <- hmw_test_data_path() %>%
    read.csv() %>%
    calc_hmw_pfu() %>%
    # Keep only a few years for speed.
    dplyr::filter(Year %in% 2000:2002)
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu() %>%
    # Keep only a few years for speed.
    dplyr::filter(Year %in% 2000:2002)
  temp <- specify_energy_type_method(hmw_df, amw_df) %>%
    specify_product() %>%
    specify_ktoe() %>%
    MWTools::specify_primary_production() %>%
    specify_useful_products() %>%
    specify_fu_machines() %>%
    specify_last_stages() %>%
    MWTools::add_row_col_meta()
  res <- temp %>%
    MWTools::collapse_to_psut()

  # Grab all the inputs to Farms (which is Biomass [from Resources])
  farms_input <- temp %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$rownames]] == RCLabels::paste_pref_suff(pref = MWTools::mw_products$biomass,
                                                                                        suff = MWTools::mw_sectors$resources_sector,
                                                                                        notation = RCLabels::from_notation)) %>%
    dplyr::group_by(.data[[MWTools::mw_cols$country]], .data[[MWTools::mw_cols$year]], .data[[MWTools::mw_cols$last_stage]]) %>%
    dplyr::summarise("{MWTools::mw_cols$e_dot}" := sum(.data[[MWTools::mw_cols$e_dot]]), .groups = "drop")

  # Compare the data in farms_input to data in the U matrices.
  years <- res[[MWTools::mw_cols$year]] %>% unique()
  countries <- res[[MWTools::mw_cols$country]] %>% unique()
  last_stages <- res[[MWTools::mw_cols$last_stage]] %>% unique()
  for (coun in countries) {
    for (yr in years) {
      for (ls in last_stages) {
        # Get the value out of the matrix
        actual <- res %>%
          dplyr::filter(.data[[MWTools::mw_cols$year]] == yr,
                        .data[[MWTools::mw_cols$country]] == coun,
                        .data[[MWTools::mw_cols$last_stage]] == ls) %>%
          magrittr::extract2(MWTools::psut_cols$R) %>%
          # Get the first and only item in the R column
          magrittr::extract2(1) %>%
          # Get the entry in the first row and first column
          magrittr::extract2(1, 1)
        # Get the correct item from farms_input
        expected <- farms_input %>%
          dplyr::filter(.data[[MWTools::mw_cols$year]] == yr,
                        .data[[MWTools::mw_cols$country]] == coun,
                        .data[[MWTools::mw_cols$last_stage]] == ls) %>%
          magrittr::extract2(MWTools::mw_cols$e_dot) %>%
          magrittr::extract2(1)
        expect_equal(actual, expected)
      }
    }
  }
})


test_that("Energy is balanced in PSUT matrices", {
  hmw_df <- hmw_test_data_path() %>%
    read.csv() %>%
    calc_hmw_pfu() %>%
    # Keep only a few years for speed.
    dplyr::filter(Year %in% 2000:2002)
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu() %>%
    # Keep only a few years for speed.
    dplyr::filter(Year %in% 2000:2002)
  psut <- specify_energy_type_method(hmw_df, amw_df) %>%
    specify_product() %>%
    specify_ktoe() %>%
    MWTools::specify_primary_production() %>%
    specify_useful_products() %>%
    specify_fu_machines() %>%
    specify_last_stages() %>%
    MWTools::add_row_col_meta() %>%
    MWTools::collapse_to_psut()
  balanced <- psut %>%
    Recca::verify_SUT_energy_balance()
  expect_true(all(balanced[[".SUT_energy_balance"]] %>% unlist()))
})


test_that("append_S_units_col() works as expected", {
  hmw_df <- hmw_test_data_path() %>%
    read.csv() %>%
    calc_hmw_pfu() %>%
    # Keep only a few years for speed.
    dplyr::filter(Year %in% 2000:2002)
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu() %>%
    # Keep only a few years for speed.
    dplyr::filter(Year %in% 2000:2002)
  with_sunits <- specify_energy_type_method(hmw_df, amw_df) %>%
    specify_product() %>%
    specify_ktoe() %>%
    MWTools::specify_primary_production() %>%
    specify_useful_products() %>%
    specify_fu_machines() %>%
    specify_last_stages() %>%
    MWTools::add_row_col_meta() %>%
    MWTools::collapse_to_psut() %>%
    append_S_units_col()

  # Check a few examples
  expect_true(all(with_sunits[[MWTools::psut_cols$s_units]][[1]] %>% rownames() == c("Biomass", "Feed")))
  expect_true(with_sunits[[MWTools::psut_cols$s_units]][[1]] %>% colnames() == "ktoe")
  expect_true(with_sunits[[MWTools::psut_cols$s_units]][[1]] %>% matsbyname::rowtype() == MWTools::mw_cols$product)
  expect_true(with_sunits[[MWTools::psut_cols$s_units]][[1]] %>% matsbyname::coltype() == MWTools::row_col_types$unit)

  expect_true(all(with_sunits[[MWTools::psut_cols$s_units]][[10]] %>% rownames() == c("Biomass", "Food", "HuMech")))
  expect_true(with_sunits[[MWTools::psut_cols$s_units]][[1]] %>% colnames() == "ktoe")
  expect_true(with_sunits[[MWTools::psut_cols$s_units]][[1]] %>% matsbyname::rowtype() == MWTools::mw_cols$product)
  expect_true(with_sunits[[MWTools::psut_cols$s_units]][[1]] %>% matsbyname::coltype() == MWTools::row_col_types$unit)

  # Make sure the unit column was deleted
  expect_false(MWTools::mw_cols$unit %in% names(with_sunits))
})


test_that("append_U_feed_U_eiou_r_eiou_cols() works as expected", {
  hmw_df <- hmw_test_data_path() %>%
    read.csv() %>%
    calc_hmw_pfu() %>%
    # Keep only a few years for speed.
    dplyr::filter(Year %in% 2000:2002)
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu() %>%
    # Keep only a few years for speed.
    dplyr::filter(Year %in% 2000:2002)
  with_U_cols <- specify_energy_type_method(hmw_df, amw_df) %>%
    specify_product() %>%
    specify_ktoe() %>%
    MWTools::specify_primary_production() %>%
    specify_useful_products() %>%
    specify_fu_machines() %>%
    specify_last_stages() %>%
    MWTools::add_row_col_meta() %>%
    MWTools::collapse_to_psut() %>%
    append_S_units_col() %>%
    append_U_feed_U_eiou_r_eiou_cols()

  for (i in 1:nrow(with_U_cols)) {
    expect_true(!is.null(with_U_cols[[MWTools::psut_cols$U_feed]][[i]]))
    expect_true(!is.null(with_U_cols[[MWTools::psut_cols$U_eiou]][[i]]))
    expect_true(matsbyname::equal_byname(with_U_cols[[MWTools::psut_cols$U]][[i]], with_U_cols[[MWTools::psut_cols$U_feed]][[i]]))
    expect_true(matsbyname::equal_byname(matsbyname::hadamardproduct_byname(with_U_cols[[MWTools::psut_cols$U]][[i]], 0),
                                         with_U_cols[[MWTools::psut_cols$U_eiou]][[i]]))
    # The U_eiou and r_eiou columns should contain all 0 matrices.
    expect_true(matsbyname::iszero_byname(with_U_cols[[MWTools::psut_cols$U_eiou]][[i]]))
    expect_true(matsbyname::iszero_byname(with_U_cols[[MWTools::psut_cols$r_eiou]][[i]]))
  }
})


test_that("prep_psut() works as expected", {
  hmw_df <- hmw_test_data_path() %>%
    read.csv() %>%
    calc_hmw_pfu() %>%
    # Keep only a few years for speed.
    dplyr::filter(Year %in% 2000:2002)
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu() %>%
    # Keep only a few years for speed.
    dplyr::filter(Year %in% 2000:2002)
  psut <- prep_psut(hmw_df, amw_df)
  # Because values are tested in the test for collapse_to_psut(),
  # only test for non-NULL return value here.
  expect_true(!is.null(psut))
  expect_true(MWTools::psut_cols$R %in% names(psut))
  expect_true(MWTools::psut_cols$U %in% names(psut))
  expect_true(MWTools::psut_cols$V %in% names(psut))
  expect_true(MWTools::psut_cols$Y %in% names(psut))
  expect_true(MWTools::psut_cols$s_units %in% names(psut))
  expect_true(MWTools::psut_cols$U_feed %in% names(psut))
  expect_true(MWTools::psut_cols$U_eiou %in% names(psut))
})
