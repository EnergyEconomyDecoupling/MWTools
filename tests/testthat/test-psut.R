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
    dplyr::filter(.data[[MWTools::mw_constants$product]] != MWTools::mw_products$food) %>%
    nrow() %>%
    expect_equal(0)
  # Feed production makes only Feed.
  food_feed_prod_rows %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$matnames]] == MWTools::psut_cols$V &
                    .data[[MWTools::mat_meta_cols$rownames]] == MWTools::mw_sectors$feed_production) %>%
    dplyr::filter(.data[[MWTools::mw_constants$product]] != MWTools::mw_products$feed) %>%
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


test_that("prep_psut() works as expected", {
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
    MWTools::add_row_col_meta() %>%
    MWTools::prep_psut()

})
