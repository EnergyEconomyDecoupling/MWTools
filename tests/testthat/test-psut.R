test_that("add_row_col_meta() works as expected", {
  hmw_df <- hmw_test_data_path() %>%
    read.csv() %>%
    calc_hmw_pfu()
  amw_df <- amw_test_data_path() %>%
    read.csv() %>%
    calc_amw_pfu()
  res <- specify_product(hmw_df, amw_df) %>%
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
  # Food production and Feed production make Feed or Food
  food_feed_prod_rows %>%
    dplyr::filter(.data[[MWTools::mat_meta_cols$matnames]] == MWTools::psut_cols$V) %>%
    dplyr::filter(!(.data[[MWTools::mat_meta_cols$colnames]] %in% c()))


})
