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
  # All rows for R matrix will be Industry x Product
  R_matrix_rows %>%
    dplyr::filter(!(.data[[MWTools::mat_meta_cols$rowtypes]] == MWTools::row_col_types$industry)) %>%
    nrow() %>%
    expect_equal(0)
  R_matrix_rows %>%
    dplyr::filter(!(.data[[MWTools::mat_meta_cols$coltypes]] == MWTools::row_col_types$product)) %>%
    nrow() %>%
    expect_equal(0)

  #



})
