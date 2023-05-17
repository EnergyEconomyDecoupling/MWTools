test_that("phi_vec_mw() works as expected", {
  # Check the default behavior
  pv <- phi_vec_mw()
  expected <- matrix(1, nrow = nrow(MWTools::phi_constants_mw), ncol = 1,
                     dimnames = list(MWTools::phi_constants_mw[[MWTools::mw_cols$product]] %>% unlist() %>% unname(),
                                     "phi")) %>%
    matsbyname::setrowtype(MWTools::mw_cols$product)
  expect_equal(pv, expected)

  # Check the case where we send fewer products.
  pv2 <- phi_vec_mw(mw_energy_carriers = c("Food", "Feed"))
  expected2 <- matrix(1, nrow = 2, ncol = 1, dimnames = list(c("Food", "Feed"), "phi")) %>%
    matsbyname::setrowtype(MWTools::mw_cols$product)
  expect_equal(pv2, expected2)
})


test_that("phi_vec_mw() works with Matrix objects", {
  # Check the default behavior
  pv <- phi_vec_mw(matrix_class = "Matrix")
  expected <- matsbyname::Matrix(1, nrow = nrow(MWTools::phi_constants_mw), ncol = 1,
                                 dimnames = list(MWTools::phi_constants_mw[[MWTools::mw_cols$product]] %>% unlist() %>% unname(),
                                                 "phi")) %>%
    matsbyname::setrowtype(MWTools::mw_cols$product)
  expect_equal(pv, expected)

  # Check the case where we send fewer products.
  pv2 <- phi_vec_mw(mw_energy_carriers = c("Food", "Feed"), matrix_class = "Matrix")
  expected2 <- matsbyname::Matrix(1, nrow = 2, ncol = 1, dimnames = list(c("Food", "Feed"), "phi")) %>%
    matsbyname::setrowtype(MWTools::mw_cols$product)
  expect_equal(pv2, expected2)
})
