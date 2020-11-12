test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


amw_1 <- read_amw_data()

amw_2 <- tidy_trim_amw_data(amw_1)
