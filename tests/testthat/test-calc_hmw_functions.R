test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})



hmw_1 <- download_hmw_data()

hmw_2 <- trim_tidy_hmw_data(hmw_1)

hmw_3 <- calc_workers_sector(hmw_2)
