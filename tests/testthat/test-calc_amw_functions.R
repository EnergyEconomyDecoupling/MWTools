test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


amw_data_1 <- read_amw_data()

amw_data_2 <- tidy_trim_amw_data(amw_data_1)

amw_data_3 <- calc_working_animals(amw_data_2)

amw_data_4 <- calc_work_split(amw_data_3)

amw_data_5 <- calc_working_animals_split(amw_data_4)

amw_data_6 <- calc_yearly_feed(amw_data_5)

amw_data_7 <- calc_final_energy(amw_data_6)

amw_data_8 <- calc_primary_energy(amw_data_7)

amw_data_9 <- calc_useful_work(amw_data_8)




amw_data <- read_amw_data() %>%
  tidy_trim_amw_data() %>%
  calc_working_animals() %>%
  calc_work_split() %>%
  calc_working_animals_split() %>%
  calc_yearly_feed() %>%
  calc_final_energy() %>%
  calc_primary_energy() %>%
  calc_useful_work()
