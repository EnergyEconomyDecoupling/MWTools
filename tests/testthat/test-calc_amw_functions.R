test_that("tidy_fao_live_animals works", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  live_animals <- test_data %>%
    tidy_fao_live_animals()

  expect_true(!is.null(live_animals))

  expect_equal(unique(live_animals$Country.name), c("China, mainland", "China"))

  expect_equal(colnames(live_animals), c("Country.name", "Species", "Year",
                                         "Value"))

  expect_equal(nrow(live_animals), 2184)

})


test_that("add_concordance_codes works", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  live_animals_w.codes <- test_data %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes()

  expect_true(!is.null(live_animals_w.codes))

  expect_equal(unique(live_animals_w.codes$Country.code), c("CHN", "-"))

  expect_equal(colnames(live_animals_w.codes), c("Country.code", "AMW.Region.code",
                                                 "Country.name", "Species", "Year",
                                                 "Value", "Country.incl.", "Country.code_PFU"))

  expect_equal(nrow(live_animals_w.codes), 2184)

})


test_that("trim_fao_data works", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  live_animals_trimmed <- test_data %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes() %>%
    trim_fao_data()

  expect_true(!is.null(live_animals_trimmed))

  expect_equal(unique(live_animals_trimmed$Country.code), "CHN")

  expect_equal(nrow(live_animals_trimmed), 1092)

})



test_that("get_working_species works", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  live_animals_w.species <- test_data %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes() %>%
    trim_fao_data() %>%
    get_working_species()

  expect_true(!is.null(live_animals_w.species))

  expect_equal(unique(live_animals_w.species$Country.code), "CHN")

  expect_equal(nrow(live_animals_w.species), 354)

  expect_equal(unique(live_animals_w.species$Species),
               c("Asses", "Buffaloes", "Camelids", "Cattle", "Horses", "Mules"))

})


test_that("calc_working_animals works", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  working_animals <- test_data %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes() %>%
    trim_fao_data() %>%
    get_working_species() %>%
    calc_working_animals()

  expect_true(!is.null(working_animals))

  expect_equal(nrow(working_animals), 354)

  expect_equal(colnames(working_animals), c("Country.code", "AMW.Region.code",
                                            "Country.name", "Year", "Species",
                                            "Live.animals", "Prop.Working.animals",
                                            "Working.animals.total"))
})


test_that("calc_sector_split works", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  working_animals_s.split <- test_data %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes() %>%
    trim_fao_data() %>%
    get_working_species() %>%
    calc_working_animals() %>%
    calc_sector_split()

  expect_true(!is.null(working_animals_s.split))

  expect_equal(nrow(working_animals_s.split), 354)

  expect_equal(colnames(working_animals_s.split), c("Country.code", "AMW.Region.code",
                                                    "Country.name", "Year", "Species",
                                                    "Live.animals", "Prop.Working.animals",
                                                    "Working.animals.total", "Prop.Working.animals.Ag",
                                                    "Prop.Working.animals.Tr", "Working.animals.Ag",
                                                    "Working.animals.Tr"))
})

test_that("tidy_numbers_data works", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  working_animals_tidy <- test_data %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes() %>%
    trim_fao_data() %>%
    get_working_species() %>%
    calc_working_animals() %>%
    calc_sector_split() %>%
    tidy_numbers_data()

  expect_true(!is.null(working_animals_tidy))

  expect_equal(nrow(working_animals_tidy), 1062)

  expect_equal(colnames(working_animals_tidy), c("AMW.Region.code", "Country.code",
                                                 "Year", "Species", "Sector",
                                                 "Live.animals", "Working.animals"))
})

test_that("calc_amw_numbers works", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  working_animals_tidy <- calc_amw_numbers(test_data)

  expect_true(!is.null(working_animals_tidy))

  expect_equal(nrow(working_animals_tidy), 1062)

  expect_equal(colnames(working_animals_tidy), c("AMW.Region.code", "Country.code",
                                                 "Year", "Species", "Sector",
                                                 "Live.animals", "Working.animals"))
})

test_that("calc_yearly_feed works", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  working_animals_w.feed <- test_data %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes() %>%
    trim_fao_data() %>%
    get_working_species() %>%
    calc_working_animals() %>%
    calc_sector_split() %>%
    calc_yearly_feed()

  expect_true(!is.null(working_animals_w.feed))

  expect_equal(nrow(working_animals_w.feed), 354)

  expect_equal(colnames(working_animals_w.feed), c("Country.code", "AMW.Region.code", "Country.name",
                                                   "Year", "Species", "Live.animals",
                                                   "Prop.Working.animals", "Working.animals.total",
                                                   "Prop.Working.animals.Ag", "Prop.Working.animals.Tr",
                                                   "Working.animals.Ag", "Working.animals.Tr",
                                                   "Total.yearly.feed [MJ/year per animal]"))
})

test_that("calc_final_energy works", {

  test_data_path <- amw_test_data_path()

  test_data <- read.csv(test_data_path)

  working_animals_w.finalenergy <- test_data %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes() %>%
    trim_fao_data() %>%
    get_working_species() %>%
    calc_working_animals() %>%
    calc_sector_split() %>%
    calc_yearly_feed() %>%
    calc_final_energy()

  expect_true(!is.null(working_animals_w.finalenergy))

  expect_equal(nrow(working_animals_w.finalenergy), 354)

  expect_equal(colnames(working_animals_w.finalenergy), c("Country.code", "AMW.Region.code", "Country.name",
                                                          "Year", "Species", "Live.animals",
                                                          "Prop.Working.animals", "Working.animals.total",
                                                          "Prop.Working.animals.Ag", "Prop.Working.animals.Tr",
                                                          "Working.animals.Ag", "Working.animals.Tr",
                                                          "Total.yearly.feed [MJ/year per animal]",
                                                          "Final.energy.total [MJ/year]", "Final.energy.Ag [MJ/year]",
                                                          "Final.energy.Tr [MJ/year]"))
})








