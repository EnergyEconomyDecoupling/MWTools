#' Tidy live animals data from the FAO
#'
#' Create a tidy data frame using data supplied by the function
#' `MWTools::down_fao_live_animals`.
#'
#' @param .df A data frame containing the raw FAO live animals data,
#'            corresponding to the "QCL" FAO bulk download query.
#' @param country_name,species,year,unit,value See `MWTools::mw_constants`.
#' @param area_fao_col,item_fao_col,year_fao_col,unit_fao_col,value_fao_col
#'        See `MWTools::fao_cols`.
#'
#'
#' @export
#'
#' @examples
#' live_animals_data <- read.csv(file = MWTools::amw_test_data_path()) %>%
#'   tidy_fao_live_animals()
#'
tidy_fao_live_animals <- function(.df,
                                  country_name = MWTools::mw_constants$country_name,
                                  species = MWTools::mw_constants$species,
                                  year = MWTools::mw_constants$year,
                                  unit = MWTools::mw_constants$unit,
                                  value = MWTools::mw_constants$value,
                                  area_fao_col = MWTools::fao_cols$area_fao_col,
                                  item_fao_col = MWTools::fao_cols$item_fao_col,
                                  year_fao_col = MWTools::fao_cols$year_fao_col,
                                  unit_fao_col = MWTools::fao_cols$unit_fao_col,
                                  value_fao_col = MWTools::fao_cols$value_fao_col){

  # Read file into a tidy data frame
  live_animals <- .df %>%
    dplyr::filter(.data[[item_fao_col]] %in% MWTools::mw_species) %>%
    dplyr::select(dplyr::all_of(c(area_fao_col, item_fao_col, year_fao_col, unit_fao_col, value_fao_col))) %>%
    magrittr::set_colnames(c(country_name, species, year, unit, value))

  # Replaces unit designations with name "Number" or "1000 Number"
  live_animals <- live_animals %>%
    # Replaces "Head" with Number
    dplyr::mutate(
      "{unit}" := stringr::str_replace(.data[[unit]], "Head", "Number") #  Not replacing correctly.
    ) %>%
    # Replaces "No" with Number
    dplyr::mutate(
      "{unit}" := stringr::str_replace(.data[[unit]], "No", "Number")
    )

  # Converts "1000 Number" to "Number"
  live_animals_1000 <- live_animals %>%
    dplyr::filter(.data[[unit]] == "1000 Number") %>%
    dplyr::mutate(
      "{value}" := .data[[value]] * 1000
    )

  # Extracts data for Unit = "Number"
  live_animals_1 <- live_animals %>%
    dplyr::filter(.data[[unit]] == "Number")

  # Re-combines live animals data
  live_animals_final <- live_animals_1000 %>%
    rbind(live_animals_1) %>%
    dplyr::select(-dplyr::all_of(c(unit)))

  # Returns tidy data
  return(live_animals_final)
}


#' Add 3 letter ISO codes and MWTools-specific region codes
#'
#' Adds 3 letter ISO country codes and muscle work region codes for countries
#' present in the FAO's Live animals data, downloaded through the `FAOSTAT` package,
#' usually using the `MWTools::down_fao_live_animals` function.
#'
#'
#' @param .df The data frame containing tidied live animals data, Usually
#'            produced by calling the `tidy_fao_live_animals` function
#'            on the raw FAO data.
#' @param concordance_path The file path to concordance information mapping the
#'                         FAO country names supplied in FAOSTAT to 3-letter ISO
#'                         codes, and MWTools specific region codes. Set to
#'                         `MWTools::fao_concordance_path()` by default, the path
#'                         to the bundled concordance information in `MWTools`.
#' @param amw_region_code_col,hmw_region_code_col,country_incl_col,country_code_col,country_code_pfu_col
#'        See `MWTools::conc_cols`.
#' @param country_name See `MWTools::mw_constants`
#'
#'
#' @export
#'
#' @examples
#' live_animals_data <- read.csv(file = MWTools::amw_test_data_path()) %>%
#'   tidy_fao_live_animals() %>%
#'   add_concordance_codes()
#'
add_concordance_codes <- function(.df,
                                  concordance_path = MWTools::fao_concordance_path(),
                                  amw_region_code_col = MWTools::conc_cols$amw_region_code_col,
                                  hmw_region_code_col = MWTools::conc_cols$hmw_region_code_col,
                                  country_incl_col = MWTools::conc_cols$country_incl_col,
                                  country_code_col = MWTools::conc_cols$country_code_col,
                                  country_code_pfu_col = MWTools::conc_cols$country_code_pfu_col,
                                  country_name = MWTools::mw_constants$country_name){

  # Read bundled concordance data
  concordance_data <- readxl::read_xlsx(path = concordance_path,
                                        sheet = "Mapping") %>%
    dplyr::select(-dplyr::all_of(hmw_region_code_col)) %>%
    magrittr::set_colnames(c(country_name, country_incl_col, country_code_col, country_code_pfu_col, amw_region_code_col))

  .df %>%
    dplyr::left_join(concordance_data, by = country_name) %>%
    dplyr::relocate(dplyr::all_of(country_code_col), .before = dplyr::all_of(country_name)) %>%
    dplyr::relocate(dplyr::all_of(amw_region_code_col), .before = dplyr::all_of(country_name))
}


#' Trim countries in FAO live animals data
#'
#' This function filters out countries which have more than one instance
#' and aggregate regions from FAO live animals data. Data is usually downloaded
#' through the `FAOSTAT` package, using the `MWTools::down_fao_live_animals`
#' function, and after applying the `MWTools::add_concordance_codes` function.
#'
#' @param .df The data frame containing live animals data, with added country
#'            codes. Usually produced by calling the
#'            `tidy_fao_live_animals`, and
#'            `add_concordance_codes` functions in sequence on the raw FAO data.
#' @param country_incl_col,country_code_pfu_col See `MWTools::conc_cols`.
#' @param yes_const See `MWTools::amw_analysis_constants`.
#'
#'
#' @export
#'
#' @examples
#' live_animals_data <- read.csv(file = MWTools::amw_test_data_path()) %>%
#'   tidy_fao_live_animals() %>%
#'   add_concordance_codes() %>%
#'   trim_fao_data()
#'
trim_fao_data <- function(.df,
                          yes_const = MWTools::amw_analysis_constants$yes_const,
                          country_incl_col = MWTools::conc_cols$country_incl_col,
                          country_code_pfu_col = MWTools::conc_cols$country_code_pfu_col){

  .df %>%
    dplyr::filter(.data[[country_incl_col]] == yes_const) %>%
    dplyr::select(-dplyr::all_of(c(country_incl_col, country_code_pfu_col)))

}

#' Create a data frame containing the total number of animals of working species
#'
#' Using a tidy dataframe containing the number of live animals, this function
#' restricts the species of animals to: Asses, Camels, Cattle, Horses, Mules,
#' Buffaloes, and Camelids, other; then combines Camels and Camlelids, other
#' into a combined "Camelids" species group.
#'
#' @param .df A tidy data frame containing the number of live animals by
#'            country over time. Usually produced by calling the
#'            `tidy_fao_live_animals`,
#'            `add_concordance_codes`, and
#'            `trim_fao_data` functions in sequence on the raw FAO data.
#' @param species,country_name,value,year See `MWTools::mw_constants`.
#' @param live_animals_col See `MWtools::amw_analysis_constants`.
#' @param country_code_col,amw_region_code_col See `MWtools::conc_cols`.
#' @param asses,camels,cattle,horses,mules,buffaloes,camelids_other,camelids
#'        See `MWTools::mw_species`.
#'
#'
#' @export
#'
#' @examples
#' live_animals_data <- read.csv(file = MWTools::amw_test_data_path()) %>%
#'   tidy_fao_live_animals() %>%
#'   add_concordance_codes() %>%
#'   trim_fao_data() %>%
#'   get_working_species()
#'
get_working_species <- function(.df,
                                species = MWTools::mw_constants$species,
                                country_name = MWTools::mw_constants$country_name,
                                country_code_col = MWTools::conc_cols$country_code_col,
                                amw_region_code_col = MWTools::conc_cols$amw_region_code_col,
                                year = MWTools::mw_constants$year,
                                value = MWTools::mw_constants$value,
                                asses = MWTools::mw_species$asses,
                                camels = MWTools::mw_species$camels,
                                cattle = MWTools::mw_species$cattle,
                                horses = MWTools::mw_species$horses,
                                mules = MWTools::mw_species$mules,
                                buffaloes = MWTools::mw_species$buffaloes,
                                camelids_other = MWTools::mw_species$camelids_other,
                                camelids = MWTools::mw_species$camelids,
                                live_animals_col = MWTools::amw_analysis_constants$live_animals_col){

  # This function assumes a specific set of animals are present.
  # If the bundled mapping data restricts countries so that one of these animals,
  # e.g. asses, does not exist the function will fail.

  # Filter data to only include working animal species
  working_species <- .df %>%
    dplyr::filter(.data[[species]] %in% c(asses,
                                          camels,
                                          cattle,
                                          horses,
                                          mules,
                                          buffaloes,
                                          camelids_other)) %>%
    tidyr::pivot_wider(names_from = dplyr::all_of(species),
                       values_from = dplyr::all_of(value))
  working_species <- replace(working_species, is.na(working_species), 0)

  if(camelids_other %in% colnames(working_species)){
    working_species_w.camelids <- working_species %>%
      dplyr::mutate(
        "{camelids}" := .data[[camels]] + .data[[camelids_other]],
        .keep = "unused"
      )
  } else {
    working_species_w.camelids <- working_species %>%
      dplyr::mutate(
        "{camelids}" := .data[[camels]],
        .keep = "unused"
      )
  }

  working_species_final <-  working_species_w.camelids %>%
    tidyr::pivot_longer(cols = dplyr::all_of(c(asses, buffaloes, camelids, cattle, horses, mules)),
                        names_to = species,
                        values_to = value) %>%
    magrittr::set_colnames(c(country_code_col, amw_region_code_col, country_name, year, species, live_animals_col))

  # Returns a tidy data frame containing the number of working animals
  return(working_species_final)

}



#' Calculate the number of working animals
#'
#' Calculate the number of working animals using data for the number of live
#' animals from the FAO, and user-supplied data for the proportion of working
#' animals by region and year.
#'
#' @param .df A data frame containing the number of live animals for working
#'            species. Usually produced by calling the
#'            `tidy_fao_live_animals`,
#'            `add_concordance_codes`,
#'            `trim_fao_data`, and
#'            `get_working_species` functions in sequence on the raw FAO data.
#' @param amw_analysis_data_path The path to the animal muscle work analysis data,
#'                               containing data for the proportion of working animals
#'                               by species, country, and over time. Set to the function
#'                               `MWTools::amw_analysis_data_path()`, by default,
#'                               which returns the path the analysis data bundled
#'                               with the `MWTools` package.
#' @param year,species,exemplar_method_col See `MWTools::mw_constants`.
#' @param prop_working_animals_col,wa_perc_sheet,working_animals_total_col,live_animals_col,amw_region_col See `MWTools::amw_analysis_constants`.
#' @param amw_region_code_col See `MWTools::conc_cols`.
#'
#'
#' @export
#'
#' @examples
#' working_animals_data <- read.csv(file = MWTools::amw_test_data_path()) %>%
#'   tidy_fao_live_animals() %>%
#'   add_concordance_codes() %>%
#'   trim_fao_data() %>%
#'   get_working_species() %>%
#'   calc_working_animals()
calc_working_animals <- function(.df,
                                 amw_analysis_data_path = MWTools::amw_analysis_data_path(),
                                 year = MWTools::mw_constants$year,
                                 species = MWTools::mw_constants$species,
                                 exemplar_method_col = MWTools::mw_constants$exemplar_method_col,
                                 prop_working_animals_col = MWTools::amw_analysis_constants$prop_working_animals_col,
                                 wa_perc_sheet = MWTools::amw_analysis_constants$wa_perc_sheet,
                                 working_animals_total_col = MWTools::amw_analysis_constants$working_animals_total_col,
                                 live_animals_col = MWTools::amw_analysis_constants$live_animals_col,
                                 amw_region_code_col = MWTools::conc_cols$amw_region_code_col,
                                 amw_region_col = MWTools::amw_analysis_constants$amw_region_col){

  # Reads the amw analysis excel file to determine the percentage of live animals
  # that are working animals.
  working_animals_prop <- readxl::read_excel(amw_analysis_data_path,
                                             sheet = wa_perc_sheet) %>%
    tibble::tibble() %>%
    dplyr::select(-dplyr::all_of(c(exemplar_method_col, amw_region_col))) %>%
    tidyr::pivot_longer(cols = -dplyr::all_of(c(species, amw_region_code_col)), # Use IEATools::year_cols()?
                        names_to = year,
                        values_to = prop_working_animals_col) %>%
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]])
    )

  .df %>%
    dplyr::left_join(working_animals_prop, by = c(species, amw_region_code_col, year)) %>%
    dplyr::mutate(
      "{working_animals_total_col}" := .data[[live_animals_col]] * .data[[prop_working_animals_col]]
      )
}



#' Calculate the number of working animals in agriculture and transport
#'
#' Calculate the split of working animals performing muscle work in agriculture
#' and transport using user-supplied data for the proportion of animals working
#' in agriculture by region and year.
#'
#' @param .df A data frame containing the number of working animals.
#'            Usually produced by calling the
#'            `tidy_fao_live_animals`,
#'            `add_concordance_codes`,
#'            `trim_fao_data`,
#'            `get_working_species`, and
#'            `calc_working_animals` functions in sequence on the raw FAO data.
#' @param amw_analysis_data_path The path to the animal muscle work analysis data,
#'                               containing data for the proportion of working animals
#'                               in agriculture and transport, by species, country, and
#'                               over time. Set to the function `MWTools::amw_analysis_data_path()`,
#'                               by default, which returns the path the analysis data
#'                               bundled with the `MWTools` package.
#' @param year,species,method_source See `MWTools::mw_constants`.
#' @param metric,amw_region_col,wa_enduse_sheet,working_animals_total_col,working_animals_ag_col,working_animals_tr_col,prop_working_animals_ag_col,prop_working_animals_tr_col
#'        See `MWTools::amw_analysis_constants`.
#' @param amw_region_code_col See `MWTools::conc_cols`.
#'
#'
#' @export
#'
#' @examples
#' working_animals_data <- read.csv(file = MWTools::amw_test_data_path()) %>%
#'   tidy_fao_live_animals() %>%
#'   add_concordance_codes() %>%
#'   trim_fao_data() %>%
#'   get_working_species() %>%
#'   calc_working_animals() %>%
#'   calc_sector_split()
#'
calc_sector_split <- function(.df,
                              amw_analysis_data_path = MWTools::amw_analysis_data_path(),
                              year = MWTools::mw_constants$year,
                              species = MWTools::mw_constants$species,
                              method_source = MWTools::mw_constants$method_source,
                              metric = MWTools::amw_analysis_constants$metric,
                              amw_region_col = MWTools::amw_analysis_constants$amw_region_col,
                              amw_region_code_col = MWTools::conc_cols$amw_region_code_col,
                              wa_enduse_sheet = MWTools::amw_analysis_constants$wa_enduse_sheet,
                              working_animals_total_col = MWTools::amw_analysis_constants$working_animals_total_col,
                              working_animals_ag_col = MWTools::amw_analysis_constants$working_animals_ag_col,
                              working_animals_tr_col = MWTools::amw_analysis_constants$working_animals_tr_col,
                              prop_working_animals_ag_col = MWTools::amw_analysis_constants$prop_working_animals_ag_col,
                              prop_working_animals_tr_col = MWTools::amw_analysis_constants$prop_working_animals_tr_col) {

  end_use <- readxl::read_excel(amw_analysis_data_path,
                                sheet = wa_enduse_sheet) %>%
    dplyr::select(-dplyr::all_of(c(method_source, metric, amw_region_col))) %>%
    tidyr::pivot_longer(cols = -dplyr::all_of(c(species, amw_region_code_col)), # Use IEATools::year_cols()?
                        names_to = year,
                        values_to = prop_working_animals_ag_col) %>%
    dplyr::mutate(
      "{year}" := as.numeric(.data[[year]]),
      "{prop_working_animals_tr_col}" := 1 - .data[[prop_working_animals_ag_col]]
      )

  .df %>%
    dplyr::left_join(end_use, by = dplyr::all_of(c(species, amw_region_code_col, year))) %>%
    dplyr::mutate(
      "{working_animals_ag_col}" := .data[[working_animals_total_col]] * .data[[prop_working_animals_ag_col]],
      "{working_animals_tr_col}" := .data[[working_animals_total_col]] * .data[[prop_working_animals_tr_col]]
      )

}

#' Calculate the yearly feed requirements of working animals by species
#'
#' Calculate the yearly feed requirements of working animals by species and region.
#' This function uses user-supplied data supplied via `amw_analysis_data_path` for daily
#' feed requirements of working animals on working and non-working days by species
#' and region, and user-supplied data for the number of working days by species
#' and region to estimate the total yearly feed requirements.
#'
#' @param .df A data frame containing tidied data for the number of live and
#'            working animals. Usually produced by calling the
#'            `tidy_fao_live_animals`,
#'            `add_concordance_codes`,
#'            `trim_fao_data`,
#'            `get_working_species`,
#'            `calc_working_animals`, and
#'            `calc_sector_split` functions in sequence on the raw FAO data.
#' @param amw_analysis_data_path The path to the animal muscle work analysis data,
#'                               containing feed requirements of working animals
#'                               in agriculture and transport, by species, country, and
#'                               over time. Set to the function `MWTools::amw_analysis_data_path()`,
#'                               by default, which returns the path the analysis data
#'                               bundled with the `MWTools` package.
#' @param species,method_source See `MWTools::mw_constants`.
#' @param wa_feed_sheet,wa_days_hours_sheet,working_days_col,nonworking_days_col,working_hours_col,working_day_feed_col,nonworking_day_feed_col,working_yearly_feed_col,nonworking_yearly_feed_col,total_yearly_feed_col
#'        See `MWTools::amw_analysis_constants`.
#' @param amw_region_code_col See `MWTools::conc_cols`.
#'
#'
#' @export
#'
#' @examples
#' working_animals_data <- read.csv(file = MWTools::amw_test_data_path()) %>%
#'   tidy_fao_live_animals() %>%
#'   add_concordance_codes() %>%
#'   trim_fao_data() %>%
#'   get_working_species() %>%
#'   calc_working_animals() %>%
#'   calc_sector_split() %>%
#'   calc_yearly_feed()
calc_yearly_feed <- function(.df,
                             amw_analysis_data_path = MWTools::amw_analysis_data_path(),
                             species = MWTools::mw_constants$species,
                             method_source = MWTools::mw_constants$method_source,
                             wa_feed_sheet= MWTools::amw_analysis_constants$wa_feed_sheet,
                             wa_days_hours_sheet = MWTools::amw_analysis_constants$wa_days_hours_sheet,
                             working_days_col = MWTools::amw_analysis_constants$working_days_col,
                             nonworking_days_col = MWTools::amw_analysis_constants$nonworking_days_col,
                             working_hours_col = MWTools::amw_analysis_constants$working_hours_col,
                             working_day_feed_col = MWTools::amw_analysis_constants$working_day_feed_col,
                             nonworking_day_feed_col = MWTools::amw_analysis_constants$nonworking_day_feed_col,
                             working_yearly_feed_col = MWTools::amw_analysis_constants$working_yearly_feed_col,
                             nonworking_yearly_feed_col = MWTools::amw_analysis_constants$nonworking_yearly_feed_col,
                             total_yearly_feed_col = MWTools::amw_analysis_constants$total_yearly_feed_col,
                             amw_region_code_col = MWTools::conc_cols$amw_region_code_col) {

  feed <- readxl::read_excel(amw_analysis_data_path,
                             sheet = wa_feed_sheet) %>%
    dplyr::select(-dplyr::all_of(method_source))

  working_days <- readxl::read_excel(amw_analysis_data_path,
                                     sheet = wa_days_hours_sheet) %>%
    dplyr::select(-dplyr::all_of(c(method_source, working_hours_col))) %>%
    dplyr::mutate(
      "{nonworking_days_col}" := 365 - .data[[working_days_col]]
      )

  yearly_feed <- feed %>%
    dplyr::left_join(working_days, by = c(species, amw_region_code_col)) %>%
    dplyr::mutate(
      "{working_yearly_feed_col}" := .data[[working_day_feed_col]] * .data[[working_days_col]],
      "{nonworking_yearly_feed_col}" := .data[[nonworking_day_feed_col]] * .data[[nonworking_days_col]],
      "{total_yearly_feed_col}" := .data[[working_yearly_feed_col]] + .data[[nonworking_yearly_feed_col]]
      ) %>%
    dplyr::select(dplyr::all_of(c(species, amw_region_code_col, total_yearly_feed_col)))

  .df %>%
    dplyr::left_join(yearly_feed, by = c(species, amw_region_code_col))
}


#' Calculate the final energy metabolised by working animals
#'
#' Calculate the final energy metabolised by working animals to perform muscle work.
#' This function multiplies the number of working animals by sector, species,
#' and country by their associated feed requirements, then corrects for feed
#' wasted, and the gross energy to digestible energy ratio.
#'
#' @param .df A data frame containing the yearly feed requirements of working
#'            animals. Usually produced by calling the
#'            `tidy_fao_live_animals`,
#'            `add_concordance_codes`,
#'            `trim_fao_data`,
#'            `get_working_species`,
#'            `calc_working_animals`,
#'            `calc_sector_split`, and
#'            `calc_yearly_feed` functions in sequence on the raw FAO data.
#' @param trough_waste The proportion of feed energy wasted when feeding working animals.
#' @param ge_de_ratio The ratio between the gross energy content of the feed and
#'                    digestible energy that is recovered from the feed.
#' @param working_animals_total_col,working_animals_ag_col,working_animals_tr_col,total_yearly_feed_col,final_energy_ag,final_energy_tr,final_energy_total
#'        See `MWTools::amw_analysis_constants`.
#'
#'
#' @export
#'
#' @examples
#' working_animals_data <- read.csv(file = MWTools::amw_test_data_path()) %>%
#'   tidy_fao_live_animals() %>%
#'   add_concordance_codes() %>%
#'   trim_fao_data() %>%
#'   get_working_species() %>%
#'   calc_working_animals() %>%
#'   calc_sector_split() %>%
#'   calc_yearly_feed() %>%
#'   calc_final_energy()
calc_final_energy <- function(.df,
                              trough_waste = 0.1,
                              ge_de_ratio = 1.163636363636364, # Gross energy to digestible energy ratio
                              working_animals_total_col = MWTools::amw_analysis_constants$working_animals_total_col,
                              working_animals_ag_col = MWTools::amw_analysis_constants$working_animals_ag_col,
                              working_animals_tr_col = MWTools::amw_analysis_constants$working_animals_tr_col,
                              total_yearly_feed_col = MWTools::amw_analysis_constants$total_yearly_feed_col,
                              final_energy_total = MWTools::amw_analysis_constants$final_energy_total,
                              final_energy_ag = MWTools::amw_analysis_constants$final_energy_ag,
                              final_energy_tr = MWTools::amw_analysis_constants$final_energy_tr) {

  .df %>%
    dplyr::mutate(
      "{final_energy_total}" := (.data[[working_animals_total_col]] * .data[[total_yearly_feed_col]]) * ge_de_ratio * (1/(1 - trough_waste)),
      "{final_energy_ag}" := (.data[[working_animals_ag_col]] * .data[[total_yearly_feed_col]]) * ge_de_ratio * (1/(1 - trough_waste)),
      "{final_energy_tr}" := (.data[[working_animals_tr_col]] * .data[[total_yearly_feed_col]]) * ge_de_ratio * (1/(1 - trough_waste))
    )
}



#' Calculate the primary energy embodied in the feed supplied to working animals
#'
#'
#'
#' @param .df A data frame containing the final energy consumption of working
#'            animals. Usually produced by calling the
#'            `tidy_fao_live_animals`,
#'            `add_concordance_codes`,
#'            `trim_fao_data`,
#'            `get_working_species`,
#'            `calc_working_animals`,
#'            `calc_sector_split`,
#'            `calc_yearly_feed`, and
#'            `calc_final_energy` functions in sequence on the raw FAO data.
#' @param harvest_waste The proportion of energy embodied in biomass wasted when
#'                      animal feed is harvested.
#' @param final_energy_total,final_energy_ag,final_energy_tr,primary_energy_total,primary_energy_ag,primary_energy_tr
#'        See `MWTools::amw_analysis_constants`.
#'
#'
#' @export
#'
#' @examples
#' working_animals_data <- read.csv(file = MWTools::amw_test_data_path()) %>%
#'   tidy_fao_live_animals() %>%
#'   add_concordance_codes() %>%
#'   trim_fao_data() %>%
#'   get_working_species() %>%
#'   calc_working_animals() %>%
#'   calc_sector_split() %>%
#'   calc_yearly_feed() %>%
#'   calc_final_energy() %>%
#'   calc_primary_energy()
#'
calc_primary_energy <- function(.df,
                                harvest_waste = 0.45,
                                final_energy_total = MWTools::amw_analysis_constants$final_energy_total,
                                final_energy_ag = MWTools::amw_analysis_constants$final_energy_ag,
                                final_energy_tr = MWTools::amw_analysis_constants$final_energy_tr,
                                primary_energy_total = MWTools::amw_analysis_constants$primary_energy_total,
                                primary_energy_ag = MWTools::amw_analysis_constants$primary_energy_ag,
                                primary_energy_tr = MWTools::amw_analysis_constants$primary_energy_tr) {

  .df %>%
    dplyr::mutate(
      "{primary_energy_total}" := .data[[final_energy_total]] / harvest_waste,
      "{primary_energy_ag}" := .data[[final_energy_ag]] / harvest_waste,
      "{primary_energy_tr}" := .data[[final_energy_tr]] / harvest_waste
      )
}



#' Calculate the useful energy produced by working animals performing muscle work
#'
#' @param .df A data frame containing the primary and final energy consumed by
#'            working animals. Usually produced by calling the
#'            `tidy_fao_live_animals`,
#'            `add_concordance_codes`,
#'            `trim_fao_data`,
#'            `get_working_species`,
#'            `calc_working_animals`,
#'            `calc_sector_split`,
#'            `calc_yearly_feed`,
#'            `calc_final_energy`, and
#'            `calc_primary_energy` functions in sequence on the raw FAO data.
#' @param amw_analysis_data_path The path to the animal muscle work analysis data,
#'                               containing data for the number of working hours and
#'                               power outputs of working animals. Set to the function
#'                               `MWTools::amw_analysis_data_path()` by default,
#'                               which returns the path the analysis data bundled with
#'                               the `MWTools` package.
#' @param species,method_source See `MWTools::mw_constants`.
#' @param wa_power_sheet,wa_days_hours_sheet See `MWTools::amw_analysis_constants`.
#' @param working_hours_col,working_seconds_col See `MWTools::amw_analysis_constants`.
#' @param amw_region_code_col,power_per_animal See `MWTools::amw_analysis_constants`.
#' @param useful_energy_total,useful_energy_ag,useful_energy_tr See `MWTools::amw_analysis_constants`.
#' @param working_animals_total_col,working_animals_ag_col,working_animals_tr_col,working_days_col See `MWTools::amw_analysis_constants`.
#'
#' @export
#'
#' @examples
#' MWTools::amw_test_data_path() %>%
#'   read.csv() %>%
#'   tidy_fao_live_animals() %>%
#'   add_concordance_codes() %>%
#'   trim_fao_data() %>%
#'   get_working_species() %>%
#'   calc_working_animals() %>%
#'   calc_sector_split() %>%
#'   calc_yearly_feed() %>%
#'   calc_final_energy() %>%
#'   calc_primary_energy() %>%
#'   calc_useful_energy()
calc_useful_energy <- function(.df,
                               amw_analysis_data_path = MWTools::amw_analysis_data_path(),
                               species = MWTools::mw_constants$species,
                               method_source = MWTools::mw_constants$method_source,
                               wa_power_sheet= MWTools::amw_analysis_constants$wa_power_sheet,
                               wa_days_hours_sheet = MWTools::amw_analysis_constants$wa_days_hours_sheet,
                               working_days_col = MWTools::amw_analysis_constants$working_days_col,
                               working_hours_col = MWTools::amw_analysis_constants$working_hours_col,
                               working_seconds_col = MWTools::amw_analysis_constants$working_seconds_col,
                               amw_region_code_col = MWTools::conc_cols$amw_region_code_col,
                               power_per_animal = MWTools::amw_analysis_constants$power_per_animal,
                               useful_energy_total = MWTools::amw_analysis_constants$useful_energy_total,
                               useful_energy_ag = MWTools::amw_analysis_constants$useful_energy_ag,
                               useful_energy_tr = MWTools::amw_analysis_constants$useful_energy_tr,
                               working_animals_total_col = MWTools::amw_analysis_constants$working_animals_total_col,
                               working_animals_ag_col = MWTools::amw_analysis_constants$working_animals_ag_col,
                               working_animals_tr_col = MWTools::amw_analysis_constants$working_animals_tr_col) {

  power <- readxl::read_excel(amw_analysis_data_path,
                              sheet = wa_power_sheet) %>%
    dplyr::select(-dplyr::all_of(method_source)) %>%
    magrittr::set_colnames(c(species, amw_region_code_col, power_per_animal))

  working_time <- readxl::read_excel(amw_analysis_data_path,
                                     sheet = wa_days_hours_sheet) %>%
    dplyr::select(-dplyr::all_of(c(method_source, working_days_col))) %>%
    magrittr::set_colnames(c(species, amw_region_code_col, working_hours_col)) %>%
    dplyr::mutate(
      "{working_seconds_col}" := .data[[working_hours_col]] * 3600, .keep = "unused"
      )

  .df %>%
    dplyr::left_join(power, by = c(species, amw_region_code_col)) %>%
    dplyr::left_join(working_time, by = c(species, amw_region_code_col)) %>%
    dplyr::mutate(
      "{useful_energy_total}" := .data[[working_animals_total_col]] * .data[[power_per_animal]] * .data[[working_seconds_col]] / 1000000,
      "{useful_energy_ag}" := .data[[working_animals_ag_col]] * .data[[power_per_animal]] * .data[[working_seconds_col]] / 1000000,
      "{useful_energy_tr}" := .data[[working_animals_tr_col]] * .data[[power_per_animal]] * .data[[working_seconds_col]] / 1000000
      )

}

#' Tidy a data frame containg primary, final, and useful energy data for working animals
#'
#' @param .df A data frame containing the primary, final, and useful energy
#'            associated with each working animal species.
#'            Usually produced by calling the
#'            `tidy_fao_live_animals()`,
#'            `add_concordance_codes()`,
#'            `trim_fao_data()`,
#'            `get_working_species()`,
#'            `calc_working_animals()`,
#'            `calc_sector_split()`,
#'            `calc_yearly_feed()`,
#'            `calc_final_energy()`,
#'            `calc_primary_energy()`, and
#'            `calc_useful_energy()` functions in sequence on the raw FAO data.
#' @param country_code_col,country_col,amw_region_code_col See `MWTools::conc_cols`.
#' @param year,species,sector_col,stage_col,energy_col,units_col See `MWTools::mw_constants`.
#' @param useful_energy_ag,useful_energy_tr,final_energy_ag,final_energy_tr,primary_energy_ag,primary_energy_tr,working_animals_ag_col,working_animals_tr_col See `MWTools::amw_analysis_constants`.
#'
#' @export
#'
#' @examples
#' working_animals_data <- read.csv(file = MWTools::amw_test_data_path()) %>%
#'   tidy_fao_live_animals() %>%
#'   add_concordance_codes() %>%
#'   trim_fao_data() %>%
#'   get_working_species() %>%
#'   calc_working_animals() %>%
#'   calc_sector_split() %>%
#'   calc_yearly_feed() %>%
#'   calc_final_energy() %>%
#'   calc_primary_energy() %>%
#'   calc_useful_energy() %>%
#'   tidy_pfu_data()
tidy_pfu_data <- function(.df,
                          country_code_col = MWTools::conc_cols$country_code_col,
                          country_col = MWTools::conc_cols$country_col,
                          amw_region_code_col = MWTools::conc_cols$amw_region_code_col,
                          year = MWTools::mw_constants$year,
                          species = MWTools::mw_constants$species,
                          sector_col = MWTools::mw_constants$sector_col,
                          stage_col = MWTools::mw_constants$stage_col,
                          energy_col = MWTools::mw_constants$energy_col,
                          units_col = MWTools::mw_constants$units_col,
                          useful_energy_ag = MWTools::amw_analysis_constants$useful_energy_ag,
                          useful_energy_tr = MWTools::amw_analysis_constants$useful_energy_tr,
                          final_energy_ag = MWTools::amw_analysis_constants$final_energy_ag,
                          final_energy_tr = MWTools::amw_analysis_constants$final_energy_tr,
                          primary_energy_ag = MWTools::amw_analysis_constants$primary_energy_ag,
                          primary_energy_tr = MWTools::amw_analysis_constants$primary_energy_tr,
                          working_animals_ag_col = MWTools::amw_analysis_constants$working_animals_ag_col,
                          working_animals_tr_col = MWTools::amw_analysis_constants$working_animals_tr_col) {

  .df %>%
    dplyr::select(dplyr::all_of(c(country_code_col, year, species,
                                  useful_energy_ag, useful_energy_tr,
                                  final_energy_ag, final_energy_tr,
                                  primary_energy_ag, primary_energy_tr))) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(c(useful_energy_ag, useful_energy_tr,
                                               final_energy_ag, final_energy_tr,
                                               primary_energy_ag, primary_energy_tr)),
                        names_to = c(stage_col, sector_col),
                        names_sep = ".energy.",
                        values_to = energy_col) %>%
    dplyr::mutate(
      "{sector_col}" := stringr::str_replace_all(.data[[sector_col]], stringr::fixed(" [MJ/year]"), ""),
      "{sector_col}" := dplyr::case_when(
        .data[[sector_col]] == "Ag" ~ "Agriculture",
        .data[[sector_col]] == "Tr" ~ "Transport",
        TRUE ~ "Unknown sector column value"
        )
      ) %>%
    dplyr::mutate("{energy_col}" := .data[[energy_col]] * 0.000000000001) %>%
    dplyr::mutate("{units_col}" := "EJ", .before = dplyr::all_of(energy_col)) %>%
    magrittr::set_colnames(c(country_col, year, species,
                             stage_col, sector_col, units_col, energy_col))
}

#' Calculate primary, final, and useful working animal energy
#'
#' Calculate the total number of working animals and primary, final, and useful
#' working animal energy by country, for six species: Asses, Buffaloes, Camelids,
#' Cattle, Horses, and Mules, and three sector categories: Total, Agriculture,
#' and Transport. This function acts as a helper function calling a number of functions
#' in sequence to convert FAO data for live animals, usually downloaded with the function
#' `down_fao_live_animals`, into a tidy data frame.
#'
#' @param .df A data frame containing the raw FAO live animals data,
#'            corresponding to the "QCL" FAO bulk download query.
#' @param concordance_path The path to the muscle work concordance information.
#' @param amw_analysis_data_path The path to the animal muscle work analysis data.
#'
#' @export
#'
#' @examples
#' tidy_amw_pfu_data <- read.csv(file = MWTools::amw_test_data_path()) %>%
#'   calc_amw_pfu()
#'
calc_amw_pfu <- function(.df,
                         concordance_path = MWTools::fao_concordance_path(),
                         amw_analysis_data_path = MWTools::amw_analysis_data_path()
                         ){

  .df %>%
    tidy_fao_live_animals() %>%
    add_concordance_codes(concordance_path = concordance_path) %>%
    trim_fao_data() %>%
    get_working_species() %>%
    calc_working_animals(amw_analysis_data_path = amw_analysis_data_path) %>%
    calc_sector_split(amw_analysis_data_path) %>%
    calc_yearly_feed(amw_analysis_data_path) %>%
    calc_final_energy() %>%
    calc_primary_energy() %>%
    calc_useful_energy(amw_analysis_data_path) %>%
    tidy_pfu_data()
}
