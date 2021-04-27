library(magrittr)

#' Download live animals data from FAO
#'
#' This function uses the bulk download facility function from the R package
#' `FAOSTAT` to download data for the number of live animals by country over time.
#'
#' @param data_folder A user-specified file path to the directory in which to
#'                    download the FAO live animals data.
#' @param live_animals_code See `MWTools::fao_codes$live_animals_code`.
#'
#' @return
#' @export
#'
#' @examples
#' down_fao_live_animals(data_folder = file.path(fs::home_path(), "FAO_data"))
#'
down_fao_live_animals <- function(data_folder,
                                  live_animals_code = MWTools::fao_codes$live_animals_code){

  # Download .zip file containing data for the number of live animals into a specified folder
  FAOSTAT::get_faostat_bulk(code = live_animals_code, data_folder = data_folder)

}


#' Tidy live animals data from the FAO
#'
#' Create a tidy data frame using data supplied by the function
#' `MWTools::down_fao_live_animals`.
#'
#' @param data_folder A user-specified file path to the directory in which to
#'                    download the FAO live animals data.
#' @param country_name,species,year,unit,value See `MWTools::mw_constants`.
#' @param area_fao_col,item_fao_col,year_fao_col,unit_fao_col,value_fao_col
#'        See `MWTools::fao_cols`.
#'
#' @return
#' @export
#'
#' @examples
#' tidy_live_animals_data <- MWTools::down_fao_live_animals(data_folder = file.path(fs::home_path(), "FAO_data")) %>%
#'   tidy_fao_live_animals(data_folder = file.path(fs::home_path(), "FAO_data"))
#'
tidy_fao_live_animals <- function(data_folder,
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

  # Read .zip file into a tidy data frame
  live_animals <- FAOSTAT::read_faostat_bulk(zip_file_name = file.path(data_folder,
                                                                       list.files(data_folder,
                                                                                  pattern = ".zip"))) %>%
    dplyr::select(area_fao_col, item_fao_col, year_fao_col, unit_fao_col, value_fao_col) %>% # Do I need to use the .data[[]] syntax here?
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
    dplyr::select(-.data[[unit]])

  # Returns tidy data
  return(live_animals_final)
}


#' Create a data frame containing the total number of animals of working species

#' Using a tidy dataframe containing the number of live animals, this function
#' restricts the species of animals to: Asses, Camels, Cattle, Horses, Mules,
#' Buffaloes, and Camelids, other; then combines Camels and Camlelids, other
#' into a combines "Camelids" species group.

#' @param live_animals A tidy data frame containing the number of live animals by
#'                     country over time. Usually supplied from the FAO through
#'                     the functions `MWTools::down_fao_live_animals` and
#'                     `MWTools::tidy_fao_live_animals`.
#' @param species,value See `MWTools::mw_constants`.
#' @param asses,camels,cattle,horses,mules,buffaloes,camelids_other,camelids
#'        See `MWTools::mw_species`.
#'
#' @return
#' @export
#'
#' @examples
#' working_speies_data <- MWTools::down_fao_live_animals(data_folder = file.path(fs::home_path(), "FAO_data")) %>%
#'   MWTools::tidy_fao_live_animals(data_folder = file.path(fs::home_path(), "FAO_data")) %>%
#'   MWTools::get_working_species()
#'
get_working_species <- function(live_animals,
                                species = MWTools::mw_constants$species,
                                country_name = MWTools::mw_constants$country_name,
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

  # Filter data to only include working animal species
  working_species <- live_animals %>%
    dplyr::filter(Species %in% c(asses,
                                 camels,
                                 cattle,
                                 horses,
                                 mules,
                                 buffaloes,
                                 camelids_other)) %>%
    # Combine "Camels" and "Camelids, other" into "Camelids".
    tidyr::pivot_wider(names_from = species,
                       values_from = value) %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(
      "{camelids}" := .data[[camels]] + .data[[camelids_other]],
      .keep = "unused"
      ) %>%
    tidyr::pivot_longer(cols = c(asses, buffaloes, camelids, cattle, horses, mules),
                        names_to = species,
                        values_to = value) %>%
    magrittr::set_colnames(c(country_name, year, species, live_animals_col))

  # Returns a tidy data frame containing the number of working animals
  return(working_species)

}



