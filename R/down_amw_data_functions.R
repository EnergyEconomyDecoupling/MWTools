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


#' Add 3 letter ISO codes and MWTools-specific region codes
#'
#' Adds 3 letter ISO country codes and muscle work region codes for countries
#' present in the FAO's Live animals data, downloaded through the `FAOSTAT` package,
#' usually using the `MWTools::down_fao_live_animals` function.
#'
#'
#' @param .live_animals
#' @param concordance_path
#' @param mw_region_code_col
#' @param country_name
#' @param country_incl_col
#' @param country_code_col
#' @param country_code_pfu_col
#'
#' @return
#' @export
#'
#' @examples
add_concordance_codes <- function(.live_animals,
                                  concordance_path,
                                  mw_region_code_col = MWTools::amw_analysis_constants$mw_region_code_col,
                                  country_name = MWTools::mw_constants$country_name,
                                  country_incl_col = MWTools::conc_cols$country_incl_col,
                                  country_code_col = MWTools::conc_cols$country_code_col,
                                  country_code_pfu_col = MWTools::conc_cols$country_code_pfu_col){

  # Read bundled concordance data
  concordance_data <- readxl::read_xlsx(path = concordance_path,
                                        sheet = "Mapping") %>%
    magrittr::set_colnames(c(country_name, country_incl_col, country_code_col, country_code_pfu_col, mw_region_code_col))

  .live_animals %>%
    dplyr::left_join(concordance_data, by = country_name) %>%
    dplyr::relocate(country_code_col, .before = country_name) %>%
    dplyr::relocate(mw_region_code_col, .before = country_name)
}


#' Trim countries in FAO live animals data
#'
#' This function filters out countries which have more than one instance
#' and aggregate regions from FAO live animals data. Data is usually downloaded
#' through the `FAOSTAT` package, usually using the `MWTools::down_fao_live_animals`
#' function, and after applying the `MWTools::add_concordance_codes` function.
#'
#' @return
#' @export
#'
#' @examples
trim_fao_data <- function(.live_animals_with_codes,
                          country_incl_col = MWTools::conc_cols$country_incl_col,
                          country_code_pfu_col = MWTools::conc_cols$country_code_pfu_col){

  .live_animals_with_codes %>%
    dplyr::filter(.data[[country_incl_col]] == "Yes") %>%
    dplyr::select(-country_incl_col, -country_code_pfu_col)

}



