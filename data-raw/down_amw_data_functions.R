#' Download live animals data from FAOSTAT
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
#' live_animals_data <- down_fao_live_animals(data_folder = file.path(fs::home_path(), "FAO_data"))
#'
down_fao_live_animals <- function(download_location = MWTools::extdata_path(),
                                  live_animals_code = MWTools::fao_codes$live_animals_code){

  # Download .zip file containing data for the number of live animals into a specified folder
  amw_data <- FAOSTAT::get_faostat_bulk(code = live_animals_code, data_folder = download_location)

  write.csv(x = amw_data, file = file.path(download_location, "amw_data.csv"))

  }

# Call down_fao_live_animals() function, activated when sourcing down_amw_data_functions.R
down_fao_live_animals()


