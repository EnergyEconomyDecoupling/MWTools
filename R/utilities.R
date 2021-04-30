#' Gives a file path to the concordance data excel workbook
#'
#' @return a path to to the concordance data excel workbook bundled with this package
#'
#' @export
#'
#' @examples
#' fao_concordance_path()
fao_concordance_path <- function() {
  file.path("extdata", "FAO_ISO_MW_Mapping.xlsx") %>%
    system.file(package = "MWTools")
}

#' Gives a file path to the animal muscle work analysis excel workbook
#'
#' @return a path to to the animal muscle work analysis excel workbook bundled with this package
#'
#' @export
#'
#' @examples
#' amw_analysis_data_path()
amw_analysis_data_path <- function() {
  file.path("extdata", "amw_analysis_data.xlsx") %>%
    system.file(package = "MWTools")
}

#' Gives a file path to the FAO animal muscle work test data
#'
#' @return a path to to the FAO animal muscle work test data bundled with this package
#'
#' @export
#'
#' @examples
#' amw_test_data_path()
amw_test_data_path <- function() {
  file.path("extdata", "test_data") %>%
    system.file(package = "MWTools")
}
