#' Gives a file path to the concordance data excel workbook
#'
#' @return a path to to the concordance data excel workbook bundled with this package
#'
#' @export
#'
#' @examples
#' fao_concordance_path()
fao_concordance_path <- function() {
  file.path("extdata", "test_data",  "FAO_ISO_MW_Mapping.xlsx") %>%
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
  file.path("extdata", "test_data",  "amw_analysis_data.xlsx") %>%
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
  file.path("extdata", "test_data", "test_amw_data.csv") %>%
    system.file(package = "MWTools")
}

#' Gives a file path to the human muscle work analysis excel workbook
#'
#' @return a path to to the human muscle work analysis excel workbook bundled with this package
#'
#' @export
#'
#' @examples
#' hmw_analysis_data_path()
hmw_analysis_data_path <- function() {
  file.path("extdata", "test_data",  "hmw_analysis_data.xlsx") %>%
    system.file(package = "MWTools")
}

#' Gives a file path to the ILO human muscle work test data
#'
#' @return a path to to the ILO human muscle work test data bundled with this package
#'
#' @export
#'
#' @examples
#' hmw_test_data_path()
hmw_test_data_path <- function() {
  file.path("extdata", "test_data", "test_hmw_data.csv") %>%
    system.file(package = "MWTools")
}
