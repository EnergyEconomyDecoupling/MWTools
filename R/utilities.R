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
