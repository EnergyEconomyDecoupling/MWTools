
#' Prep PFU data for conversion to PSUT matrices
#'
#' Primary-final-useful (PFU) data need to be prepared
#' for conversion to PSUT matrices by
#' duplicating some rows and adding a matrix name column.
#' This function performs that preparation.
#'
#' @param .pfu_df A PFU data frame,
#'                the output from `specify_primary_production()`.
#'
#' @return A data frame with correct rows (many duplicated) and a matrix name column.
#'
#' @export
#'
#' @examples
prep_mw_df <- function(.pfu_df,
                       biomass = MWTools::mw_products$biomass,
                       resources = MWTools::mw_sectors$resources_sector,
                       notation = RCLabels::from_notation) {

  biomass_from_resources <- RCLabels::paste_pref_suff(pref = biomass,
                                                      suff = resources,
                                                      notation = notation)

  # Create an empty outgoing data frame
  ncol_out <- ncol(.pfu_df) + 1 # +1 for the matnames columns
  out <- data.frame(matrix(ncol = ncol_out, nrow = 0)) %>%
    tibble::as.tibble()
  colnames(out) <- c(colnames(.pfu_df), matnames)


}
