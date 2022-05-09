
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
                       product = MWTools::mw_constants$product,
                       matnames = MWTools::mw_constants$matnames_col,
                       rownames = MWTools::mw_constants$rownames_col,
                       colnames = MWTools::mw_constants$colnames_col,
                       rowtypes = MWTools::mw_constants$rowtypes_col,
                       coltypes = MWTools::mw_constants$coltypes_col,
                       R_mat = MWTools::psut_cols$R,
                       U_mat = MWTools::psut_cols$U,
                       V_mat = MWTools::psut_cols$V,
                       Y_mat = MWTools::psut_cols$Y,
                       industry_type = MWTools::mw_constants$industry_type,
                       product_type = MWTools::mw_constants$product_type,
                       product_notation = RCLabels::from_notation,
                       resource_notation = RCLabels::of_notation,
                       species_notation = RCLabels::arrow_notation) {

  biomass_from_resources <- RCLabels::paste_pref_suff(pref = biomass,
                                                      suff = resources,
                                                      notation = product_notation)
  biomass_resource_sector <- biomass_from_resources %>%
    RCLabels::switch_notation(from = product_notation, to = resource_notation, flip = TRUE)

  # Deal with resource flows.
  biomass_resource_rows <- .pfu_df %>%
    dplyr::filter(.data[[product]] == biomass_from_resources)
  biomass_resource_rows_R <- biomass_resource_rows %>%
    dplyr::mutate(
      "{matnames}" := R_mat,
      "{rownames}" := biomass_resource_sector,
      "{colnames}" := .data[[product]],
      "{rowtypes}" := industry_type,
      "{coltypes}" := product_type
    )
  biomass_resource_rows_U <- biomass_resource_rows %>%
    dplyr::mutate(
      "{matnames}" := "V"
    )
  # Each of these rows gets an entry in the R matrix and an entry in the V matrix.
  out <- dplyr::bind_rows(biomass_resource_rows_R, biomass_resource_rows_V)


}
