
#' Prep PFU data for conversion to PSUT matrices
#'
#' Primary-final-useful (PFU) data need to be converted to PSUT matrices.
#' This function take the output of `specify_last_stages()`
#' and converts to PSUT matrices in a data frame.
#'
#' @param .df A data frame containing muscle work data,
#'            likely the output from `specify_last_stages()`.
#'
#' @return A data frame containing PSUT matrices.
#'
#' @export
#'
#' @examples
#' hmw_df <- hmw_test_data_path() %>%
#'   read.csv() %>%
#'   calc_hmw_pfu()
#' amw_df <- amw_test_data_path() %>%
#'   read.csv() %>%
#'   calc_amw_pfu()
#' specify_product(hmw_df, amw_df) %>%
#'   MWTools::specify_primary_production() %>%
#'   specify_useful_products() %>%
#'   specify_fu_machines() %>%
#'   specify_last_stages() %>%
#'   prep_psut()
prep_psut <- function(.df,
                      biomass = MWTools::mw_products$biomass,
                      resources = MWTools::mw_sectors$resources_sector,
                      farms = MWTools::mw_sectors$farms,
                      product = MWTools::mw_constants$product,
                      matnames = MWTools::mat_meta_cols$matnames,
                      rownames = MWTools::mat_meta_cols$rownames,
                      colnames = MWTools::mat_meta_cols$colnames,
                      rowtypes = MWTools::mat_meta_cols$rowtypes,
                      coltypes = MWTools::mat_meta_cols$coltypes,
                      R_name = MWTools::psut_cols$R,
                      U_name = MWTools::psut_cols$U,
                      V_name = MWTools::psut_cols$V,
                      Y_name = MWTools::psut_cols$Y,
                      industry_type = MWTools::row_col_types$industry,
                      product_type = MWTools::row_col_types$product,
                      product_notation = RCLabels::from_notation,
                      resource_notation = RCLabels::of_notation,
                      species_notation = RCLabels::arrow_notation) {

  biomass_from_resources_label <- RCLabels::paste_pref_suff(pref = biomass,
                                                            suff = resources,
                                                            notation = product_notation)
  biomass_resource_sector <- biomass_from_resources_label %>%
    RCLabels::switch_notation(from = product_notation, to = resource_notation, flip = TRUE)

  # Deal with biomass resource flows (R and U).
  biomass_resource_rows <- .df %>%
    dplyr::filter(.data[[product]] == biomass_from_resources_label)
  # Biomass resource entries in the R matrix
  biomass_resource_rows_R <- biomass_resource_rows %>%
    dplyr::mutate(
      "{matnames}" := R_name,
      "{rownames}" := biomass_resource_sector,
      "{colnames}" := .data[[product]],
      "{rowtypes}" := industry_type,
      "{coltypes}" := product_type
    )
  # Biomass resource entries in the U matrix
  biomass_resource_rows_U <- biomass_resource_rows %>%
    dplyr::mutate(
      "{matnames}" := U_name,
      "{rownames}" := .data[[product]],
      "{colnames}" := farms,
      "{rowtypes}" := product_type,
      "{coltypes}" := industry_type
    )
  # Add the R and U matrix entries to the outgoing data frame.
  out <- dplyr::bind_rows(biomass_resource_rows_R, biomass_resource_rows_U)

  # Biomass rows (V and U)


  # Food and Feed rows (V and U)


  # Useful energy rows (V and Y)
}
