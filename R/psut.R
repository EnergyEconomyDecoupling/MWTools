
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
#'   add_row_col_meta()
add_row_col_meta <- function(.df,
                             # Products (energy carriers)
                             biomass = MWTools::mw_products$biomass,
                             food = MWTools::mw_products$food,
                             feed = MWTools::mw_products$feed,
                             hu_mech = MWTools::mw_products$hu_mech,
                             an_mech = MWTools::mw_products$an_mech,
                             an_p = MWTools::mw_products$an_p,
                             # Industries (species, sectors, energy conversion machines)
                             resources = MWTools::mw_sectors$resources_sector,
                             farms = MWTools::mw_sectors$farms,
                             food_production = MWTools::mw_sectors$food_production,
                             feed_production = MWTools::mw_sectors$feed_production,
                             human_females = MWTools::mw_species$human_females,
                             human_males = MWTools::mw_species$human_males,
                             transport = MWTools::mw_sectors$transport_sector,
                             # Stages
                             final = MWTools::all_stages$final,
                             useful = MWTools::all_stages$useful,
                             # Column names
                             species = MWTools::mw_constants$species,
                             sector = MWTools::mw_constants$sector_col,
                             product = MWTools::mw_constants$product,
                             last_stage = MWTools::mw_constants$last_stage,
                             matnames = MWTools::mat_meta_cols$matnames,
                             rownames = MWTools::mat_meta_cols$rownames,
                             colnames = MWTools::mat_meta_cols$colnames,
                             rowtypes = MWTools::mat_meta_cols$rowtypes,
                             coltypes = MWTools::mat_meta_cols$coltypes,
                             # Matrix names
                             R_name = MWTools::psut_cols$R,
                             U_name = MWTools::psut_cols$U,
                             V_name = MWTools::psut_cols$V,
                             Y_name = MWTools::psut_cols$Y,
                             # Row and column types
                             industry_type = MWTools::row_col_types$industry,
                             product_type = MWTools::row_col_types$product,
                             # Row and column notations for products, resources, and species
                             product_notation = RCLabels::from_notation,
                             resource_notation = RCLabels::of_notation,
                             species_notation = RCLabels::arrow_notation) {

  biomass_from_resources_label <- RCLabels::paste_pref_suff(pref = biomass,
                                                            suff = resources,
                                                            notation = product_notation)
  biomass_resource_sector <- biomass_from_resources_label %>%
    RCLabels::switch_notation(from = product_notation, to = resource_notation, flip = TRUE)

  # Biomass [from Resources] flows go into the R and U matrices.
  # These are primary energy flows.
  biomass_resource_rows <- .df %>%
    dplyr::filter(.data[[product]] == biomass_from_resources_label)
  # Biomass [from Resources] entries in the R matrix
  biomass_resource_rows_R <- biomass_resource_rows %>%
    dplyr::mutate(
      "{matnames}" := R_name,
      "{rownames}" := biomass_resource_sector,
      "{colnames}" := .data[[product]],
      "{rowtypes}" := industry_type,
      "{coltypes}" := product_type
    )
  # Biomass [from resources] entries in the U matrix
  biomass_resource_rows_U <- biomass_resource_rows %>%
    dplyr::mutate(
      "{matnames}" := U_name,
      "{rownames}" := .data[[product]],
      "{colnames}" := farms,
      "{rowtypes}" := product_type,
      "{coltypes}" := industry_type
    )
  # Add the Biomass [from Resources] entries to the outgoing data frame.
  out <- dplyr::bind_rows(biomass_resource_rows_R, biomass_resource_rows_U)

  # Biomass flows go into the V and U matrices.
  # These are final energy flows.
  biomass_rows <- .df %>%
    dplyr::filter(.data[[product]] == biomass)
  # Biomass entries in the V matrix
  biomass_rows_V <- .df %>%
    dplyr::mutate(
      "{matnames}" := V_name,
      "{rownames}" := farms,
      "{colnames}" := .data[[product]],
      "{rowtypes}" := industry_type,
      "{coltypes}" := product_type
    )
  # Biomass entries in the U matrix
  biomass_rows_U <- .df %>%
    dplyr::mutate(
      "{matnames}" := U_name,
      "{rownames}" := .data[[product]],
      "{colnames}" := dplyr::case_when(
        (.data[[species]] %in% c(human_females, human_males)) ~ food_production,
        TRUE ~ feed_production
      ),
      "{rowtypes}" := product_type,
      "{coltypes}" := industry_type
    )
  out <- out %>%
    dplyr::bind_rows(biomass_rows_V, biomass_rows_U)

  # Food and Feed rows go into the V and U matrices.
  # These are final energy flows.
  food_feed_rows <- .df %>%
    dplyr::filter(.data[[product]] %in% c(food, feed))
  # Food and Feed entries in the V matrix.
  food_feed_rows_V <- .df %>%
    dplyr::mutate(
      "{matnames}" := V_name,
      "{rownames}" := dplyr::case_when(
        .data[[product]] == food ~ food_production,
        .data[[product]] == feed ~ feed_production,
        TRUE ~ NA_character_
      ),
      "{colnames}" := .data[[product]], # product will be Food or Feed.
      "{rowtypes}" := industry_type,
      "{coltypes}" := product_type
    )
  # Food and Feed entries in the U or Y matrix.
  # Set some of the names we'll use later.
  # horses_anmech <- RCLabels::paste_pref_suff(pref = horses, suff = an_mech, notation = species_notation)
  food_feed_rows_UY <- .df %>%
    dplyr::mutate(
      # The matrix name depends on whether the last stage is final or useful.
      "{matnames}" := dplyr::case_when(
        .data[[last_stage]] == final ~ U_name,
        .data[[last_stage]] == useful ~ Y_name
      ),
      # The row name in U or Y is always the product.
      "{rownames}" := .data[[product]],
      # Column names depend on whether the last stage is final or useful.
      "{colnames}" := dplyr::case_when(
        # If last stage is final, the column in the Y matrix is the destination sector.
        .data[[last_stage]] == final ~ .data[[sector]],
        # If last stage is useful
        # (the only other option, so everything following *will* be last stage useful),
        # the column in the U matrix is the specified name
        # of the human or animal machine.
        # If the species is a human, then the column name (Industry) is
        # Species -> hu_mech.
        .data[[species]] %in% c(human_females, human_males) ~ RCLabels::paste_pref_suff(pref = .data[[species]],
                                                                                        suff = hu_mech,
                                                                                        notation = species_notation),
        # If we get here, we have taken care of all of the humans, so only animals remain.
        # When the species is an animal, the column name is again
        # Species -> Useful product,
        # but the Useful product can be either AnMech or AnP,
        # depending on the final demand category (Transport for AnP or Agriculture for everything else).
        .data[[sector]] == transport ~ RCLabels::paste_pref_suff(pref = .data[[species]],
                                                                 suff = an_p,
                                                                 notation = species_notation),
        TRUE ~ RCLabels::paste_pref_suff(pref = .data[[species]],
                                         suff = an_mech,
                                         notation = species_notation)
      ),
      "{rowtypes}" := product_type,
      "{coltypes}" := industry_type
    )
  out <- out %>%
    dplyr::bind_rows(food_feed_rows_V, food_feed_rows_UY)

  # Useful energy rows (V and Y)
}
