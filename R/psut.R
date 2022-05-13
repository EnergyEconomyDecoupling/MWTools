
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
                             stage = MWTools::mw_constants$stage_col,
                             product = MWTools::mw_cols$product,
                             last_stage = MWTools::mw_cols$last_stage,
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
  # Add the Biomass entries to the outgoing data frame.
  out <- dplyr::bind_rows(biomass_resource_rows_R, biomass_resource_rows_U)

  # Biomass flows go into the V and U matrices.
  # These are final energy flows.
  biomass_rows <- .df %>%
    dplyr::filter(.data[[product]] == biomass)
  # Biomass entries in the V matrix
  biomass_rows_V <- biomass_rows %>%
    dplyr::mutate(
      "{matnames}" := V_name,
      "{rownames}" := farms,
      "{colnames}" := .data[[product]],
      "{rowtypes}" := industry_type,
      "{coltypes}" := product_type
    )
  # Biomass entries in the U matrix
  biomass_rows_U <- biomass_rows %>%
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
  food_feed_rows_V <- food_feed_rows %>%
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
  food_feed_rows_UY <- food_feed_rows %>%
    dplyr::mutate(
      # The matrix name depends on whether the last stage is final or useful.
      "{matnames}" := dplyr::case_when(
        .data[[last_stage]] == final ~ Y_name,
        .data[[last_stage]] == useful ~ U_name
      ),
      # The row name in U or Y is always the product.
      "{rownames}" := .data[[product]],
      # Column names depend on whether the last stage is final or useful.
      "{colnames}" := dplyr::case_when(
        # If last stage is final, the column in the Y matrix is the destination sector, and
        # the column name should be the sector into which this energy flows ultimately.
        .data[[last_stage]] == final ~ .data[[sector]],
        # If last stage is useful
        # (the only other option, so everything below *will* be energy for useful last stage),
        # the column in the U matrix is the specified name
        # of the human or animal machine.
        # If the species is a human being, then the column name (Industry) is
        # Species -> hu_mech.
        .data[[species]] %in% c(human_females, human_males) ~ RCLabels::paste_pref_suff(pref = .data[[species]],
                                                                                        suff = hu_mech,
                                                                                        notation = species_notation),
        # If we get here, we have taken care of all of the humans, so only animals remain.
        # When the species is an animal, the column name is again
        # Species -> Useful product,
        # but the Useful product can be either AnMech or AnP,
        # depending on the final demand category.
        # The energy product going into the Transport sector is AnP.
        .data[[sector]] == transport ~ RCLabels::paste_pref_suff(pref = .data[[species]],
                                                                 suff = an_p,
                                                                 notation = species_notation),
        # The energy product going into any other sector is AnMech.
        TRUE ~ RCLabels::paste_pref_suff(pref = .data[[species]],
                                         suff = an_mech,
                                         notation = species_notation)
      ),
      "{rowtypes}" := product_type,
      "{coltypes}" := industry_type
    )
  out <- out %>%
    dplyr::bind_rows(food_feed_rows_V, food_feed_rows_UY)

  # Useful energy rows go into the V and Y matrices.
  useful_rows <- .df %>%
    dplyr::filter(.data[[stage]] == useful)
  # Useful entries in the V matrix.
  useful_rows_V <- useful_rows %>%
    dplyr::mutate(
      "{matnames}" := V_name,
      "{rownames}" := .data[[species]],
      "{colnames}" := .data[[product]],
      "{rowtypes}" := industry_type,
      "{coltypes}" := product_type
    )
  useful_rows_Y <- useful_rows %>%
    dplyr::mutate(
      "{matnames}" := Y_name,
      "{rownames}" := .data[[product]],
      "{colnames}" := .data[[sector]],
      "{rowtypes}" := product_type,
      "{coltypes}" := industry_type
    )

  out %>%
    dplyr::bind_rows(useful_rows_V, useful_rows_Y)
}


#' Convert a tidy data frame to PSUT matrices
#'
#' A tidy data frame of muscle work information can be converted to
#' a `matsindf` data frame via this function.
#'
#' Prior to forming matrices, this function deletes unneeded columns
#' (columns that are neither metadata nor energy values).
#' It also aggregates data frame rows that will end up at the same
#' row, column location in the matrices.
#'
#' @param .df
#' @param country,year,method,energy_type,last_stage,unit,e_dot See `MWTools::mw_cols`.
#' @param matnames,matvals,rownames,colnames,rowtypes,coltypes See `MWTools::mat_meta_cols`.
#'
#' @return A `matsindf`-style, wide-by-matrices data frame of muscle work matrices.
#'
#' @export
#'
#' @examples
prep_psut <- function(.df,
                      # Metadata columns
                      country = MWTools::mw_cols$country,
                      year = MWTools::mw_cols$year,
                      method = MWTools::mw_cols$method,
                      energy_type = MWTools::mw_cols$energy_type,
                      last_stage = MWTools::mw_cols$last_stage,
                      unit = MWTools::mw_cols$unit,
                      e_dot = MWTools::mw_cols$e_dot,
                      matnames = MWTools::mat_meta_cols$matnames,
                      matvals = MWTools::mat_meta_cols$matvals,
                      rownames = MWTools::mat_meta_cols$rownames,
                      colnames = MWTools::mat_meta_cols$colnames,
                      rowtypes = MWTools::mat_meta_cols$rowtypes,
                      coltypes = MWTools::mat_meta_cols$coltypes) {


  trimmed_df <- .df %>%
    # Keep only the columns we need.
    dplyr::select(dplyr::all_of(c(country, year, method, energy_type, last_stage, unit, e_dot,
                                  matnames, rownames, colnames, rowtypes, coltypes)))
  grouping_symbols_summarise <- matsindf::everything_except(trimmed_df, e_dot)
  grouping_symbols_collapse <- rlang::syms(c(country, year, method, energy_type, last_stage, unit, matnames))
  trimmed_df %>%
    # Keep only the columns we need.
    dplyr::select(dplyr::all_of(c(country, year, method, energy_type, last_stage, unit, e_dot,
                  matnames, rownames, colnames, rowtypes, coltypes))) %>%
    # Group for the summarise operation
    # by everything except the energy column,
    # so we can aggregate rows whose energy belongs
    # in the same spot (row and column) in the PSUT matrices.
    # dplyr::group_by(!!!grouping_symbols_summarise) %>%
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    # Group for the collapse operation
    # dplyr::group_by(!!!rlang::enquos(!!!grouping_symbols_collapse)) %>%
    matsindf::group_by_everything_except(e_dot, matvals, rownames, colnames, rowtypes, coltypes) %>%
    # Create matrices
    matsindf::collapse_to_matrices(matvals = e_dot) %>%
    # Spread to be wide-by-matrices
    tidyr::pivot_wider(names_from = matnames, values_from = dplyr::all_of(e_dot))
}
