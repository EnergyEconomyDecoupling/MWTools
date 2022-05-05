

#' Specify a data frame of PFU muscle work data
#'
#' The specification process adds entries (rows)
#' needed for conversion to PSUT matrices.
#'
#' @param .df A data frame, likely produced by `calc_amw_pfu()` or `calc_hmw_pfu()`.
#'
#' @return A data frame with additional rows needed for converting to PSUT matrices.
#'
#' @export
#'
#' @examples
#'
specify_product <- function(.df,
                            product = IEATools::iea_cols$product,
                            primary = IEATools::all_stages$primary,
                            final = IEATools::all_stages$final,
                            useful = IEATools::all_stages$useful,
                            species = MWTools::mw_constants$species,
                            human = MWTools::mw_species$human,
                            stage = MWTools::mw_constants$stage_col,
                            sector = MWTools::mw_constants$sector_col,
                            biomass = MWTools::mw_products$biomass,
                            food = MWTools::mw_products$food,
                            feed = MWTools::mw_products$feed,
                            transport = MWTools::amw_analysis_constants$transport_sector,
                            hu_mech = MWTools::mw_products$hu_mech,
                            an_mech = MWTools::mw_products$an_mech,
                            an_p = MWTools::mw_products$an_p) {

  # Go through each row of .df to create the specified data frame.


  # Add a Product column
  .df %>%
    dplyr::mutate(
      "{product}" := dplyr::case_when(
        .data[[stage]] == primary ~ biomass,
        .data[[stage]] == final & startsWith(.data[[species]], human) ~ food,
        .data[[stage]] == final ~ feed,
        .data[[stage]] == useful & startsWith(.data[[species]], human) ~ hu_mech,
        .data[[stage]] == useful & .data[[sector]] == transport ~ an_p,
        .data[[stage]] == useful ~ an_mech,
        TRUE ~ NA_character_
      )
    )
}
