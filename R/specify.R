

#' Specify a data frame of PFU muscle work data
#'
#' The specification process adds entries (rows)
#' needed for conversion to PSUT matrices.
#'
#' @param .df A data frame, likely produced by `calc_amw_pfu()` or `calc_hmw_pfu()`.
#' @param product See `MWTools::mw_constants`.
#' @param primary,final,useful See `MWTools::all_stages`.
#' @param species See `MWTools::mw_constants`.
#' @param human See `MWTools::mw_species`.
#' @param stage,sector See `MWTools::mw_constants`.
#' @param biomass,food,feed,hu_mech,an_mech,an_p See `MWTools::mw_products`.
#' @param transport See `MWTools::sectors`.
#'
#' @return A data frame with additional rows needed for converting to PSUT matrices.
#'
#' @export
#'
#' @examples
#' hmw_test_data_path() %>%
#'   read.csv() %>%
#'   calc_hmw_pfu() %>%
#'   specify_product()
#' amw_test_data_path() %>%
#'   read.csv() %>%
#'   calc_amw_pfu() %>%
#'   specify_product()
specify_product <- function(.df,
                            product = MWTools::mw_constants$product,
                            primary = MWTools::all_stages$primary,
                            final = MWTools::all_stages$final,
                            useful = MWTools::all_stages$useful,
                            species = MWTools::mw_constants$species,
                            human = MWTools::mw_species$human,
                            stage = MWTools::mw_constants$stage_col,
                            sector = MWTools::mw_constants$sector_col,
                            biomass = MWTools::mw_products$biomass,
                            food = MWTools::mw_products$food,
                            feed = MWTools::mw_products$feed,
                            hu_mech = MWTools::mw_products$hu_mech,
                            an_mech = MWTools::mw_products$an_mech,
                            an_p = MWTools::mw_products$an_p,
                            transport = MWTools::mw_sectors$transport_sector) {

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


#' Add primary production to a data frame of PFU muscle work data
#'
#' Adds rows for biomass from resources
#'
#' @param .df A muscle work data frame with products already specified,
#'            usually by `specify_product()`.
#' @param product See `MWTools::mw_constants`.
#' @param primary See `MWTools::all_stages`.
#' @param stage See `MWTools::mw_constants`.
#' @param notation The notation to be used for primary energy product specification.
#'                 Default is `RCLabels::from_notation`.
#' @param resources See `MWTools::mw_sectors`.
#'
#' @return A data frame with
#'
#' @export
#'
#' @examples
#' hmw_test_data_path() %>%
#'   read.csv() %>%
#'   calc_hmw_pfu() %>%
#'   specify_product() %>%
#'   specify_primary_production()
#' amw_test_data_path() %>%
#'   read.csv() %>%
#'   calc_amw_pfu() %>%
#'   specify_product() %>%
#'   specify_primary_production()
specify_primary_production <- function(.df,
                                       product = MWTools::mw_constants$product,
                                       primary = MWTools::all_stages$primary,
                                       stage = MWTools::mw_constants$stage_col,
                                       notation = RCLabels::from_notation,
                                       resources = MWTools::mw_sectors$resources_sector) {

  # Find all primary rows
  primary_rows <- .df %>%
    dplyr::filter(.data[[stage]] == primary)
  # Modify the product
  primary_rows <- primary_rows %>%
    dplyr::mutate(
      "{product}" := RCLabels::paste_pref_suff(pref = .data[[product]], suff = resources, notation = notation)
    )
  dplyr::bind_rows(.df, primary_rows)
}
