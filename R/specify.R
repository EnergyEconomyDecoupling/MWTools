

#' Specify a data frame of PFU muscle work data
#'
#' The specification process adds entries (rows)
#' needed for conversion to PSUT matrices.
#'
#' This function binds `.hmw_df` and `.amw_df` by rows.
#'
#' @param .hmw_df A data frame, likely produced by `calc_hmw_pfu()`.
#' @param .amw_df A data frame, likely produced by `calc_amw_pfu()`.
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
specify_product <- function(.hmw_df, .amw_df,
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

  dplyr::bind_rows(.hmw_df, .amw_df) %>%
    # Add a Product column
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
#' @return A data frame with primary production specified.
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
#'   MWTools::specify_primary_production()
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


#' Specify useful energy products
#'
#' Final-to-useful machines make useful energy products.
#' This function specifies those products
#' to include a `[from X]` suffix.
#'
#' @param .df A data frame, usually the output of `MWTools::specify_primary_production()`.
#'
#' @return A data frame with
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
#'   specify_useful_products()
specify_useful_products <- function(.df,
                                    product = MWTools::mw_constants$product,
                                    useful = MWTools::all_stages$useful,
                                    stage = MWTools::mw_constants$stage_col,
                                    species = MWTools::mw_constants$species,
                                    notation = RCLabels::from_notation) {
  # Find all useful rows
  useful_rows <- .df %>%
    dplyr::filter(.data[[stage]] == useful)
  # Specify the products in the useful rows
  specified_useful_rows <- useful_rows %>%
    dplyr::mutate(
      "{product}" := RCLabels::paste_pref_suff(pref = .data[[product]],
                                               suff = .data[[species]], ###### Not right.
                                               notation = notation)
    )
  .df %>%
    # Eliminate useful energy rows
    dplyr::filter(.data[[stage]] != useful) %>%
    # Bind the specified useful rows to the bottom of the data frame.
    dplyr::bind_rows(specified_useful_rows)
}
