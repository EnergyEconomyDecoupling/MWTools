#' Create a phi vector for muscle work energy conversion chains
#'
#' To move from an energy to an exergy description of an energy conversion chain,
#' a vector of phi (exergy-to-energy ratio) values is needed.
#' Entries in the vector are for each type of muscle work energy carrier.
#'
#' @param .phi_table A table of phi (exergy-to-energy ratio) values.
#' @param mw_energy_carriers A vector of energy carriers relevant to muscle work calculations.
#'
#' @return A column vector of phi values.
#'
#' @export
phi_vec_mw <- function(.phi_table = MWTools::phi_constants_mw,
                       mw_energy_carriers = MWTools::mw_products,
                       product = MWTools::mw_cols$product,
                       product_type = MWTools::row_col_types$product,
                       phi = "phi") {

  # Keep only those rows in the .phi_table that are also mw_energy_carriers.
  # First form a data frame from the mw_energy_carrier vector
  carriers <- tibble::tibble(x = mw_energy_carriers %>% unlist() %>% unname()) %>%
    magrittr::set_names(product)

  keep <- dplyr::semi_join(.phi_table, carriers, by = product)

  # Create the vector as a matrix with 1 column.
  matrix(1, nrow = nrow(keep), ncol = 1,
         dimnames = list(keep[[product]] %>% unlist() %>% unname(), phi)) %>%
    matsbyname::setrowtype(product_type)
}
