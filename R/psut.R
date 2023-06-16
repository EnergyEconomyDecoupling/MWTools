
#' Prep PFU data for conversion to PSUT matrices
#'
#' Primary-final-useful (PFU) data need to be converted to PSUT matrices.
#' This function take the output of `specify_last_stages()`
#' and converts to PSUT matrices in a data frame.
#'
#' @param .df A data frame containing muscle work data,
#'            likely the output from `specify_last_stages()`.
#' @param biomass,food,feed,hu_mech,an_mech,an_p See `MWTools::mw_products`.
#' @param resources,farms,food_production,feed_production,transport See `MWTools::mw_sectors`
#' @param human_females,human_males See `MWTools::mw_species`.
#' @param final,useful See `MWTools::all_stages`.
#' @param species,sector,stage See `MWTools::mw_constants`
#' @param product,last_stage See `MWTools::mw_cols`.
#' @param matnames,rownames,colnames,rowtypes,coltypes See `MWTools::mat_meta_cols`.
#' @param R_name,U_name,V_name,Y_name See `MWTools::psut_cols`.
#' @param industry_type,product_type See `MWTools::row_col_types`.
#' @param product_notation The notation for products. Default is `RCLabels::from_notation`.
#' @param resource_notation The notation for resources. Default is `RCLabels::of_notation`.
#' @param species_notation The notation for species. Default is `RCLabels::arrow_notation`.
#'
#' @return A data frame containing PSUT matrices
#'         representing muscle work energy conversion chains.
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
#' specify_energy_type_method(hmw_df, amw_df) %>%
#'   specify_product() %>%
#'   specify_TJ() %>%
#'   MWTools::specify_primary_production() %>%
#'   specify_useful_products() %>%
#'   specify_fu_machines() %>%
#'   specify_last_stages() %>%
#'   MWTools::add_row_col_meta()
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
                             transport = MWTools::mw_sectors$transport_sector,
                             human_females = MWTools::mw_species$human_females,
                             human_males = MWTools::mw_species$human_males,
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
  if (nrow(food_feed_rows) == 0) {
    # Add the columns to the empty data frame to avoid the error
    # in paste_pref_suff() that comes when pref or suff has character().
    food_feed_rows_UY <- food_feed_rows %>%
      dplyr::mutate(
        "{matnames}" := character(),
        "{rownames}" := character(),
        "{colnames}" := character(),
        "{rowtypes}" := character(),
        "{coltypes}" := character()
      )
  } else {
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
  }
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
#' @param .df A data frame created by `add_row_col_meta()` so that it contains
#'            metadata columns for creating PSUT matrices.
#' @param matrix_class The type of matrix to be created, one of "matrix" or "Matrix".
#'                     Default is "matrix".
#' @param country,year,method,energy_type,last_stage,unit,e_dot See `MWTools::mw_cols`.
#' @param matnames,matvals,rownames,colnames,rowtypes,coltypes See `MWTools::mat_meta_cols`.
#'
#' @return A `matsindf`-style, wide-by-matrices data frame of muscle work matrices.
#'
#' @export
#'
#' @examples
#' hmw_df <- hmw_test_data_path() %>%
#'   read.csv() %>%
#'   calc_hmw_pfu() %>%
#'   # Keep only a few years for speed.
#'   dplyr::filter(Year %in% 2000:2002)
#' amw_df <- amw_test_data_path() %>%
#'   read.csv() %>%
#'   calc_amw_pfu() %>%
#'   # Keep only a few years for speed.
#'   dplyr::filter(Year %in% 2000:2002)
#' specify_energy_type_method(hmw_df, amw_df) %>%
#'   specify_product() %>%
#'   specify_TJ() %>%
#'   MWTools::specify_primary_production() %>%
#'   specify_useful_products() %>%
#'   specify_fu_machines() %>%
#'   specify_last_stages() %>%
#'   MWTools::add_row_col_meta() %>%
#'   MWTools::collapse_to_psut()
collapse_to_psut <- function(.df,
                             matrix_class = c("matrix", "Matrix"),
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

  matrix_class <- match.arg(matrix_class)

  trimmed_df <- .df %>%
    # Keep only the columns we need.
    dplyr::select(dplyr::all_of(c(country, year, method, energy_type, last_stage, unit, e_dot,
                                  matnames, rownames, colnames, rowtypes, coltypes)))
  trimmed_df %>%
    # Keep only the columns we need.
    dplyr::select(dplyr::all_of(c(country, year, method, energy_type, last_stage, unit, e_dot,
                                  matnames, rownames, colnames, rowtypes, coltypes))) %>%
    # Group for the summarise operation
    # by everything except the energy column,
    # so we can aggregate rows whose energy belongs
    # in the same spot (row and column) in the PSUT matrices.
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    # Group for the collapse operation
    matsindf::group_by_everything_except(e_dot, matvals, rownames, colnames, rowtypes, coltypes) %>%
    # Create matrices
    matsindf::collapse_to_matrices(matvals = e_dot, matrix_class = matrix_class) %>%
    # Spread to be wide-by-matrices
    tidyr::pivot_wider(names_from = matnames, values_from = dplyr::all_of(e_dot))
}


#' Calculate an S_units vector given other PSUT matrices
#'
#' The `S_units` vector is a unit summation vector
#' with products (energy carriers) in rows and units in columns.
#' The row names for each vector are the various products in that row of `.df`.
#' The column names for each vector are taken from the `units` column of `.df`.
#'
#' This function employs `matsindf::matsindf_apply()` internally, so
#' `unit` can be a single string or the name of a column in `.df`.
#' Similarly, `R`, `U`, `V`, and `Y` can be either matrices or the names of columns in `.df`.
#'
#' Product names are taken from the prefixes of row or columns names
#' of the `R`, `U`, `V`, and `Y` matrices.
#'
#' The `unit` column will remain in `.df` on output and will need to be deleted afterward.
#'
#' @param .df A data frame. Default is `NULL`.
#' @param matrix_class The type of matrix to be created, one of "matrix" or "Matrix".
#'                     Default is "matrix".
#' @param unit A string unit for each row or the name of the unit column in `.df`. See `MWTools::mw_cols`.
#' @param R,U,V,Y PSUT matrices or the names of matrix columns in `.df`. See `MWTools::psut_cols`.
#' @param s_units The name of the output matrix or the output column. See `MWTools::psut_cols`.
#' @param product_notation Notation for products. Default is `RCLabels::from_notation`.
#' @param product_type,unit_type The types for products and units columns. See `MWTools::row_col_types$unit`.
#'
#' @return `.df` with an `s_units` column added.
#'
#' @export
#'
#' @examples
#' hmw_df <- hmw_test_data_path() %>%
#'   read.csv() %>%
#'   calc_hmw_pfu() %>%
#'   # Keep only a few years for speed.
#'   dplyr::filter(Year %in% 2000:2002)
#' amw_df <- amw_test_data_path() %>%
#'   read.csv() %>%
#'   calc_amw_pfu() %>%
#'   # Keep only a few years for speed.
#'   dplyr::filter(Year %in% 2000:2002)
#' specify_energy_type_method(hmw_df, amw_df) %>%
#'   specify_product() %>%
#'   specify_TJ() %>%
#'   MWTools::specify_primary_production() %>%
#'   specify_useful_products() %>%
#'   specify_fu_machines() %>%
#'   specify_last_stages() %>%
#'   MWTools::add_row_col_meta() %>%
#'   MWTools::collapse_to_psut() %>%
#'   calc_S_units()
calc_S_units <- function(.df = NULL,
                         matrix_class = c("matrix", "Matrix"),
                         # Input columns
                         unit = MWTools::mw_cols$unit,
                         R = MWTools::psut_cols$R,
                         U = MWTools::psut_cols$U,
                         V = MWTools::psut_cols$V,
                         Y = MWTools::psut_cols$Y,
                         # Output column
                         s_units = MWTools::psut_cols$s_units,
                         # Miscellaneous information
                         product_notation = RCLabels::from_notation,
                         product_type = MWTools::row_col_types$product,
                         unit_type = MWTools::row_col_types$unit) {

  matrix_class <- match.arg(matrix_class)

  s_units_func <- function(unit_val, R_mat, U_mat, V_mat, Y_mat) {
    # Get the products in the R, U, V, and Y matrices
    R_products <- R_mat %>%
      matsbyname::getcolnames_byname() %>%
      lapply(FUN = function(lab) {
        RCLabels::get_pref_suff(lab, which = "pref", notation = product_notation)
      }) %>%
      unlist()
    U_products <- U_mat %>%
      matsbyname::getrownames_byname() %>%
      lapply(FUN = function(lab) {
        RCLabels::get_pref_suff(lab, which = "pref", notation = product_notation)
      }) %>%
      unlist()
    V_products <- V_mat %>%
      matsbyname::getcolnames_byname() %>%
      lapply(FUN = function(lab) {
        RCLabels::get_pref_suff(lab, which = "pref", notation = product_notation)
      }) %>%
      unlist()
    Y_products <- Y_mat %>%
      matsbyname::getrownames_byname() %>%
      lapply(FUN = function(lab) {
        RCLabels::get_pref_suff(lab, which = "pref", notation = product_notation)
      }) %>%
      unlist()

    products_list <- c(R_products, U_products, V_products, Y_products) %>%
      unique()
    if (matrix_class == "matrix") {
      units_vector <- matrix(1, nrow = length(products_list), ncol = 1,
                             dimnames = list(products_list, unit_val)) %>%
        matsbyname::setrowtype(product_type) %>% matsbyname::setcoltype(unit_type)
    } else {
      units_vector <- matsbyname::Matrix(1, nrow = length(products_list), ncol = 1,
                                         dimnames = list(products_list, unit_val)) %>%
        matsbyname::setrowtype(product_type) %>% matsbyname::setcoltype(unit_type)
    }
    list(units_vector) %>%
      magrittr::set_names(c(s_units))
  }

  matsindf::matsindf_apply(.df, FUN = s_units_func, unit_val = unit, R_mat = R, U_mat = U, V_mat = V, Y_mat = Y)
}


#' Calculate U_feed, U_eiou, and r_eiou columns from a U matrix
#'
#' `U_feed`, `U_eiou`, and `r_eiou` matrices are calculated from `U`.
#' All three matrices (`U_feed`, `U_eiou`, and `r_eiou`)
#' have the same structure (row names, column names, row types, and column types)
#' as `U`.
#' For `MWTools`, there is no energy industry own use (EIOU),
#' so `U_feed` is simply a copy of `U`, and `U_eiou` and `r_eiou` are full of `0`s.
#'
#' This function employs `matsindf::matsindf_apply()` internally, so
#' `U` can be either a single matrix or the name of the `U` column in `.df`.
#'
#' @param .df A PSUT data frame containing a column of `U` matrices.
#'            Default is `NULL`, allowing a single matrix for the `U` argument.
#' @param U The name of the incoming `U` matrix. See `MWTools::psut_cols`.
#' @param U_feed,U_eiou,r_eiou Names for outgoing matrices. See `MWTools::psut_cols`.
#'
#' @return `.df` with new columns for `U_feed`, `U_eiou`, and `r_eiou` matrices.
#'
#' @export
#'
#' @examples
#' hmw_df <- hmw_test_data_path() %>%
#'   read.csv() %>%
#'   calc_hmw_pfu() %>%
#'   # Keep only a few years for speed.
#'   dplyr::filter(Year %in% 2000:2002)
#' amw_df <- amw_test_data_path() %>%
#'   read.csv() %>%
#'   calc_amw_pfu() %>%
#'   # Keep only a few years for speed.
#'   dplyr::filter(Year %in% 2000:2002)
#' specify_energy_type_method(hmw_df, amw_df) %>%
#'   specify_product() %>%
#'   specify_TJ() %>%
#'   MWTools::specify_primary_production() %>%
#'   specify_useful_products() %>%
#'   specify_fu_machines() %>%
#'   specify_last_stages() %>%
#'   MWTools::add_row_col_meta() %>%
#'   MWTools::collapse_to_psut() %>%
#'   calc_S_units() %>%
#'   calc_U_feed_U_eiou_r_eiou()
calc_U_feed_U_eiou_r_eiou <- function(.df = NULL,
                                      # Input names
                                      U = MWTools::psut_cols$U,
                                      # Output names
                                      U_feed = MWTools::psut_cols$U_feed,
                                      U_eiou = MWTools::psut_cols$U_eiou,
                                      r_eiou = MWTools::psut_cols$r_eiou) {
  u_func <- function(U_mat) {
    # At this point, U will be a matrix
    U_feed_mat <- U_mat
    U_eiou_mat <- matsbyname::hadamardproduct_byname(U_mat, 0)
    r_eiou_mat <- matsbyname::quotient_byname(U_eiou_mat, U_mat) %>%
      # Some values in U are 0, which gives NaN.
      # So replace all NaN with 0.
      matsbyname::replaceNaN_byname(val = 0)
    list(U_feed_mat, U_eiou_mat, r_eiou_mat) %>%
      magrittr::set_names(c(U_feed, U_eiou, r_eiou))
  }
  matsindf::matsindf_apply(.df, FUN = u_func, U_mat = U)
}


#' A convenience function to create PSUT matrices in a data frame
#'
#' Starting from human and animal muscle work data frames,
#' this function bundles several functions to create a
#' data frame of PSUT matrices.
#' The bundled functions are:
#'   - `specify_energy_type_method()`,
#'   - `specify_product()`,
#'   - `specify_ktoe()` or `specify_TJ()`, depending on the value of `output_unit`,
#'   - `MWTools::specify_primary_production()`,
#'   - `specify_useful_products()`,
#'   - `specify_fu_machines()`,
#'   - `specify_last_stages()`,
#'   - `MWTools::add_row_col_meta()`,
#'   - `MWTools::collapse_to_psut()`,
#'   - `calc_S_units()`, and
#'   - `calc_U_feed_U_eiou_r_eiou()`.
#'
#' Default values are assumed for function arguments.
#'
#' The "Unit" column is deleted after the "S_units" column is created.
#'
#' @param .hmw_df A data frame produced by `calc_hmw_pfu()`.
#' @param .amw_df A data frame produced by `calc_amw_pfu()`.
#' @param matrix_class The type of matrix to be created, one of "matrix" or "Matrix".
#'                     Default is "matrix".
#' @param output_unit A string of length one that specifies the output unit.
#'                    One of "TJ" or "ktoe" for terajoules or kilotons of oil equivalent.
#' @param unit,R,U,V,Y,s_units,U_feed,U_eiou,r_eiou Column names. See `IEATools::psut_cols`.
#'
#' @return A data frame of musle work PSUT matrices.
#'
#' @export
#'
#' @examples
#' hmw_df <- hmw_test_data_path() %>%
#'   read.csv() %>%
#'   calc_hmw_pfu() %>%
#'   # Keep only a few years for speed.
#'   dplyr::filter(Year %in% 2000:2002)
#' amw_df <- amw_test_data_path() %>%
#'   read.csv() %>%
#'   calc_amw_pfu() %>%
#'   # Keep only a few years for speed.
#'   dplyr::filter(Year %in% 2000:2002)
#' prep_psut(hmw_df, amw_df)
prep_psut <- function(.hmw_df, .amw_df,
                      matrix_class = c("matrix", "Matrix"),
                      output_unit = c("TJ", "ktoe"),
                      unit = IEATools::iea_cols$unit,
                      R = IEATools::psut_cols$R,
                      U = IEATools::psut_cols$U,
                      V = IEATools::psut_cols$V,
                      Y = IEATools::psut_cols$Y,
                      s_units = IEATools::psut_cols$s_units,
                      U_feed = IEATools::psut_cols$U_feed,
                      U_eiou = IEATools::psut_cols$U_eiou,
                      r_eiou = IEATools::psut_cols$r_eiou) {

  matrix_class <- match.arg(matrix_class)
  output_unit <- match.arg(output_unit)

  # Calculate a preliminary outbound data frame.
  out <- specify_energy_type_method(.hmw_df, .amw_df) %>%
    specify_product()
  if (output_unit == "TJ") {
    out <- out |>
      specify_TJ()
  } else if (output_unit == "ktoe") {
    out <- out |>
      specify_ktoe()
  }
  out <- out |>
    MWTools::specify_primary_production() %>%
    specify_useful_products() %>%
    specify_fu_machines() %>%
    specify_last_stages() %>%
    MWTools::add_row_col_meta() %>%
    MWTools::collapse_to_psut(matrix_class = matrix_class)
  # If out has no rows, it probably means that the incoming data frames
  # had no rows.
  # Trap that condition here (where correct metadata columns are present) and
  # return a data frame with the correct columns of empty data.
  if (nrow(out) == 0) {
    # Get names of additional outgoing columns
    more_cnames <- c(R, U, V, Y, s_units, U_feed, U_eiou, r_eiou)
    # Make a zero-row data frame with these columns,
    # all of which are list columns to match the standard output of this function.
    mat_cols <- tibble::tibble(c1 = list(),
                               c2 = list(),
                               c3 = list(),
                               c4 = list(),
                               c5 = list(),
                               c6 = list(),
                               c7 = list(),
                               c8 = list())
    colnames(mat_cols) <- more_cnames
    no_rows <- out %>%
      dplyr::mutate(
        # The incoming unit column should be deleted.
        "{unit}" := NULL,
      ) %>%
      # Add R, U, V, Y, S_units, U_feed, U_EIOU, and r_EIOU columns.
      dplyr::bind_cols(mat_cols)
    return(no_rows)
  }

  # If we get here, we have a non-zero-rows outgoing data frame.
  # Continue with the calculations.
  out %>%
    calc_S_units(matrix_class = matrix_class) %>%
    # Eliminate the Unit column.
    dplyr::mutate(
      "{MWTools::mw_cols$unit}" := NULL
    ) %>%
    calc_U_feed_U_eiou_r_eiou()
}
