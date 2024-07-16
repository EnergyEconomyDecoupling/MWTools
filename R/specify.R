

#' Add energy type and method columns to tidy muscle work data frames
#'
#' To specify muscle work data in preparation for
#' conversion to PSUT matrices,
#' the first step is specifying that these data are energy (not exergy) data
#' and indicating the method for estimating
#' the primary energy ("Biomass \[from Resources\]")
#' associated with
#' the initial stage of final energy ("Biomass").
#' We assume the Physical Content Method (PCM) in which
#' the primary energy ("Biomass \[from Resources\]")
#' is equal in magnitude to
#' the final energy ("Biomass").
#'
#' `.hmw_df` and `.amw_df` bound by rows.
#' The resulting data frame is modified,
#' replacing any `NA` values with `0` in the `e_dot` column.
#'
#' @param .hmw_df A data frame produced by `calc_hmw_pfu()`.
#' @param .amw_df A data frame produced by `calc_amw_pfu()`.
#' @param e_dot,energy_type,method,e_type See `MWTools::mw_cols`.
#' @param pcm See `MWTools::methods`.
#'
#' @return A data frame in which `energy_type` and `method` columns are included.
#'
#' @export
#'
#' @examples
#' ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
#' ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())
#' hmw_data <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
#'                               ilo_employment_data = ilo_employment_data)
#' hmw_df <- hmw_data %>%
#'   calc_hmw_pfu()
#' amw_df <- amw_test_data_path() %>%
#'   read.csv() %>%
#'   calc_amw_pfu()
#' specify_energy_type_method(hmw_df, amw_df)
specify_energy_type_method <- function(.hmw_df, .amw_df,
                                       e_dot = MWTools::mw_cols$e_dot,
                                       energy_type = MWTools::mw_cols$energy_type,
                                       method = MWTools::mw_cols$method,
                                       e_type = MWTools::energy_types$e,
                                       pcm = MWTools::methods$pcm) {
  dplyr::bind_rows(.hmw_df, .amw_df) %>%
    # Replace all NA values with 0.
    dplyr::mutate(
      "{e_dot}" := dplyr::case_when(
        is.na(.data[[e_dot]]) ~ 0,
        TRUE ~ .data[[e_dot]]
      )
    ) %>%
    # Add energy type and method.
    dplyr::mutate(
      "{energy_type}" := e_type,
      "{method}" := pcm
    )
}


#' Add a product column to a muscle work data frame
#'
#' A `product` column is needed before converting a muscle work data frame
#' to PSUT matrices. This function adds and populates the `product` column.
#'
#' @param .df A data frame, likely produced by `specify_energy_type_method()`.
#' @param product The name of the column to be added. See `MWTools::mw_constants`.
#' @param primary,final,useful See `MWTools::all_stages`.
#' @param concordance_species See `MWTools::conc_cols`.
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
#' ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
#' ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())
#' hmw_data <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
#'                               ilo_employment_data = ilo_employment_data)
#' hmw_df <- hmw_data %>%
#'   calc_hmw_pfu() %>%
#'   specify_product()
#' amw_df <- amw_test_data_path() %>%
#'   read.csv() %>%
#'   calc_amw_pfu()
#' specify_energy_type_method(hmw_df, amw_df) %>%
#'   specify_product()
specify_product <- function(.df,
                            product = MWTools::mw_cols$product,
                            primary = MWTools::all_stages$primary,
                            final = MWTools::all_stages$final,
                            useful = MWTools::all_stages$useful,
                            concordance_species = MWTools::conc_cols$species,
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

  .df %>%
    # Add a Product column
    dplyr::mutate(
      "{product}" := dplyr::case_when(
        .data[[stage]] == primary ~ biomass,
        .data[[stage]] == final & startsWith(.data[[concordance_species]], human) ~ food,
        .data[[stage]] == final ~ feed,
        .data[[stage]] == useful & startsWith(.data[[concordance_species]], human) ~ hu_mech,
        .data[[stage]] == useful & .data[[sector]] == transport ~ an_p,
        .data[[stage]] == useful ~ an_mech,
        TRUE ~ NA_character_
      )
    )
}


#' Add primary production to a data frame of PFU muscle work data
#'
#' Adds rows for biomass from resources.
#'
#' If no primary rows are found
#' (probably because `.df` has no rows),
#' `.df` is returned unmodified.
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
#' ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
#' ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())
#' hmw_data <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
#'                               ilo_employment_data = ilo_employment_data)
#' hmw_df <- hmw_data %>%
#'   calc_hmw_pfu()
#' amw_df <- amw_test_data_path() %>%
#'   read.csv() %>%
#'   calc_amw_pfu()
#' specify_energy_type_method(hmw_df, amw_df) %>%
#'   specify_product() %>%
#'   MWTools::specify_primary_production()
specify_primary_production <- function(.df,
                                       product = MWTools::mw_cols$product,
                                       primary = MWTools::all_stages$primary,
                                       stage = MWTools::mw_constants$stage_col,
                                       notation = RCLabels::from_notation,
                                       resources = MWTools::mw_sectors$resources_sector) {

  # Find all primary rows
  primary_rows <- .df %>%
    dplyr::filter(.data[[stage]] == primary)
  # If we have no primary rows, avoid an error by simply returning the incoming .df
  if (nrow(primary_rows) == 0) {
    return(.df)
  }
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
#' @param product,stage See `MWTools::mw_constants`.
#' @param concordance_species See `MWTools::conc_cols`.
#' @param useful See `MWTools::all_stages`.
#' @param notation See `RCLabels::from_notation`.
#'
#' @return A data frame with
#'
#' @export
#'
#' @examples
#' ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
#' ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())
#' hmw_data <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
#'                               ilo_employment_data = ilo_employment_data)
#' hmw_df <- hmw_data %>%
#'   calc_hmw_pfu()
#' amw_df <- amw_test_data_path() %>%
#'   read.csv() %>%
#'   calc_amw_pfu()
#' specify_energy_type_method(hmw_df, amw_df) %>%
#'   specify_product() %>%
#'   MWTools::specify_primary_production() %>%
#'   specify_useful_products()
specify_useful_products <- function(.df,
                                    product = MWTools::mw_cols$product,
                                    stage = MWTools::mw_constants$stage_col,
                                    concordance_species = MWTools::conc_cols$species,
                                    useful = MWTools::all_stages$useful,
                                    notation = RCLabels::from_notation) {
  # Find all useful rows
  useful_rows <- .df %>%
    dplyr::filter(.data[[stage]] == useful)
  # Specify the products in the useful rows
  specified_useful_rows <- useful_rows %>%
    dplyr::mutate(
      "{product}" := RCLabels::paste_pref_suff(pref = .data[[product]],
                                               # Get the prefix of the species, in case
                                               # specify_fu_machines() has already been called.
                                               suff = RCLabels::get_pref_suff(.data[[concordance_species]],
                                                                              which = "pref",
                                                                              notation = notation),
                                               notation = notation),
      "{product}" := as.character(.data[[product]])
    )
  .df %>%
    # Eliminate useful energy rows
    dplyr::filter(.data[[stage]] != useful) %>%
    # Bind the specified useful rows to the bottom of the outgoing data frame.
    dplyr::bind_rows(specified_useful_rows)
}


#' Specify final-to-useful industries
#'
#' Final-to-useful industries should be named as "Industry -> Product".
#' This function renames final-to-useful industries (in this case, species) to the desired form.
#'
#' @param .df A data frame, likely the output of `specify_useful_products()`.
#' @param product,stage See `MWTools::mw_constants`.
#' @param concordance_species See `MWTools::conc_cols`.
#' @param final,useful See `MWTools::all_stages`.
#' @param product_notation The notation for products. Default is `RCLabels::from_notation`.
#' @param machine_notation The notation for machines. Default is `RCLabels::arrow_notation`.
#'
#' @return A data frame in which final-to-useful machines are specified.
#'
#' @export
#'
#' @examples
#' ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
#' ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())
#' hmw_data <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
#'                               ilo_employment_data = ilo_employment_data)
#' hmw_df <- hmw_data %>%
#'   calc_hmw_pfu()
#' amw_df <- amw_test_data_path() %>%
#'   read.csv() %>%
#'   calc_amw_pfu()
#' specify_energy_type_method(hmw_df, amw_df) %>%
#'   specify_product() %>%
#'   MWTools::specify_primary_production() %>%
#'   specify_useful_products() %>%
#'   specify_fu_machines()
specify_fu_machines <- function(.df,
                                product = MWTools::mw_cols$product,
                                stage = MWTools::mw_constants$stage_col,
                                concordance_species = MWTools::conc_cols$species,
                                final = MWTools::all_stages$final,
                                useful = MWTools::all_stages$useful,
                                product_notation = RCLabels::from_notation,
                                machine_notation = RCLabels::arrow_notation) {
  fu_machine_rows <- .df %>%
    dplyr::filter(.data[[stage]] == useful)
  # Specify the fu machines (in this case, concordance_species)
  specified_fu_machines <- fu_machine_rows %>%
    dplyr::mutate(
      "{concordance_species}" := RCLabels::paste_pref_suff(pref = .data[[concordance_species]],
                                               # Get the prefix of the product, in case
                                               # specify_useful_products() has already been called.
                                               suff = RCLabels::get_pref_suff(.data[[product]],
                                                                              which = "pref",
                                                                              notation = product_notation),
                                               notation = machine_notation),
      "{concordance_species}" := as.character(.data[[concordance_species]])
    )
  .df %>%
    # Eliminate useful energy rows
    dplyr::filter(.data[[stage]] != useful) %>%
    # Bind the specified final-to-useful machines at the bottom of the outgoing data frame.
    dplyr::bind_rows(specified_fu_machines)
}


#' Create energy conversion chains for final and useful last stages
#'
#' We have two ways to describe the energy conversion chain:
#' with last stage of "Final" and last stage of "Useful".
#' This function takes `.df` (assumed to be `last_stage = "Useful"`)
#' and converts to `last_stage = "Final"`
#' by adding a last_stage column with appropriate values.
#'
#' @param .df A data frame, probably the output of `specify_fu_machines()`.
#' @param stage See `MWTools::mw_constants`.
#' @param last_stage See `MWTools::mw_cols`.
#' @param final,useful See `MWTools::last_stages`.
#'
#' @return A data frame containing a new column `last_stage` and
#'         rows for both final and useful being the last stage.
#'
#' @export
#'
#' @examples
#' ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
#' ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())
#' hmw_data <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
#'                               ilo_employment_data = ilo_employment_data)
#' hmw_df <- hmw_data %>%
#'   calc_hmw_pfu()
#' amw_df <- amw_test_data_path() %>%
#'   read.csv() %>%
#'   calc_amw_pfu()
#' specify_energy_type_method(hmw_df, amw_df) %>%
#'   specify_product() %>%
#'   MWTools::specify_primary_production() %>%
#'   specify_useful_products() %>%
#'   specify_fu_machines() %>%
#'   specify_last_stages()
specify_last_stages <- function(.df,
                                stage = MWTools::mw_constants$stage_col,
                                last_stage = MWTools::mw_cols$last_stage,
                                final = MWTools::last_stages$final,
                                useful = MWTools::last_stages$useful) {
  # .df comes in with last_stage of useful,
  # so just name it.
  ls_useful_rows <- .df %>%
    dplyr::mutate(
      "{last_stage}" := useful
    )

  # Now work on situation where last stage is final.
  # We need to construct this ECC using information available in .df.

  ls_final_rows <- .df %>%
    # Eliminate useful rows
    dplyr::filter(.data[[stage]] != useful) %>%
    # And set last_stage
    dplyr::mutate(
      "{last_stage}" := final
    )
  # Bind and return
  dplyr::bind_rows(ls_final_rows, ls_useful_rows)
}


#' Converts units from EJ to ktoe
#'
#' Primary-final-useful (PFU) data from `calc_hmw_pfu()` and `calc_amw_pfu()`
#' are in EJ.
#' This function converts entries in the `energy_col` from EJ (exajoules) to ktoe and
#' changes the `units_col` from "EJ" to "ktoe".
#'
#' Prior to converting the `energy_col` from EJ to ktoe,
#' the `units_col` is verified to contain only "EJ".
#' An error is thrown if any `units_col` entry is not in EJ.
#'
#' @param .df A data frame with `units_col` and `energy_col` columns.
#' @param energy_col,units_col See `MWTools::mw_constants`.
#'
#' @return `.df` with `energy` column converted from EJ to ktoe.
#'
#' @export
#'
#' @examples
#' ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
#' ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())
#' hmw_data <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
#'                               ilo_employment_data = ilo_employment_data)
#' hmw_data %>%
#'   calc_hmw_pfu() %>%
#'   specify_ktoe()
specify_ktoe <- function(.df,
                         energy_col = MWTools::mw_cols$e_dot,
                         units_col = MWTools::mw_cols$unit) {
  # Verify that .df has units of EJ
  assertthat::assert_that(all(.df[[units_col]] == "EJ"), msg = "units_col not in EJ in MWTools::specify_ktoe().")
  # Do the conversion
  .df %>%
    dplyr::mutate(
      "{energy_col}" := .data[[energy_col]] * MWTools::unit_constants$EJ_to_ktoe,
      "{units_col}" := "ktoe"
    )
}


#' Converts units from EJ to TJ
#'
#' Primary-final-useful (PFU) data from `calc_hmw_pfu()` and `calc_amw_pfu()`
#' are in EJ.
#' This function converts entries in the `energy_col` from EJ (exajoules) to TJ (terajoules) and
#' changes the `units_col` from "EJ" to "TJ".
#'
#' Prior to converting the `energy_col` from EJ to TJ,
#' the `units_col` is verified to contain only "EJ".
#' An error is thrown if any `units_col` entry is not in EJ.
#'
#' @param .df A data frame with `units_col` and `energy_col` columns.
#' @param energy_col,units_col See `MWTools::mw_constants`.
#'
#' @return `.df` with `energy` column converted from EJ to TJ.
#'
#' @export
#'
#' @examples
#' ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
#' ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())
#' hmw_data <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
#'                               ilo_employment_data = ilo_employment_data)
#' hmw_data %>%
#'   calc_hmw_pfu() %>%
#'   specify_TJ()
specify_TJ <- function(.df,
                       energy_col = MWTools::mw_cols$e_dot,
                       units_col = MWTools::mw_cols$unit) {
  # Verify that .df has units of EJ
  assertthat::assert_that(all(.df[[units_col]] == "EJ"), msg = "units_col not in EJ in MWTools::specify_TJ().")
  # Do the conversion
  .df %>%
    dplyr::mutate(
      "{energy_col}" := .data[[energy_col]] * MWTools::unit_constants$EJ_to_TJ,
      "{units_col}" := "TJ"
    )
}
