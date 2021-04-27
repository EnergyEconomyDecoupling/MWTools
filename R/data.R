#' General MWTools constants
#'
#' A string list containing constants used in the MWTools package functions
#'
#' @format A string list with `r length(mw_constants)` entries.
#' \describe{
#' \item{country_name}{The name of a metadata column containing full length country names.}
#' \item{year}{The name of a metadata column containing values for the year of the observation.}
#' \item{species}{The name of the metadata column representing a species of live animal from FAO data.}
#' \item{unit}{The name of the metadata column containing the units associated with the `value` column}
#' \item{value}{The name of the data column containing the values of a given observation.}
#' }
#'
#' @examples
#' mw_constants
"mw_constants"

#' ILO data column names
#'
#' A string list containing a selection of ILO data column names. For data retrieved by the package `Rilostat`.
#'
#' @format A string list with `r length(ilo_cols)` entries.
#' \describe{
#' \item{sex_ilo_col}{The string "Sex".}
#' \item{sector_ilo_col}{The string "Sector".}
#' \item{working_hours_ilo_col}{The string "Working.hours".}
#' \item{employed_persons_ilo_col}{The string "Employed.persons".}
#' }
#'
#' @examples
#' ilo_cols
"ilo_cols"

#' Working animal species
#'
#' A string list containing the species of animals considered to perform muscle work
#'
#' @format A string list with `r length(mw_species)` entries.
#' \describe{
#' \item{asses}{The string "Asses".}
#' \item{camels}{The string "Camels".}
#' \item{cattle}{The string "Cattle".}
#' \item{horses}{The string "Horses".}
#' \item{mules}{The string "Mules".}
#' \item{buffaloes}{The string "Buffaloes".}
#' \item{camelids_other}{The string "Camelids, other".}
#' \item{camelids}{The string "Camelids", representing the combined category of "Camels" and "Camelids, other".}
#' \item{humans}{The string "Humans".}
#' }
#'
#' @examples
#' mw_species
"mw_species"

#' FAO data column names
#'
#' A string list containing a selection of FAO data column names. For data retrieved by the package `FAOSTAT`.
#'
#' @format A string list with `r length(fao_cols)` entries.
#' \describe{
#' \item{area_fao_col}{The string "area".}
#' \item{item_fao_col}{The string "item".}
#' \item{year_fao_col}{The string "year".}
#' \item{unit_fao_col}{The string "unit".}
#' \item{value_fao_col}{The string "value".}
#' }
#'
#' @examples
#' fao_cols
"fao_cols"

#' FAO data item code names
#'
#' A string list containing a selection of FAO data code names. See `FAOSTAT::FAOsearch()`
#' to view the associated items, codes, and other metadata.
#'
#' @format A string list with `r length(fao_codes)` entries.
#' \describe{
#' \item{live_animals_code}{The string "QA" representing data for the number of live animals.}
#' }
#'
#' @examples
#' fao_codes
"fao_codes"

#' ILO data item code names
#'
#' A string list containing a selection of ILO data code names. See `Rilostat::get_ilostat_toc()`
#' to view the associated items, codes, and other metadata..
#'
#' @format A string list with `r length(ilo_codes)` entries.
#' \describe{
#' \item{working_hours_code}{The string "HOW_TEMP_SEX_ECO_NB_A", representing Mean weekly hours actually worked per employed person by sex and economic activity.}
#' \item{employment_code}{The string "EMP_TEMP_SEX_ECO_NB_A", representing Employment by sex and economic activity (thousands).}
#' }
#'
#' @examples
#' ilo_codes
"ilo_codes"


