#' General MWTools contants
#'
#' A string list containing contants used in the MWTools package functions
#'
#' @format A string list with `r length(mw_constants)` entries.
#' \describe{
#' \item{country_name}{The name of a metadata column containing full length country names.}
#' \item{sex}{The name of a metadata column representing gender.}
#' \item{sector}{The name of a metadata column containing representing the sector, or sector category.}
#' \item{year}{The name of a metadata column containing values for the year of the observation.}
#' \item{working_hours}{The name of the data column containing data for the number of working hours.}
#' \item{employed_persons}{The name of the data column containing data for the number of employed persons.}
#' \item{species}{The name of the metadata column representing a species of live animal from FAO data.}
#' \item{unit}{The name of the metadata column containing the units associated with the `value` column}
#' \item{value}{The name of the data column containing the values of a given observation.}
#' }
#'
#' @examples
#' mw_constants
"mw_constants"
