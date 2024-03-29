% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/psut.R
\name{calc_S_units}
\alias{calc_S_units}
\title{Calculate an S_units vector given other PSUT matrices}
\usage{
calc_S_units(
  .df = NULL,
  matrix_class = c("matrix", "Matrix"),
  unit = MWTools::mw_cols$unit,
  R = MWTools::psut_cols$R,
  U = MWTools::psut_cols$U,
  V = MWTools::psut_cols$V,
  Y = MWTools::psut_cols$Y,
  s_units = MWTools::psut_cols$s_units,
  product_notation = RCLabels::from_notation,
  product_type = MWTools::row_col_types$product,
  unit_type = MWTools::row_col_types$unit
)
}
\arguments{
\item{.df}{A data frame. Default is \code{NULL}.}

\item{matrix_class}{The type of matrix to be created, one of "matrix" or "Matrix".
Default is "matrix".}

\item{unit}{A string unit for each row or the name of the unit column in \code{.df}. See \code{MWTools::mw_cols}.}

\item{R, U, V, Y}{PSUT matrices or the names of matrix columns in \code{.df}. See \code{MWTools::psut_cols}.}

\item{s_units}{The name of the output matrix or the output column. See \code{MWTools::psut_cols}.}

\item{product_notation}{Notation for products. Default is \code{RCLabels::from_notation}.}

\item{product_type, unit_type}{The types for products and units columns. See \code{MWTools::row_col_types$unit}.}
}
\value{
\code{.df} with an \code{s_units} column added.
}
\description{
The \code{S_units} vector is a unit summation vector
with products (energy carriers) in rows and units in columns.
The row names for each vector are the various products in that row of \code{.df}.
The column names for each vector are taken from the \code{units} column of \code{.df}.
}
\details{
This function employs \code{matsindf::matsindf_apply()} internally, so
\code{unit} can be a single string or the name of a column in \code{.df}.
Similarly, \code{R}, \code{U}, \code{V}, and \code{Y} can be either matrices or the names of columns in \code{.df}.

Product names are taken from the prefixes of row or columns names
of the \code{R}, \code{U}, \code{V}, and \code{Y} matrices.

The \code{unit} column will remain in \code{.df} on output and will need to be deleted afterward.
}
\examples{
ilo_working_hours_data <- read.csv(file = MWTools::ilo_working_hours_test_data_path())
ilo_employment_data <- read.csv(file = MWTools::ilo_employment_test_data_path())
hmw_data <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
                              ilo_employment_data = ilo_employment_data)
hmw_df <- hmw_data \%>\%
  calc_hmw_pfu() \%>\%
  # Keep only a few years for speed.
  dplyr::filter(Year \%in\% 2000:2002)
amw_df <- amw_test_data_path() \%>\%
  read.csv() \%>\%
  calc_amw_pfu() \%>\%
  # Keep only a few years for speed.
  dplyr::filter(Year \%in\% 2000:2002)
specify_energy_type_method(hmw_df, amw_df) \%>\%
  specify_product() \%>\%
  specify_TJ() \%>\%
  MWTools::specify_primary_production() \%>\%
  specify_useful_products() \%>\%
  specify_fu_machines() \%>\%
  specify_last_stages() \%>\%
  MWTools::add_row_col_meta() \%>\%
  MWTools::collapse_to_psut() \%>\%
  calc_S_units()
}
