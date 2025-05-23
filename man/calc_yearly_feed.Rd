% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_amw_functions.R
\name{calc_yearly_feed}
\alias{calc_yearly_feed}
\title{Calculate the yearly feed requirements of working animals by species}
\usage{
calc_yearly_feed(
  .df,
  amw_analysis_data_path = MWTools::amw_analysis_data_path(),
  concordance_species = MWTools::conc_cols$species,
  method_source = MWTools::mw_constants$method_source,
  wa_feed_sheet = MWTools::amw_analysis_constants$wa_feed_sheet,
  wa_days_hours_sheet = MWTools::amw_analysis_constants$wa_days_hours_sheet,
  working_days_col = MWTools::amw_analysis_constants$working_days_col,
  nonworking_days_col = MWTools::amw_analysis_constants$nonworking_days_col,
  working_hours_col = MWTools::amw_analysis_constants$working_hours_col,
  working_day_feed_col = MWTools::amw_analysis_constants$working_day_feed_col,
  nonworking_day_feed_col = MWTools::amw_analysis_constants$nonworking_day_feed_col,
  working_yearly_feed_col = MWTools::amw_analysis_constants$working_yearly_feed_col,
  nonwkg_yearly_feed_col = MWTools::amw_analysis_constants$nonwkg_yearly_feed_col,
  total_yearly_feed_col = MWTools::amw_analysis_constants$total_yearly_feed_col,
  amw_region_code_col = MWTools::conc_cols$amw_region_code_col
)
}
\arguments{
\item{.df}{A data frame containing tidied data for the number of live and
working animals. Usually produced by calling the
\code{tidy_fao_live_animals},
\code{add_concordance_codes},
\code{trim_fao_data},
\code{get_working_species},
\code{calc_working_animals}, and
\code{calc_sector_split} functions in sequence on the raw FAO data.}

\item{amw_analysis_data_path}{The path to the animal muscle work analysis data,
containing feed requirements of working animals
in agriculture and transport, by species, country, and
over time. Set to the function \code{MWTools::amw_analysis_data_path()},
by default, which returns the path the analysis data
bundled with the \code{MWTools} package.}

\item{concordance_species}{See \code{MWTools::conc_cols}.}

\item{method_source}{See \code{MWTools::mw_constants}.}

\item{wa_feed_sheet, wa_days_hours_sheet, working_days_col, nonworking_days_col, working_hours_col, working_day_feed_col, nonworking_day_feed_col, working_yearly_feed_col, nonwkg_yearly_feed_col, total_yearly_feed_col}{See \code{MWTools::amw_analysis_constants}.}

\item{amw_region_code_col}{See \code{MWTools::conc_cols}.}
}
\description{
Calculate the yearly feed requirements of working animals by species and region.
This function uses user-supplied data supplied via \code{amw_analysis_data_path} for daily
feed requirements of working animals on working and non-working days by species
and region, and user-supplied data for the number of working days by species
and region to estimate the total yearly feed requirements.
}
\examples{
working_animals_data <- read.csv(file = MWTools::amw_test_data_path()) \%>\%
  tidy_fao_live_animals() \%>\%
  add_concordance_codes() \%>\%
  trim_fao_data() \%>\%
  get_working_species() \%>\%
  calc_working_animals() \%>\%
  calc_sector_split() \%>\%
  calc_yearly_feed()
}
