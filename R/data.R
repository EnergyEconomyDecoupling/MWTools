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
#' \item{sector_col}{The name of the metadata column containing the sector associated with the `value`.}
#' \item{stage_col}{The name of the metadata column containing the stage of the energy conversion chain associated with the `value`. Usally one of "Primary", "Final", or "Useful".}
#' \item{energy_mj_year}{The name of the data column containing muscle work energyg values in units of megajoules (MJ) per year.}
#' \item{exemplar_method_col}{The metadata column "Exemplar/Method" representing the regional exemplar or method used for a particular data series.}
#' \item{method_source_col}{The metadata column "Method/Source" representing the method or source of information use for a particular data series.}
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
#' \item{ref_area_col}{The string "ref_area".}
#' \item{sex_ilo_col}{The string "Sex".}
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

#' Human muscle work (hmw) analysis data constants
#'
#' A string list containing a selection of hmw analysis constants.
#'
#' @format A string list with `r length(hmw_analysis_constants)` entries.
#' \describe{
#' \item{total_working_hours_ilo_col}{The data column "Total.hours [hours/year]" containing the number of hours worked yearly by all humans for a given country, sex, sector, and year.}
#' \item{industry_activity_col}{The data column "Industry/Activity" containing information on the industry or activity of human workers. Usually one of "Sedentary", "Primary", or "Secondary".}
#' \item{hmw_power_sheet}{The excel sheet name "hmw_power", which contains information on the power output of humans performing muscle work.}
#' \item{hmw_food_sheet}{The excel sheet name "hmw_food", which contains information on the food consumption of humans.}
#' \item{hmw_sector_map_sheet}{The excel sheet name "hmw_sector_map", which contains information on the mapping the sector names used in the hmw analysis data excel file bundled with this package with the sector names in the ILO data.}
#' \item{hmw_plate_waste_sheet}{The excel sheet name "hmw_plate_waste", which contains information on proportion of food waste by region.}
#' \item{hmw_harvest_waste_sheet}{The excel sheet name "hmw_harvest_waste", which contains information on the proportion of phytomass lost at the harvest stage by region.}
#' \item{broad_sector_col}{The column name "broad_sector", which contains the "Broad sector" sector names used by the ILO.}
#' \item{hmw_analysis_sector_col}{The column name "Sector.hmw", which contains the sector names used in the hmw analysis data excel file bundled with this package.}
#' \item{agriculture_broad.sector}{The string "Agriculture" representing the agriculture sector. One of the ILO's "Broad sectors".}
#' \item{industry_broad.sector}{The string "Industry" representing the industry sector. One of the ILO's "Broad sectors".}
#' \item{services_broad.sector}{The string "Services" representing the services sector. One of the ILO's "Broad sectors".}
#' \item{not_classified_broad.sector}{The string "Not classfied" representing unclassified labor data. One of the ILO's "Broad sectors".}
#' \item{non_agriculture_broad.sector}{The string "Non-agriculture" representing all sectors outside of agriculture. One of the ILO's "Broad sectors".}
#' \item{total_sector}{The string "Total" representing the entire economy. One of the ILO's "Broad sectors".}
#' \item{food_consumption_col}{The data column "Food consumption [kcal/day per person]", which contains information on the daily food consumption of humans in kilocalories (kcal).}
#' \item{yearly_energy_consumption_pp_col}{The data column "Energy consumption [MJ/year per person]", which contains information on the yearly food consumption per working person in megajoules (MJ).}
#' \item{final_energy_col}{The data column "Final energy [MJ/year]", which contains data on the yearly food consumption of all working persons in megajoules (MJ).}
#' \item{primary_energy_col}{The data column "Primary energy [MJ/year]", which contains data on the yearly quantity of biomass required to meet the food requirements of all working persons in megajoules (MJ).}
#' \item{useful_energy_hmw_col}{The data column "Useful energy [MJ/year]", which contains data on the yearly useful work performed by all working persons in megajoules (MJ).}
#' \item{power_col}{The data column "Power [W]", which contains data on the power output of humans performing muscle work.}
#' \item{plate_waste_col}{The data column "Plate waste [-]", which contains data on the proportion of food waste by region.}
#' \item{hmw_harvest_waste_col}{The data column "arvest waste [-]", which contains data on the proportion of phytomass lost at the harvest stage by region.}
#' }
#'
#' @examples
#' hmw_analysis_constants
"hmw_analysis_constants"

#' Animal muscle work (amw) analysis data constants
#'
#' A string list containing a selection of amw analysis constants, used in the "amw_analysis_data.R" excel file.
#'
#' @format A string list with `r length(amw_analysis_constants)` entries.
#' \describe{
#' \item{prop_working_animals_col}{The metadata column "Prop.Working.animals" representing the proportion of live animals that are working animals.}
#' \item{prop_working_animals_ag_col}{The metadata column "Prop.Working.animals.Ag" representing the proportion of working animals that work in agriculture.}
#' \item{prop_working_animals_tr_col}{The metadata column "Prop.Working.animals.Tr" representing the proportion of working animals that work transporting goods outside of agriculture.}
#' \item{wa_perc_sheet}{The excel sheet name "WA_perc" representing the sheet containing information on the proportion of working animals.}
#' \item{wa_enduse_sheet}{The excel sheet name "WA_enduse" representing the sheet containing information of the proportion of working animals in agriculture/transport.}
#' \item{wa_feed_sheet}{The excel sheet name "WA_feed" representing the sheet containing information on the feed requirements of working animals.}
#' \item{wa_days_hours_sheet}{The excel sheet name "WA_days_hours" representing the sheet containing information on the number of days and hours worked by working animals.}
#' \item{wa_power_sheet}{The excel sheet name "WA_power" representing the sheet containing information on the power output of working animals.}
#' \item{working_seconds_col}{The data column "Working.seconds [seconds per animal]" representing the number of seconds of work performed by working animals.}
#' \item{working_hours_col}{The data column "Working.hours [hour per animal]" representing the number of hours of work performed by working animals.}
#' \item{working_days_col}{The data column "Working.days [day per animal]" representing the number of days that working animals worked.}
#' \item{nonworking_days_col}{The data column "Non-Working days [day per animal]" representing the number of days that working animals did not work.}
#' \item{power_per_animal}{The data column "Power.per.animal [W]" representing the power output of working animals.}
#' \item{live_animals_col}{The data column "Live.animals" representing the number of live animals.}
#' \item{working_animals_col}{The data column "Working.animals" representing the number of working animals.}
#' \item{working_animals_total_col}{The data column "Working.animals.total" representing the total number of working animals.}
#' \item{working_animals_ag_col}{The data column "Working.animals.Ag" representing the number of working animals that work in agriculture.}
#' \item{working_animals_tr_col}{The data column "Working.animals.Tr" representing the number of working animals that work transporting goods outside of agriculture.}
#' \item{working_day_feed_col}{The data column "Working.day.feed [MJ/day per animal]" representing the quantity of feed required by working animals each working day.}
#' \item{nonworking_day_feed_col}{The data column "Non-Working.day.feed [MJ/day per animal]" representing the quantity of feed required by working animals each non-working day. }
#' \item{working_yearly_feed_col}{The data column "Working.yearly.feed [MJ/year per animal]" representing the quantity of feed required by working animals on working days each year.}
#' \item{nonworking_yearly_feed_col}{The data column "Non-Working.yearly.feed [MJ/year per animal]" representing the quantity of feed required by working animals on non-working days each year.}
#' \item{total_yearly_feed_col}{The data column "Total.yearly.feed [MJ/year per animal]" representing the quantity of feed required by working animals each year.}
#' \item{useful_energy_total}{The data column "Useful.energy.total [MJ/year]" representing the total useful energy produced by working animals through muscle work.}
#' \item{useful_energy_ag}{The data column "Useful.energy.Ag [MJ/year]" representing the useful energy produced by working animals through muscle work in agriculture.}
#' \item{useful_energy_tr}{The data column "Useful.energy.Tr [MJ/year]" representing the useful energy produced by working animals through muscle work transporting goods outside of agriculture.}
#' \item{final_energy_total}{The data column "Final.energy.total [MJ/year]" representing the total final energy (feed) required by working animals.}
#' \item{final_energy_ag}{The data column "Final.energy.Ag [MJ/year]" representing the final energy (feed) required by working animals in agriculture.}
#' \item{final_energy_tr}{The data column "Final.energy.Tr [MJ/year]" representing the final energy (feed) required by working animals working to transport goods outside of agriculture.}
#' \item{primary_energy_total}{The data column "Primary.energy.total [MJ/year]" representing the primary energy (total feed crop biomass) embodied in the total final energy required by working animals.}
#' \item{primary_energy_ag}{The data column "Primary.energy.Ag [MJ/year]" representing the primary energy (total feed crop biomass) embodied in the final energy required by working animals in agriculture.}
#' \item{primary_energy_tr}{The data column "Primary.energy.Tr [MJ/year]" representing the primary energy (total feed crop biomass) embodied in the final energy required by working animals transporting goods outside of agriculture.}
#' \item{mw_region_code_col}{The metadata column "MW.Region.code" representing the codes associated with the aggregate regions used in this package.}
#' \item{mw_region_col}{The metadata column "MW.Region" representing the a names associated with the aggregate regions used in this package.}
#' \item{metric_col}{The metadata column "Metric" representing the metric name associated with a particular data series.}
#' }
#'
#' @examples
#' amw_analysis_constants
"amw_analysis_constants"

#' Country concordance columns
#'
#' A string list containing the names of the columns containing country code information.
#'
#' @format A string list with `r length(conc_cols)` entries.
#' \describe{
#' \item{country_code_col}{The metadata column name "Country.code" containing three letter ISO 3166-1 country codes.}
#' \item{country_code_pfu_col}{The metadata column name "Country.code_PFU" containing bespoke three letter country codes.}
#' \item{country_incl_col}{The metadata column name "Country.incl." containing either "Yes" or "No" depending on whether a particulr region or country shoule be included for analysis in `MWTools`.}
#' }
#'
#' @examples
#' conc_cols
"conc_cols"

#' Unit constants
#'
#' A list containing data values for converting between different units of measurement.
#'
#' @format A string list with `r length(unit_constants)` entries.
#' \describe{
#' \item{kcal_to_mj}{The value 0.0041858, representing the conversion factor between kilocalories (kcal) and megajoules (MJ)}
#' \item{hours_to_seconds}{The value 3600, representing the conversion factor between hours and seconds.}
#' \item{joules_to_megajoules}{The value 0.000001, representing the conversion factor between joules (J) and megajoules (MJ)}
#' }
#'
#' @examples
#' unit_constants
"unit_constants"
