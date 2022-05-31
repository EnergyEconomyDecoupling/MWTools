#' General MWTools constants
#'
#' A string list containing constants used in MWTools package functions.
#'
#' @format A string list with `r length(mw_constants)` entries.
#' \describe{
#' \item{country_name}{The name of a metadata column containing full length country names.}
#' \item{species}{The name of the metadata column representing a species of live animal from FAO data.}
#' \item{value}{The name of the data column containing the values of a given observation.}
#' \item{sector_col}{The name of the metadata column containing the sector associated with the `value`.}
#' \item{stage_col}{The name of the metadata column containing the stage of the energy conversion chain associated with the `value`. Usally one of "Primary", "Final", or "Useful".}
#' \item{exemplar_method_col}{The metadata column "Exemplar/Method" representing the regional exemplar or method used for a particular data series.}
#' \item{method_source_col}{The metadata column "Method/Source" representing the method or source of information use for a particular data series.}
#' }
#'
#' @examples
#' mw_constants
"mw_constants"


#' Column names in muscle work data frames
#'
#' A string list containing package constants used in MWTools package functions.
#' This list is borrowed directly from the `IEATools` package.
#'
#' @format A string list with `r length(mw_cols)` entries.
#' \describe{
#' See `IEATools::iea_cols`.
#' }
#' @examples
#' mw_cols
"mw_cols"


#' Energy types in muscle work data frames
#'
#' A string list containing package constants used in MWTools package functions.
#' This list is borrowed directly from the `IEATools` package.
#'
#' @format A string list with `r length(energy_types)` entries.
#' \describe{
#' See `IEATools::energy_types`.
#' }
#' @examples
#' energy_types
"energy_types"


#' Methods of attributing primary energy from final energy in muscle work data frames
#'
#' A string list containing package constants used in MWTools package functions.
#' This list is borrowed directly from the `IEATools` package.
#'
#' @format A string list with `r length(methods)` entries.
#' \describe{
#' See `IEATools::methods`.
#' }
#' @examples
#' methods
"methods"


#' Names of matrix metadata columns in muscle work data frames
#'
#' A string list containing package constants used in MWTools package functions.
#' This list is borrowed directly from the `IEATools` package.
#'
#' @format A string list with `r length(mat_meta_cols)` entries.
#' \describe{
#' See `IEATools::mat_meta_cols`.
#' }
#' @examples
#' mat_meta_cols
"mat_meta_cols"


#' Row and column types for matrices made from muscle work data frames
#'
#' A string list containing package constants used in MWTools package functions.
#' This list is borrowed directly from the `IEATools` package.
#'
#' @format A string list with `r length(row_col_types)` entries.
#' \describe{
#' See `IEATools::row_col_types`.
#' }
#' @examples
#' row_col_types
"row_col_types"


#' Column names in PSUT data frames
#'
#' A string list containing package constants used in MWTools package functions.
#' This list is borrowed directly from the `IEATools` package.
#'
#' @format A string list with `r length(psut_cols)` entries.
#' \describe{
#' See `IEATools::psut_cols`.
#' }
#' @examples
#' psut_cols
"psut_cols"


#' Last stages for energy conversion chains in PSUT format
#'
#' A string list containing package constants used in MWTools package functions.
#' This list is borrowed directly from the `IEATools` package.
#'
#' @format A string list with `r length(last_stages)` entries.
#' \describe{
#' See `IEATools::last_stages`.
#' }
#' @examples
#' last_stages
"last_stages"


#' All stages for energy conversion chains
#'
#' A string list containing package constants used in MWTools package functions.
#' This list is borrowed directly from the `IEATools` package.
#'
#' @format A string list with `r length(all_stages)` entries.
#' \describe{
#' See `IEATools::all_stages`.
#' }
#' @examples
#' all_stages
"all_stages"


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


#' Human muscle work (hmw) analysis data constants
#'
#' A string list containing a selection of hmw analysis constants.
#'
#' @format A string list with `r length(hmw_analysis_constants)` entries.
#' \describe{
#' \item{total_wk_hrs_ilo_col}{The data column "Total.hours \[hours/year\]" containing the number of hours worked yearly by all humans for a given country, sex, sector, and year.}
#' \item{col1960}{The column for 1960. Default is "1960".}
#' \item{col2020}{The column for 2020. Default is "2020".}
#' \item{labor_type_col}{The data column "Labor.Type" containing information on the human labor type (Primary, Secondary, or Sedentary) of human workers.}
#' \item{labor_split_col}{The data column "Labor.Type.Split" containing information on the proportion of human labor type by sector.}
#' \item{hmw_power_sheet}{The excel sheet name "hmw_power", which contains information on the power output of humans performing muscle work.}
#' \item{hmw_food_sheet}{The excel sheet name "hmw_food", which contains information on the food consumption of humans.}
#' \item{hmw_labor_map_sheet}{The excel sheet name "hmw_sector_labor_map", which contains data on the split of labor type by ILO broad sector.}
#' \item{hmw_sector_map_sheet}{The excel sheet name "hmw_sector_map", which contains information on the mapping the sector names used in the hmw analysis data excel file bundled with this package with the sector names in the ILO data.}
#' \item{hmw_plate_waste_sheet}{The excel sheet name "hmw_plate_waste", which contains information on proportion of food waste by region.}
#' \item{hmw_harvest_waste_sheet}{The excel sheet name "hmw_harvest_waste", which contains information on the proportion of phytomass lost at the harvest stage by region.}
#' \item{food_consumption_col}{The data column "Food consumption \[kcal/day per person\]", which contains information on the daily food consumption of humans in kilocalories (kcal).}
#' \item{energy_pppa_col}{The data column "Energy consumption \[MJ/year per person\]", which contains information on the yearly food consumption per working person in megajoules (MJ).}
#' \item{final_energy_col}{The data column "Final energy \[MJ/year\]", which contains data on the yearly food consumption of all working persons in megajoules (MJ).}
#' \item{primary_energy_col}{The data column "Primary energy \[MJ/year\]", which contains data on the yearly quantity of biomass required to meet the food requirements of all working persons in megajoules (MJ).}
#' \item{useful_energy_hmw_col}{The data column "Useful energy \[MJ/year\]", which contains data on the yearly useful work performed by all working persons in megajoules (MJ).}
#' \item{power_col}{The data column "Power \[W\]", which contains data on the power output of humans performing muscle work.}
#' \item{plate_waste_col}{The data column "Plate waste \[-\]", which contains data on the proportion of food waste by region.}
#' \item{hmw_harvest_waste_col}{The data column "arvest waste \[-\]", which contains data on the proportion of phytomass lost at the harvest stage by region.}
#' }
#'
#' @examples
#' hmw_analysis_constants
"hmw_analysis_constants"


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
#' \item{human_females}{The string "Human females", representing human females.}
#' \item{human_males}{The string "Human males", representing human males.}
#' \item{human}{The string "Human", representing all humans.}
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


#' Animal muscle work (amw) analysis data constants
#'
#' A string list containing a selection of amw analysis constants, used in the "amw_analysis_data.R" excel file.
#'
#' @format A string list with `r length(amw_analysis_constants)` entries.
#' \describe{
#' \item{prop_working_animals_col}{The metadata column "Prop.Working.animals" representing the proportion of live animals that are working animals.}
#' \item{prop_wkg_anmls_ag_col}{The metadata column "Prop.Working.animals.Ag" representing the proportion of working animals that work in agriculture.}
#' \item{prop_wkg_anmls_tr_col}{The metadata column "Prop.Working.animals.Tr" representing the proportion of working animals that work transporting goods outside of agriculture.}
#' \item{wa_perc_sheet}{The excel sheet name "WA_perc" representing the sheet containing information on the proportion of working animals.}
#' \item{wa_enduse_sheet}{The excel sheet name "WA_enduse" representing the sheet containing information of the proportion of working animals in agriculture/transport.}
#' \item{wa_feed_sheet}{The excel sheet name "WA_feed" representing the sheet containing information on the feed requirements of working animals.}
#' \item{wa_days_hours_sheet}{The excel sheet name "WA_days_hours" representing the sheet containing information on the number of days and hours worked by working animals.}
#' \item{wa_power_sheet}{The excel sheet name "WA_power" representing the sheet containing information on the power output of working animals.}
#' \item{working_seconds_col}{The data column "Working.seconds \[seconds per animal\]" representing the number of seconds of work performed by working animals.}
#' \item{working_hours_col}{The data column "Working.hours \[hour per animal\]" representing the number of hours of work performed by working animals.}
#' \item{working_days_col}{The data column "Working.days \[day per animal\]" representing the number of days that working animals worked.}
#' \item{nonworking_days_col}{The data column "Non-Working days \[day per animal\]" representing the number of days that working animals did not work.}
#' \item{power_per_animal}{The data column "Power.per.animal \[W\]" representing the power output of working animals.}
#' \item{live_animals_col}{The data column "Live.animals" representing the number of live animals.}
#' \item{working_animals_col}{The data column "Working.animals" representing the number of working animals.}
#' \item{working_animals_total_col}{The data column "Working.animals.total" representing the total number of working animals.}
#' \item{working_animals_ag_col}{The data column "Working.animals.Ag" representing the number of working animals that work in agriculture.}
#' \item{working_animals_tr_col}{The data column "Working.animals.Tr" representing the number of working animals that work transporting goods outside of agriculture.}
#' \item{working_day_feed_col}{The data column "Working.day.feed \[MJ/day per animal\]" representing the quantity of feed required by working animals each working day.}
#' \item{nonworking_day_feed_col}{The data column "Non-Working.day.feed \[MJ/day per animal\]" representing the quantity of feed required by working animals each non-working day. }
#' \item{working_yearly_feed_col}{The data column "Working.yearly.feed \[MJ/year per animal\]" representing the quantity of feed required by working animals on working days each year.}
#' \item{nonwkg_yearly_feed_col}{The data column "Non-Working.yearly.feed \[MJ/year per animal\]" representing the quantity of feed required by working animals on non-working days each year.}
#' \item{total_yearly_feed_col}{The data column "Total.yearly.feed \[MJ/year per animal\]" representing the quantity of feed required by working animals each year.}
#' \item{useful_energy_total}{The data column "Useful.energy.total \[MJ/year\]" representing the total useful energy produced by working animals through muscle work.}
#' \item{useful_energy_ag}{The data column "Useful.energy.Ag \[MJ/year\]" representing the useful energy produced by working animals through muscle work in agriculture.}
#' \item{useful_energy_tr}{The data column "Useful.energy.Tr \[MJ/year\]" representing the useful energy produced by working animals through muscle work transporting goods outside of agriculture.}
#' \item{final_energy_total}{The data column "Final.energy.total \[MJ/year\]" representing the total final energy (feed) required by working animals.}
#' \item{final_energy_ag}{The data column "Final.energy.Ag \[MJ/year\]" representing the final energy (feed) required by working animals in agriculture.}
#' \item{final_energy_tr}{The data column "Final.energy.Tr \[MJ/year\]" representing the final energy (feed) required by working animals working to transport goods outside of agriculture.}
#' \item{primary_energy_total}{The data column "Primary.energy.total \[MJ/year\]" representing the primary energy (total feed crop biomass) embodied in the total final energy required by working animals.}
#' \item{primary_energy_ag}{The data column "Primary.energy.Ag \[MJ/year\]" representing the primary energy (total feed crop biomass) embodied in the final energy required by working animals in agriculture.}
#' \item{primary_energy_tr}{The data column "Primary.energy.Tr \[MJ/year\]" representing the primary energy (total feed crop biomass) embodied in the final energy required by working animals transporting goods outside of agriculture.}
#' \item{amw_region_col}{The data column "MMW.Region" representing the animal muscle work region.}
#' \item{metric_col}{The metadata column "Metric" representing the metric name associated with a particular data series.}
#' \item{yes_const}{The string "Yes" representing when a quantity is present or applicable.}
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
#' \item{country_col}{The metadata column name "Country" containing three letter ISO 3166-1 country codes.}
#' \item{country_code_pfu_col}{The metadata column name "Country_PFU" containing bespoke three letter country codes.}
#' \item{country_incl_col}{The metadata column name "Country.incl." containing either "Yes" or "No" depending on whether a particulr region or country shoule be included for analysis in `MWTools`.}
#' \item{amw_region_code_col}{The metadata column "AMW.Region.code" telling the region for this row.}
#' \item{hmw_region_code_col}{The metadata column "HMW.Region.code" telling the region for this row.}
#' \item{mapping_sheet}{The name of the sheet for name mappings.}
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
#' \item{EJ_to_ktoe}{The value 23884.58966275, representing the conversion factor between exajoules (EJ) and kilotonnes of oil equivalent (ktoe)}
#' }
#'
#' @examples
#' unit_constants
"unit_constants"


#' Muscle work products
#'
#' A list containing names of muscle work products (energy carriers).
#'
#' @format A string list with `r length(mw_products)` entries.
#' \describe{
#' \item{food}{The name for food final energy.}
#' \item{feed}{The name for feed final energy.}
#' \item{biomass}{The name for biomass primary energy.}
#' \item{hu_mech}{The name for human mechanical work useful energy.}
#' \item{an_mech}{The name for animal mechanical work useful energy.}
#' \item{an_p}{The name for animal propulsion work useful energy.}
#' }
#'
#' @examples
#' mw_products
"mw_products"


#' Muscle work phi constants
#'
#' A data frame containing
#'     - muscle work products (energy carriers),
#'     - phi (exergy-to-energy ratio) values,
#'     - and an "is.useful" column saying whether that energy carrier is found
#'       at the useful stage.
#'
#' @format A data frame with `r nrow(phi_constants_mw)` rows. Each row provides the phi value for one energy carrier:
#' \describe{
#' \item{Product}{A string column of muscle work energy carriers.}
#' \item{phi}{A string column of exergy-to-energy ratios.}
#' \item{is.useful}{A boolean column telling whether this energy carrier is found at the useful stage (or not).}
#' }
#'
#' @examples
#' phi_constants_mw
"phi_constants_mw"


#' Economic sectors for muscle work
#'
#' A list containing names of economic sectors for muscle work.
#'
#' @format A string list with `r length(mw_sectors)` entries.
#' \describe{
#' \item{broad_sector_col}{The column name "broad_sector", which contains the "Broad sector" sector names used by the ILO.}
#' \item{agriculture_broad.sector}{The string "Agriculture" representing the agriculture sector. One of the ILO's "Broad sectors".}
#' \item{industry_broad.sector}{The string "Industry" representing the industry sector. One of the ILO's "Broad sectors".}
#' \item{services_broad.sector}{The string "Services" representing the services sector. One of the ILO's "Broad sectors".}
#' \item{transport.sector}{The string "Transport" representing the transport sector.}
#' \item{not_classified_broad.sector}{The string "Not classfied" representing unclassified labor data. One of the ILO's "Broad sectors".}
#' \item{non_agriculture_broad.sector}{The string "Non-agriculture" representing all sectors outside of agriculture. One of the ILO's "Broad sectors".}
#' \item{total_sector}{The string "Total" representing the entire economy. One of the ILO's "Broad sectors".}
#' \item{resources_sectors}{The string "Resources" representing resouce sectors.}
#' \item{farms}{The string "Farms" representing the farming sector.}
#' \item{food_production}{The string "Food production" representing the food production sector.}
#' \item{feed_production}{The string "Feed production" representing the feed production sector.}
#' }
#'
#' @examples
#' mw_sectors
"mw_sectors"


#' All energy stages
#'
#' A string list containing options for the all stages of energy conversion chain analysis.
#' The values of these constants are taken from the `IEATools` package for consistency.
#'
#' @format A string list with `r length(all_stages)`
#' \describe{
#' \item{primary}{The string identifier for the Primary stage of the energy conversion chain.}
#' \item{final}{The string identifier for the Final stage of the energy conversion chain.}
#' \item{useful}{The string identifier for the Useful stage of the energy conversion chain.}
#' \item{services}{The string identifier for the Services stage of the energy conversion chain.}
#' }
#' @examples
#' all_stages
"all_stages"

