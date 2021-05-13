### SOURCE this file when adding new constants ###
### Document constants in R/Data.R script ###

#
# Constants for use in MWTools package
#

mw_constants <- list(country_name = "Country.name",
                     year = "Year",
                     species = "Species",
                     unit = "Unit",
                     value = "Value",
                     sector_col = "Sector",
                     stage_col = "Stage",
                     energy_mj_year = "Energy [MJ/year]",
                     exemplar_method_col = "Exemplar/Method",
                     method_source_col = "Method/Source")

usethis::use_data(mw_constants, overwrite = TRUE)


#
# ILO bundled data columns
#
ilo_cols <- list(ref_area_col = "ref_area",
                 sex_ilo_col = "Sex",
                 yearly_working_hours_ilo_col = "Working.hours [hours/year]",
                 employed_persons_ilo_col = "Employed.persons [persons]")

usethis::use_data(ilo_cols, overwrite = TRUE)


#
# Human Muscle Work (hmw) analysis data constants
#
hmw_analysis_constants <- list(total_working_hours_ilo_col = "Total.hours [hours/year]",
                               industry_activity_col = "Industry/Activity",
                               hmw_power_sheet = "hmw_power",
                               hmw_food_sheet = "hmw_food",
                               hmw_sector_map_sheet = "hmw_sector_map",
                               broad_sector_col = "broad_sector",
                               hmw_analysis_sector_col = "Sector.hmw",
                               agriculture_broad.sector = "Agriculture",
                               industry_broad.sector = "Industry",
                               services_broad.sector = "Services",
                               not_classified_broad.sector = "Not classified",
                               non_agriculture_broad.sector = "Non-agriculture",
                               total_sector = "Total",
                               food_consumption_col = "Food consumption [kcal/day per person]",
                               yearly_energy_consumption_pp_col = "Energy consumption [MJ/year per person]",
                               final_energy_col = "Final energy [MJ/year]",
                               useful_energy_hmw_col = "Useful energy [MJ/year]",
                               power_col = "Power [W]")

usethis::use_data(hmw_analysis_constants, overwrite = TRUE)


#
# Working animal species
#
mw_species <- list(asses = "Asses",
                   camels = "Camels",
                   cattle = "Cattle",
                   horses = "Horses",
                   mules = "Mules",
                   buffaloes = "Buffaloes",
                   camelids_other = "Camelids, other",
                   camelids = "Camelids")

usethis::use_data(mw_species, overwrite = TRUE)

#
# FAO data columns
#
fao_cols <- list(area_fao_col = "area",
                 item_fao_col = "item",
                 year_fao_col = "year",
                 unit_fao_col = "unit",
                 value_fao_col = "value")

usethis::use_data(fao_cols, overwrite = TRUE)

#
# FAO data codes
#
fao_codes <- list(live_animals_code = "QA")

usethis::use_data(fao_codes, overwrite = TRUE)

#
# ILO data codes
#
ilo_codes <- list(working_hours_code = "HOW_TEMP_SEX_ECO_NB_A",
                  employment_code = "EMP_TEMP_SEX_ECO_NB_A")

usethis::use_data(ilo_codes, overwrite = TRUE)

#
# Animal Muscle Work (amw) analysis data constants
#
amw_analysis_constants <- list(prop_working_animals_col = "Prop.Working.animals",
                               prop_working_animals_ag_col = "Prop.Working.animals.Ag",
                               prop_working_animals_tr_col = "Prop.Working.animals.Tr",
                               wa_perc_sheet = "WA_perc",
                               wa_enduse_sheet = "WA_enduse",
                               wa_feed_sheet = "WA_feed",
                               wa_days_hours_sheet = "WA_days_hours",
                               wa_power_sheet = "WA_power",
                               working_seconds_col = "Working.seconds [seconds per animal]",
                               working_hours_col = "Working.hours [hour per animal]",
                               working_days_col = "Working.days [day per animal]",
                               nonworking_days_col = "Non-Working days [day per animal]",
                               power_per_animal = "Power.per.animal [W]",
                               live_animals_col = "Live.animals",
                               working_animals_col = "Working.animals",
                               working_animals_total_col = "Working.animals.total",
                               working_animals_ag_col = "Working.animals.Ag",
                               working_animals_tr_col = "Working.animals.Tr",
                               working_day_feed_col = "Working.day.feed [MJ/day per animal]",
                               nonworking_day_feed_col = "Non-Working.day.feed [MJ/day per animal]",
                               working_yearly_feed_col = "Working.yearly.feed [MJ/year per animal]",
                               nonworking_yearly_feed_col = "Non-Working.yearly.feed [MJ/year per animal]",
                               total_yearly_feed_col = "Total.yearly.feed [MJ/year per animal]",
                               useful_energy_total = "Useful.energy.total [MJ/year]",
                               useful_energy_ag = "Useful.energy.Ag [MJ/year]",
                               useful_energy_tr = "Useful.energy.Tr [MJ/year]",
                               final_energy_total = "Final.energy.total [MJ/year]",
                               final_energy_ag = "Final.energy.Ag [MJ/year]",
                               final_energy_tr = "Final.energy.Tr [MJ/year]",
                               primary_energy_total = "Primary.energy.total [MJ/year]",
                               primary_energy_ag = "Primary.energy.Ag [MJ/year]",
                               primary_energy_tr = "Primary.energy.Tr [MJ/year]",
                               # ag_string
                               # tr_string
                               # total_string
                               # ag_full_string
                               # tr_full_string
                               # total_full_string
                               amw_region_col = "AMW.Region",
                               metric_col = "Metric")

usethis::use_data(amw_analysis_constants, overwrite = TRUE)

#
# Concordance columns
#
conc_cols <- list(country_code_col = "Country.code",
                  country_code_pfu_col = "Country.code_PFU",
                  country_incl_col = "Country.incl.",
                  amw_region_code_col = "AMW.Region.code",
                  hmw_region_code_col = "HMW.Region.code")

usethis::use_data(conc_cols, overwrite = TRUE)

#
# Unit conversion constants
#
unit_constants <- list(kcal_to_mj = 0.0041858,
                       hours_to_seconds = 3600,
                       joules_to_megajoules = 0.000001)

usethis::use_data(unit_constants, overwrite = TRUE)

