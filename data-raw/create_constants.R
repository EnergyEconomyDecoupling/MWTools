### SOURCE this file when adding new constants ###
### Document constants in R/Data.R script ###

library(IEATools)

#
# Constants for use in MWTools package
#

mw_constants <- list(country_name = "Area",
                     species = "Item",
                     value = "Value",
                     value_count = "Value.count",
                     sector_col = "Sector",
                     stage_col = "Stage",
                     exemplar_method_col = "Exemplar/Method",
                     method_source_col = "Method/Source")
usethis::use_data(mw_constants, overwrite = TRUE)


#
# Names of columns in the tidy muscle work data frame
#
mw_cols <- IEATools::iea_cols
usethis::use_data(mw_cols, overwrite = TRUE)


#
# Types of energy
#
energy_types <- IEATools::energy_types
usethis::use_data(energy_types, overwrite = TRUE)


#
# Ways of counting primary energy associated with
# renewable or biomass final energy
#
methods <- IEATools::methods
usethis::use_data(methods, overwrite = TRUE)


#
# Names for metadata columns in data frames
# to be collapsed into matrices
#
mat_meta_cols <- IEATools::mat_meta_cols
usethis::use_data(mat_meta_cols, overwrite = TRUE)


#
# Row and column types
#
row_col_types <- IEATools::row_col_types
usethis::use_data(row_col_types, overwrite = TRUE)

#
# Names for PSUT columns
#
psut_cols <- IEATools::psut_cols
usethis::use_data(psut_cols, overwrite = TRUE)


#
# Possible last stages
#
last_stages <- IEATools::last_stages
usethis::use_data(last_stages, overwrite = TRUE)


#
# All energy conversion chain stages
#
all_stages <- IEATools::all_stages
usethis::use_data(all_stages, overwrite = TRUE)


#
# ILO bundled data columns
#
ilo_cols <- list(ref_area_col = "ref_area",
                 sex_ilo_col = "sex",
                 yearly_working_hours_ilo_col = "Working.hours [hours/year]",
                 employed_persons_ilo_col = "Employed.persons [persons]",
                 employed_count = "Employed.persons.count",
                 hours_count = "Working.hours.count")
usethis::use_data(ilo_cols, overwrite = TRUE)


#
# Human Muscle Work (hmw) analysis data constants
#
hmw_analysis_constants <- list(total_wk_hrs_ilo_col = "Total.hours [hours/year]",
                               col_1960 = "1960",
                               col_2020 = "2020",
                               labor_type_col = "Labor.Type",
                               labor_split_col = "Labor.Type.Split",
                               hmw_power_sheet = "hmw_power",
                               hmw_food_sheet = "hmw_food",
                               hmw_sector_map_sheet = "hmw_sector_map",
                               hmw_labor_map_sheet = "hmw_sector_labor_map",
                               hmw_plate_waste_sheet = "hmw_plate_waste",
                               hmw_harvest_waste_sheet = "hmw_harvest_waste",
                               food_consumption_col = "Food consumption [kcal/day per person]",
                               energy_pppa_col = "Energy consumption [MJ/year per person]",
                               final_energy_col = "Final energy [MJ/year]",
                               primary_energy_col = "Primary energy [MJ/year]",
                               useful_energy_hmw_col = "Useful energy [MJ/year]",
                               power_col = "Power [W]",
                               plate_waste_col = "Plate waste [-]",
                               hmw_harvest_waste_col = "Harvest waste [-]")
usethis::use_data(hmw_analysis_constants, overwrite = TRUE)


#
# Human Muscle Work (hmw) sector constants
#
hmw_sector_constants <- list(ilo_agr_name = "ECO_SECTOR_AGR",
                             ilo_ind_name = "ECO_SECTOR_IND",
                             ilo_ser_name = "ECO_SECTOR_SER",
                             agr_name = "Agriculture",
                             ind_name = "Industry",
                             ser_name = "Services")
usethis::use_data(hmw_sector_constants, overwrite = TRUE)


#
# Human Muscle Work (hmw) sex constants
#
hmw_sex_constants <- list(ilo_female_name = "SEX_F",
                          ilo_male_name = "SEX_M",
                          ilo_other_name = "SEX_O",
                          ilo_total_name = "SEX_T",
                          female_name = "Female",
                          male_name = "Male",
                          other_name = "Other",
                          total_name = "Total")
usethis::use_data(hmw_sex_constants, overwrite = TRUE)


#
# Working animal species
#
mw_species <- list(asses = "Asses",
                   buffaloes = "Buffalo",
                   camels = "Camels",
                   cattle = "Cattle",
                   # cattle_and_buffaloes = "Cattle and Buffaloes",
                   horses = "Horses",
                   mules = "Mules and hinnies",
                   camelids_other = "Other camelids",
                   camelids = "Camelids",
                   human_females = "Human females",
                   human_males = "Human males",
                   human = "Human")
usethis::use_data(mw_species, overwrite = TRUE)


#
# FAO data columns
#
fao_cols <- list(area_fao_col = "Area",
                 item_fao_col = "Item",
                 year_fao_col = "Year",
                 unit_fao_col = "Unit",
                 value_fao_col = "Value")
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
                               prop_wkg_anmls_ag_col = "Prop.Working.animals.Ag",
                               prop_wkg_anmls_tr_col = "Prop.Working.animals.Tr",
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
                               nonwkg_yearly_feed_col = "Non-Working.yearly.feed [MJ/year per animal]",
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
                               amw_region_col = "AMW.Region",
                               metric_col = "Metric",
                               yes_const = "Yes")
usethis::use_data(amw_analysis_constants, overwrite = TRUE)


#
# Concordance columns
#
conc_cols <- list(country_code_col = "Country.code",
                  country_col = "Country",
                  country_incl_col = "Country.incl.",
                  exemplar_country_col = "Exemplar.country",
                  amw_region_code_col = "AMW.Region.code",
                  hmw_region_code_col = "HMW.Region.code",
                  region_code_col = "Region.code",
                  mapping_sheet = "Mapping",
                  species = "Species")
usethis::use_data(conc_cols, overwrite = TRUE)


#
# Unit conversion constants
#
unit_constants <- list(kcal_to_mj = 0.0041858,
                       hours_to_seconds = 3600,
                       joules_to_megajoules = 0.000001,
                       EJ_to_ktoe = 23884.58966275,
                       EJ_to_TJ = 1e6)
usethis::use_data(unit_constants, overwrite = TRUE)


#
# Energy products
#
mw_products <- list(food = "Food",
                    feed = "Feed",
                    biomass = "Biomass",
                    hu_mech = "HuMech",
                    an_mech = "AnMech",
                    an_p = "AnP")
usethis::use_data(mw_products, overwrite = TRUE)


#
# phi constants
#
phi_constants_mw <- tibble::tibble(Product = unlist(mw_products),
                                   phi = 1,
                                   is.useful = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
usethis::use_data(phi_constants_mw, overwrite = TRUE)


#
# Economic sectors
#
mw_sectors <- list(broad_sector_col = "broad_sector",
                   agriculture_broad.sector = "Agriculture",
                   industry_broad.sector = "Industry",
                   services_broad.sector = "Services",
                   transport_sector = "Transport",
                   not_classified_broad.sector = "Not classified",
                   non_agriculture_broad.sector = "Non-agriculture",
                   total_sector = "Total",
                   resources_sector = "Resources",
                   farms = "Farms",
                   food_production = "Food production",
                   feed_production = "Feed production")
usethis::use_data(mw_sectors, overwrite = TRUE)


