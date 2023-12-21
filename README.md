
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/MWTools)](https://CRAN.R-project.org/package=MWTools)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Codecov test
coverage](https://codecov.io/gh/EnergyEconomyDecoupling/MWTools/branch/master/graph/badge.svg)](https://codecov.io/gh/EnergyEconomyDecoupling/MWTools?branch=master)
[![R-CMD-check](https://github.com/EnergyEconomyDecoupling/MWTools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EnergyEconomyDecoupling/MWTools/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7584859.svg)](https://doi.org/10.5281/zenodo.7584859)
<!-- badges: end -->

# MWTools

## Statement of need

In societal energy analysis and societal exergy analysis (SEA), animals
and humans provide energy to society by performing *muscle work*. A
previous study by [Steenwyk et al.
(2022)](https://doi.org/10.1007/s41247-022-00096-z) developed and
standardized ways to estimate muscle work contributed to society by
animals and humans. And data exist from the UN’s [Food and Agriculture
Organization](https://www.fao.org/home/en) (FAO) and the [International
Labor Organization](https://www.ilo.org) (ILO) that could be used for
such purposes. However, none of the data are in the PSUT format proposed
by [Heun, Owen, and Brockway
(2018)](https://doi.org/10.1016/j.apenergy.2018.05.109). Computational
tools are needed to gather and shape relevant data.

This package provides functions for the estimation of animal and human
muscle work for use in SEA with the Physical Supply Use Table (PSUT)
framework.

## Installation

You can install this package from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EnergyEconomyDecoupling/MWTools")
```

<!-- ## Examples -->
<!-- ### Animal Muscle Work -->
<!-- Raw data for the estimation of animal muscle work is obtained from the Food and Agriculture Organisation of the United Nations Statistical Database (FAOSTAT), via the `R` package `FAOSTAT`. The `MWTools` package provides a wrapper function for downloading data for the number of live animals, `down_fao_live_animals`, which utilises the function `FAOSTAT::get_faostat_bulk()`. Once downloaded `MWTools` provides a number of functions for the calculation of the number of working animals by species, country, year and in agriculture, transport, and in total. -->
<!-- The helper function `calc_amw_pfu` returns a tidy data frame containing data for the primary, final, and useful energy by species, country, year and in agriculture, transport, and in total. -->
<!-- ```{r get_example_data} -->
<!-- library(MWTools) -->
<!-- fao_fp <- PFUSetup::get_abs_paths(version = "v1.2")$fao_data_path -->
<!-- fao_raw_rds <- readr::read_rds(file = fao_fp) -->
<!-- amw_pfu_data <- calc_amw_pfu(.df = fao_raw_rds, -->
<!--                              concordance_path = PFUSetup::get_abs_paths()$mw_concordance_path, -->
<!--                              amw_analysis_data_path = PFUSetup::get_abs_paths()$amw_analysis_data_path) -->
<!-- head(amw_pfu_data) -->
<!-- ``` -->
<!-- ### Human Muscle Work -->
<!-- Raw data for the estimation of human muscle work is obtained from the International Labor Organisation (ILO), via the `R` package `Rilostat`. The `MWTools` package includes bundled ILO data for the number of employed persons by sector and mean number of working hours by sector. -->
<!-- Using data for the number of employed persons and mean yearly working hours the primary, final, and useful energy associated with human muscle work can be estimated using the helper function `MWTools::calc_hmw_pfu`. -->
<!-- ``` {r hmw_pfu} -->
<!-- library(MWTools) -->
<!-- ilo_working_hours_data <- readr::read_rds(file = PFUSetup::get_abs_paths(version = "v1.2")$ilo_working_hours_data_path) -->
<!-- ilo_employment_data <- readr::read_rds(file = PFUSetup::get_abs_paths(version = "v1.2")$ilo_employment_data_path) -->
<!-- preparedILOData <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data, -->
<!--                                      ilo_employment_data = ilo_employment_data) -->
<!-- hmw_pfu_data <- calc_hmw_pfu(.df = preparedILOData, -->
<!--                              concordance_path = PFUSetup::get_abs_paths()$mw_concordance_path, -->
<!--                              hmw_analysis_data_path = PFUSetup::get_abs_paths()$hmw_analysis_data_path) -->
<!-- head(hmw_pfu_data) -->
<!-- ``` -->
<!-- ### PSUT matrices -->
<!-- PFU data can be converted to PSUT matrices using `prep_psut()`. -->

## More Information

Find more information, including vignettes and function documentation,
at <https://energyeconomydecoupling.github.io/MWTools/>.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Heun:2018" class="csl-entry">

Heun, Matthew Kuperus, Anne Owen, and Paul E. Brockway. 2018. “A
Physical Supply-Use Table Framework for Energy Analysis on the Energy
Conversion Chain.” *Applied Energy* 226 (September): 1134–62.
<https://doi.org/10.1016/j.apenergy.2018.05.109>.

</div>

<div id="ref-Steenwyk:2022ww" class="csl-entry">

Steenwyk, Paul, Matthew Kuperus Heun, Paul Brockway, Tânia Sousa, and
Sofia Henriques. 2022. “The Contributions of Muscle and Machine Work to
Land and Labor Productivity in World Agriculture Since 1800.”
*Biophysical Economics and Sustainability* 7 (2): 1–17.
<https://doi.org/10.1007/s41247-022-00096-z>.

</div>

</div>
