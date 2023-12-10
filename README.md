
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MWTools

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/EnergyEconomyDecoupling/MWTools/branch/master/graph/badge.svg)](https://codecov.io/gh/EnergyEconomyDecoupling/MWTools?branch=master)
[![R-CMD-check](https://github.com/EnergyEconomyDecoupling/MWTools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EnergyEconomyDecoupling/MWTools/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7584859.svg)](https://doi.org/10.5281/zenodo.7584859)
<!-- badges: end -->

The `R` package `MWTools` provides functions for the estimation of human
and animal muscle work for use in Societal Exergy Analysis (SEA), and
using the Physical Supply Use Table (PSUT) framework.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EnergyEconomyDecoupling/MWTools")
```

## Animal Muscle Work

Raw data for the estimation of animal muscle work is obtained from the
Food and Agriculture Organisation of the United Nations Statistical
Database (FAOSTAT), via the `R` package `FAOSTAT`. The `MWTools` package
provides a wrapper function for downloading data for the number of live
animals, `down_fao_live_animals`, which utilises the function
`FAOSTAT::get_faostat_bulk()`. Once downloaded `MWTools` provides a
number of functions for the calculation of the number of working animals
by species, country, year and in agriculture, transport, and in total.

The helper function `calc_amw_pfu` returns a tidy data frame containing
data for the primary, final, and useful energy by species, country, year
and in agriculture, transport, and in total.

``` r
library(MWTools)

fao_fp <- PFUSetup::get_abs_paths(version = "v1.2")$fao_data_path

fao_raw_rds <- readr::read_rds(file = fao_fp)

amw_pfu_data <- calc_amw_pfu(.df = fao_raw_rds,
                             concordance_path = PFUSetup::get_abs_paths()$mw_concordance_path,
                             amw_analysis_data_path = PFUSetup::get_abs_paths()$amw_analysis_data_path)

head(amw_pfu_data)
#> # A tibble: 6 × 7
#>   Country  Year Species Stage   Sector      Unit     E.dot
#>   <chr>   <dbl> <chr>   <chr>   <chr>       <chr>    <dbl>
#> 1 AFG      1960 Asses   Useful  Agriculture EJ    0.000125
#> 2 AFG      1960 Asses   Useful  Transport   EJ    0.000706
#> 3 AFG      1960 Asses   Final   Agriculture EJ    0.00190 
#> 4 AFG      1960 Asses   Final   Transport   EJ    0.0108  
#> 5 AFG      1960 Asses   Primary Agriculture EJ    0.00423 
#> 6 AFG      1960 Asses   Primary Transport   EJ    0.0240
```

## Human Muscle Work

Raw data for the estimation of human muscle work is obtained from the
International Labor Organisation (ILO), via the `R` package `Rilostat`.
The `MWTools` package includes bundled ILO data for the number of
employed persons by sector and mean number of working hours by sector.

Using data for the number of employed persons and mean yearly working
hours the primary, final, and useful energy associated with human muscle
work can be estimated using the helper function `MWTools::calc_hmw_pfu`.

``` r
library(MWTools)

ilo_working_hours_data <- readr::read_rds(file = PFUSetup::get_abs_paths(version = "v1.2")$ilo_working_hours_data_path)
ilo_employment_data <- readr::read_rds(file = PFUSetup::get_abs_paths(version = "v1.2")$ilo_employment_data_path)

preparedILOData <- prepareRawILOData(ilo_working_hours_data = ilo_working_hours_data,
                                     ilo_employment_data = ilo_employment_data)

hmw_pfu_data <- calc_hmw_pfu(.df = preparedILOData,
                             concordance_path = PFUSetup::get_abs_paths()$mw_concordance_path,
                             hmw_analysis_data_path = PFUSetup::get_abs_paths()$hmw_analysis_data_path)

head(hmw_pfu_data)
#> # A tibble: 6 × 7
#>   Country  Year Species       Stage   Sector      Unit     E.dot
#>   <chr>   <dbl> <chr>         <chr>   <chr>       <chr>    <dbl>
#> 1 AFG      1960 Human females Final   Agriculture EJ    0.00686 
#> 2 AFG      1960 Human females Final   Industry    EJ    0.000842
#> 3 AFG      1960 Human females Final   Services    EJ    0.000393
#> 4 AFG      1960 Human females Primary Agriculture EJ    0.0156  
#> 5 AFG      1960 Human females Primary Industry    EJ    0.00191 
#> 6 AFG      1960 Human females Primary Services    EJ    0.000892
```

## PSUT matrices

PFU data can be converted to PSUT matrices using `prep_psut()`.

## More Information

Find more information, including vignettes and function documentation,
at <https://energyeconomydecoupling.github.io/MWTools/>.
