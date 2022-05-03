
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MWTools

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/EnergyEconomyDecoupling/MWTools/branch/master/graph/badge.svg)](https://codecov.io/gh/EnergyEconomyDecoupling/MWTools?branch=master)
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

fao_fp <- PFUSetup::get_abs_paths()$fao_data_path

fao_raw_rds <- readr::read_rds(file = fao_fp)

amw_pfu_data <- calc_amw_pfu(.df = fao_raw_rds,
                             concordance_path = PFUSetup::get_abs_paths()$mw_concordance_path,
                             amw_analysis_path = PFUSetup::get_abs_paths()$amw_analysis_data_path)

head(amw_pfu_data)
#> # A tibble: 6 × 7
#>   Country  Year Species Stage   Sector      Units    E.dot
#>   <chr>   <dbl> <chr>   <chr>   <chr>       <chr>    <dbl>
#> 1 AFG      1961 Asses   Useful  Agriculture EJ    0.000125
#> 2 AFG      1961 Asses   Useful  Transport   EJ    0.000706
#> 3 AFG      1961 Asses   Final   Agriculture EJ    0.00190 
#> 4 AFG      1961 Asses   Final   Transport   EJ    0.0108  
#> 5 AFG      1961 Asses   Primary Agriculture EJ    0.00423 
#> 6 AFG      1961 Asses   Primary Transport   EJ    0.0240
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
ilo_fp <- PFUSetup::get_abs_paths()$ilo_data_path

ilo_raw_rds <- readr::read_rds(file = ilo_fp)

hmw_pfu_data <- calc_hmw_pfu(.df = ilo_raw_rds,
                             concordance_path = PFUSetup::get_abs_paths()$mw_concordance_path,
                             hmw_analysis_data_path = PFUSetup::get_abs_paths()$hmw_analysis_data_path)

head(hmw_pfu_data)
#> # A tibble: 6 × 7
#>   Country  Year Species       Stage   Sector      Units E.dot
#>   <chr>   <dbl> <chr>         <chr>   <chr>       <chr> <dbl>
#> 1 ABW      1994 Human Females Final   Agriculture EJ       NA
#> 2 ABW      1994 Human Females Primary Agriculture EJ       NA
#> 3 ABW      1994 Human Females Useful  Agriculture EJ       NA
#> 4 ABW      1997 Human Females Final   Agriculture EJ       NA
#> 5 ABW      1997 Human Females Primary Agriculture EJ       NA
#> 6 ABW      1997 Human Females Useful  Agriculture EJ       NA
```
