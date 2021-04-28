
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MWTools

<!-- badges: start -->
<!-- badges: end -->

The goal of MWTools is to …

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EnergyEconomyDecoupling/MWTools")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(MWTools)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

Use the `plot_amw_summary` function to produce a plot summarising the
animal muscle work data for a particular country and sector.

``` r
plot_amw_summary(amw_pfu_df = all_data,
                 amw_numbers_df = all_numbers,
                 country = "CHN",
                 sector = "Total")
```

<img src="man/figures/README-example_plot-1.png" width="100%" />

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
