
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
