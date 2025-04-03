---
title: "Release notes for `MWTools`"
output: html_document
---

Cite all releases with doi [10.5281/zenodo.7584858](https://doi.org/10.5281/zenodo.7584858), 
which always resolves to the latest release.


## MWTools 0.2.10 (2025-04-02) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15128150.svg)](https://doi.org/10.5281/zenodo.15128150)

* Changed `tidy_pfu_data()` to manually split column names
  after `tidyr::pivot_longer()`. 
  Using the built-in `names_sep` functionality of `pivot_longer()` 
  is not working for some data frames
  (despite working for all tests).
* Change column names to conform to new downloaded ILO
  files.
* Update column names to eliminate some "."s.
* No new tests.
    * Now at 452 tests, all passing.
    * Test coverage remains at 100%.


## MWTools 0.2.9 (2023-12-25) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10431628.svg)](https://doi.org/10.5281/zenodo.10431628)

* Fix lingering problems with the vignette
  on some testing platforms.
  For example, on Ubuntu, references in the `MWTools.Rmd` vignette failed because
  `.bib` file was named `references.bib` but the vignette
  looked for `References.bib`. 
  This problem was fixed by changing the name of the file to `References.bib`.
* No new tests.
    * Still at 453 tests, all passing.
    * Test coverage remains at 100%.


## MWTools 0.2.8 (2023-12-23) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10429640.svg)](https://doi.org/10.5281/zenodo.10429640)

* Fixing vignette.
* No new tests.
    * Still at 453 tests, all passing.
    * Test coverage remains at 100%.


## MWTools 0.2.7 (2023-12-23) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10427232.svg)](https://doi.org/10.5281/zenodo.10427232)

* Fixing vignette.
* No new tests.
    * Still at 453 tests, all passing.
    * Test coverage remains at 100%.


## MWTools 0.2.6 (2023-12-21) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10420270.svg)](https://doi.org/10.5281/zenodo.10420270)

* Added a statement of need to the README file.
* Added a vignette, moving some example code 
  out of the README file.
* No new tests.
    * Still at 453 tests, all passing.
    * Test coverage remains at 100%.


## MWTools 0.2.5 (2023-12-12) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10365505.svg)](https://doi.org/10.5281/zenodo.10365505)

* It appears there has been a change in the units column of the FAO data.
  "1000 Number" has been replaced by "An", 
  presumably indicating that previously the FAO had been reporting 
  thousands of animals but now they are reporting total numbers of animals.
  `tidy_fao_live_animals()` has been adjusted to accommodate this change.
* Two name changes and one reallocation of data relevant to `MWTools` occurred
  in the 2023 release of the FAO data retrieved via `FAOSTAT`. "Buffaloes" is 
  now incorporated in a combined "Cattle and Buffaloes" item. To fix this we
  calculate "Buffaloes" by subtracting "Cattle" from "Cattle and Buffaloes".
  The species "Mules" has been renamed "Mules and hinnies".
* Two tests added in `test-calc_amw_functions.R` to ensure these name changes 
  are flowing through the functions.
    * Now at 453 tests, all passing.
    * Test coverage remains at 100%.
  

## MWTools 0.2.4 (2023-12-10) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10342042.svg)](https://doi.org/10.5281/zenodo.10342042)

* Fixed errors in author list.
* Fixed GitHub pages deployment by adding package dependencies.
  Also, don't do `local::.. github::MatthewHeu/IEATools`, because
  the GitHub Action will look for a DESCRIPTION file in the parent directory!
  Instead, do `local::., github::MatthewHeu/IEATools`.
  Sigh! 
* No new tests.
    * Still at 451 tests, all passing.
    * Test coverage remains at 100%.


## MWTools 0.2.3 (2023-12-10) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10340976.svg)](https://doi.org/10.5281/zenodo.10340976)

* Added spell checks and fixed several spelling errors.
* Fixed pkgdown GitHub pages.
* No new tests.
    * Still at 451 tests, all passing.
    * Test coverage remains at 100%.


## MWTools 0.2.2 (2023-12-02) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10251505.svg)](https://doi.org/10.5281/zenodo.10251505)

* Added code of conduct and contributing pages to documentation.
* No new tests.
    * Still at 451 tests, all passing.
    * Test coverage remains at 100%.


## MWTools 0.2.1 (2023-08-20) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8266809.svg)](https://doi.org/10.5281/zenodo.8266809)

* Adding tests for `calc_amw_functions.R` to ensure 100% test coverage.
* Tests:
    * Now at 451 tests, all passing.
    * Test coverage is at 100%.


## MWTools 0.2.0 (2023-08-12) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8240992.svg)](https://doi.org/10.5281/zenodo.8240992)

* Add DOI in reference and citation places.
* Can now change between TJ and ktoe units
  via `output_unit` argument to `prep_psut()`.
  Default is TJ.
* Renamed `matrix.class` argument to `matrix_class`.
* Now works with `Matrix` objects.
* Split raw ILO data into separate working hours and employment objects, and
  added a function `prepareRawILOData()` to prepare this raw data.
* Tests:
    * Now at 447 tests, all passing.
    * Test coverage is at 99.17%.


## MWTools 0.1.2 (2023-01-30) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7584859.svg)](https://doi.org/10.5281/zenodo.7584859)

* Adding Zenodo badge


## MWTools 0.1.1 (2023-01-28)

* Responded to changes in `tidyselect`, 
  eliminating warnings from several `dplyr::all_of()` invocations.
* Defend against zero-row data frames,
  which can occur when there is no muscle work data.
  Now returning zero-row data frames
  to enable pipelines containing some countries without muscle work data.
* `prep_psut()` now returns a zero-row data frame
  if the inputs are zero-row data frames.
* New function `phi_vec_mw()` creates a phi vector 
  for use when converting energy to exergy.
* Now setting `NA` values to `0` in `specify_energy_type_method()`,
  to defend against energy balance errors later in the `prep_psut()` 
  function.
* New functions to create a data frame of PSUT matrices
  representing a muscle work energy conversion chain.
  See `prep_psut()` and friends.
* Functions now return uniform data frames with the following columns:
  AMW.Region.code (or HMW.Region.code), Country.code, Year, 
  Species (or Sex), Stage, Sector, and E.dot.


## MWTools 0.1.0

* Added a `NEWS.md` file to track changes to the package.
