---
title: "Release notes for `MWTools`"
output: html_document
---

Cite all releases with doi [10.5281/zenodo.7584858](https://doi.org/10.5281/zenodo.7584858), 
which always resolves to the latest release.

# MWTools 0.2.0 (2023-08-12)

* Add DOI in reference and citation places.
* Can now change between TJ and ktoe units
  via `output_unit` argument to `prep_psut()`.
  Default is TJ.
* Renamed `matrix.class` argument to `matrix_class`.
* Now works with `Matrix` objects.
* Splitting raw ILO data into separate working hours and employment objects, and adding a function `prepareRawILOData` to prepare this raw data.


# MWTools 0.1.2 (2023-01-30)

* Adding Zenodo badge


# MWTools 0.1.1 (2023-01-28)

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


# MWTools 0.1.0

* Added a `NEWS.md` file to track changes to the package.
