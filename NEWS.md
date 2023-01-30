---
title: "Release notes for `MWTools`"
output: html_document
---

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
