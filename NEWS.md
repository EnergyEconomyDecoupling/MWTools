---
title: "Release notes for `MWTools`"
output: html_document
---


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
