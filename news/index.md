# Changelog

## birdscanR (development version)

- New functionality:
  [`compileData()`](https://birdscancommunity.github.io/birdscanR/reference/compileData.md)
  was added to compile a standardized MR1 dataset for publication to a
  data repository and allow easier interoperability
  ([\#44](https://github.com/BirdScanCommunity/birdscanR/issues/44)).
- New functionality:
  [`filterSpeedFeature37()`](https://birdscancommunity.github.io/birdscanR/reference/filterSpeedFeature37.md)
  was added to filter unreasonable speed values created for animals
  flying through the radar beam for too short a time
  ([\#50](https://github.com/BirdScanCommunity/birdscanR/issues/50)).
- New functionality:
  [`createVPTS()`](https://birdscancommunity.github.io/birdscanR/reference/createVPTS.md)
  was added to create vpts csv output files in line with the ALOFT
  standard
  ([\#52](https://github.com/BirdScanCommunity/birdscanR/issues/52)).
- New functionality: the functions
  [`getCollectionTable()`](https://birdscancommunity.github.io/birdscanR/reference/getCollectionTable.md)
  and
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md)
  now include an additional optional parameter `timeInterval` for when
  you want to limit the data being retrieved from the collection table
  ([\#49](https://github.com/BirdScanCommunity/birdscanR/issues/49)).
- New functionality:
  [`getEchoFeatures()`](https://birdscancommunity.github.io/birdscanR/reference/getEchoFeatures.md)
  now includes an additional optional parameter `echoSubset` for when
  you want to limit the data being retrieved from the
  `echo_rffeature_map` table to a certain subset of echoes only. In the
  function `extractDBData()`, the default is now to not extract the full
  `echo_rffeature_map` table anymore, but only for the echoes included
  in the `collection` table (which can now be subset based on a time
  interval). This improves performance for very large databases ([commit
  78988c5](https://github.com/BirdScanCommunity/birdscanR/commit/78988c56f4d3b6849801f9f8afc6dd46b84b6a3d)).
- New functionality: Added a function to create a list of time ranges
  that can then be used for plotting
  ([\#78](https://github.com/BirdScanCommunity/birdscanR/issues/78)).
- Simplification: all `get` functions do not require the `dbDriverChar`
  argument
  ([\#72](https://github.com/BirdScanCommunity/birdscanR/issues/72)).
- Improved functionality: For
  [`getEchoFeatures()`](https://birdscancommunity.github.io/birdscanR/reference/getEchoFeatures.md)
  and
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md),
  `listOfRfFeaturesToExtract` can now also be set to ‘all’ to extract
  all features
  ([\#85](https://github.com/BirdScanCommunity/birdscanR/issues/85)).
- Improved documentation: README
  ([\#66](https://github.com/BirdScanCommunity/birdscanR/issues/66)),
  pkgdown website
  ([\#67](https://github.com/BirdScanCommunity/birdscanR/issues/67)),
  styling
  ([\#70](https://github.com/BirdScanCommunity/birdscanR/issues/70) and
  [\#74](https://github.com/BirdScanCommunity/birdscanR/issues/74)),
  updated ‘Getting Started’ vignette
  ([\#77](https://github.com/BirdScanCommunity/birdscanR/issues/77)) and
  additional vignette on extracting specific tables from the ‘Birdscan
  MR1’ ‘SQL’ database
  ([\#80](https://github.com/BirdScanCommunity/birdscanR/issues/80)).
- Improved collaboration: Contributing guide, Code of conduct and
  license
  ([\#66](https://github.com/BirdScanCommunity/birdscanR/issues/66)),
  GitHub actions for R CMD check, pkgdown and styler
  ([\#67](https://github.com/BirdScanCommunity/birdscanR/issues/67)).
- Bug Fix: Connections to postgreSQL servers were not working anymore.
  Fixed, and made database connection code cleaner by using a separate
  function
  [`dbConnectBirdscanSQL()`](https://birdscancommunity.github.io/birdscanR/reference/dbConnectBirdscanSQL.md)
  and file ([commit
  337a6be](https://github.com/BirdScanCommunity/birdscanR/commit/337a6be473178eb9f35d545e1c7bb164ad951f76)).
- Some smaller bug fixes:
  ([\#63](https://github.com/BirdScanCommunity/birdscanR/issues/63),
  [commit
  41288aa](https://github.com/BirdScanCommunity/birdscanR/commit/41288aa18898c95958372c5668f942b8ea9626a8)).

## birdscanR 0.3.0

CRAN release: 2024-07-05

- New functionality: the function
  [`computeDensity()`](https://birdscancommunity.github.io/birdscanR/reference/computeDensity.md)
  was added to calculate the density (expressed as \#objects / km3).
  Note that this function only works for Birdscan MR1 database versions
  \>= 1.7.0.4 as the variable feature37.speed is required for the
  density calculation
  ([\#40](https://github.com/BirdScanCommunity/birdscanR/issues/40)).
- New functionality: the function
  [`getBatClassification()`](https://birdscancommunity.github.io/birdscanR/reference/getBatClassification.md)
  was added to get the data from the MR1 SQL tables `bat_classification`
  and `bat_class_probability`. The function
  [`extractDbData()`](https://birdscancommunity.github.io/birdscanR/reference/extractDbData.md)
  was updated to also extract the bat classification results using the
  prior function
  ([\#36](https://github.com/BirdScanCommunity/birdscanR/issues/36)).
- New functionality: the function
  [`reclassToBats()`](https://birdscancommunity.github.io/birdscanR/reference/reclassToBats.md)
  was added to change the class of the echoes to `bat` for those having
  a classification probability higher than a specified threshold
  ([\#36](https://github.com/BirdScanCommunity/birdscanR/issues/36)).
- New functionality: the `feature37.speed` from the new Birdscan
  software v 1.7 is now also being extracted when calling the function
  [`getCollectionTable()`](https://birdscancommunity.github.io/birdscanR/reference/getCollectionTable.md)
  ([\#39](https://github.com/BirdScanCommunity/birdscanR/issues/39)).
- Bug fix: conditional
  `if (exists("mtrDayNight", envir = environment()))` caused bug in
  [`computeMTR()`](https://birdscancommunity.github.io/birdscanR/reference/computeMTR.md)
  function when variable was already defined in a user script (i.e.,
  outside of the package). This has been fixed
  ([\#38](https://github.com/BirdScanCommunity/birdscanR/issues/38)).

## birdscanR 0.2.0

CRAN release: 2023-10-06

- New functionality: add sample classification into crepuscularMorning,
  day, crepuscularNight, and night when extracting data.
- New functionality: enable mtr calculation for day, crepuscule, and
  night instead of merely day/night only (both options available now).
- Bug fixes: small bug fixes in the two plotting functions.
- Bug fix: sunrisesunset information was being deleted for locations at
  higher latitudes because of the lack of the sunrising or setting. This
  has been fixed.
- Dependency fix: maptools package is retiring; Moved to suntools
  package which has the same functionality for twilight calculations
  ([\#34](https://github.com/BirdScanCommunity/birdscanR/issues/34)).

## birdscanR 0.1.1

- Add a `NEWS.md` file to track changes to the package.
