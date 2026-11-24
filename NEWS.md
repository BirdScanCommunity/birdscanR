# birdscanR (development version)

- Breaking change: birdscanR now requires R >= 4.2.0 (was R >= 4.1.0), because current versions of its dependency ggplot2 no longer support R 4.1.
- New functionality: `filterSpeedFeature37()` was added to filter unreasonable speed values created for animals flying through the radar beam for too short a time (#50).
- New functionality: `createVPTS()` was added to create vpts csv output files in line with the ALOFT standard (#52).
- New functionality: the functions `getCollectionTable()` and `extractDbData()` now include an additional optional parameter `timeInterval` for when you want to limit the data being retrieved from the collection table (#49).
- New functionality: `getEchoFeatures()` now includes an additional optional parameter `echoSubset` for when you want to limit the data being retrieved from the `echo_rffeature_map` table to a certain subset of echoes only. In the function `extractDBData()`, the default is now to not extract the full `echo_rffeature_map` table anymore, but only for the echoes included in the `collection` table (which can now be subset based on a time interval). This improves performance for very large databases ([commit 78988c5](https://github.com/BirdScanCommunity/birdscanR/commit/78988c56f4d3b6849801f9f8afc6dd46b84b6a3d)).
- New functionality: Added a function to create a list of time ranges that can then be used for plotting (#78).
- New functionality: `createDataPackage()` was added to create a standardised MR1 data package of a radar campaign (#44), following the Frictionless Data Package standard, for publication on Zenodo. The package holds the echo, protocol, blind time, sunrise/sunset and site data, with a schema per table (column types, descriptions, units, primary and foreign keys) and package-level metadata (title, owner, licence, embargo, contributors with ORCID and DataCite roles, related identifiers, suggested citation, spatial and temporal coverage, applied filters and provenance).
- New functionality: `createDataPackageMetadataTemplate()` was added to write a YAML template for the package-level metadata used by `createDataPackage()`.
- New functionality: `validateDataPackage()` was added to check an MR1 data package, as object or on disk, before publication: package metadata, presence and readability of the resources, required columns, column names and types against the schema, primary and foreign keys, and plausibility of the content (start before stop times, echoes within their measurement period and the temporal coverage, site coordinates).
- New functionality: `uploadDataPackageToZenodo()` was added to upload a validated MR1 data package as a draft record to Zenodo (or the Zenodo sandbox), with a reserved DOI that is written into the package, and with the Zenodo metadata (creators, contributor roles, licence, embargo, keywords, related identifiers, location, period, BirdScan MR1 community) taken from `datapackage.json` (#59). It requires the suggested package `httr2`. The record is never published by the function.
- Improved functionality: `createDataPackageMetadataTemplate()` now includes the optional field `id` for the DOI of the dataset.
- New functionality: `uploadDataPackageToZenodo()` now creates a draft of a new version of an existing Zenodo record when the data package has the DOI of that record as `id` (new argument `newVersion`). The files of the previous version are replaced, and its related identifiers (e.g. links to collections) are kept.
- New functionality: `addZenodoRelatedIdentifiers()` was added to add related identifiers (e.g. `IsPartOf` links to collections) to the metadata of a Zenodo record without creating a new version. The edit is published only with `publish = TRUE`.
- Improved functionality: `createDataPackage()` now only includes blind times that overlap a selected measurement period (protocol); before, blind times of protocols that were filtered out (e.g. by `pulseTypeSelection`) were included without `protocolID`.
- Improved documentation: new vignette 'Create and publish an MR1 data package'.
- Changed sample data: the example manual blind times (`data(manualBlindTimes)`) now fall within the period of the sample data extract (Sempach, 24-25 September 2024), so they can be used together in examples.
- Simplification: all `get` functions do not require the `dbDriverChar` argument (#72).
- Improved functionality: For `getEchoFeatures()` and `extractDbData()`, `listOfRfFeaturesToExtract` can now also be set to 'all' to extract all features (#85).
- Improved documentation: README (#66), pkgdown website (#67), styling (#70 and #74), updated 'Getting Started' vignette (#77) and additional vignette on extracting specific tables from the 'Birdscan MR1' 'SQL' database (#80).
- Improved collaboration: Contributing guide, Code of conduct and license (#66), GitHub actions for R CMD check, pkgdown and styler (#67).
- Bug Fix: Connections to postgreSQL servers were not working anymore. Fixed, and made database connection code cleaner by using a separate function `dbConnectBirdscanSQL()` and file ([commit 337a6be](https://github.com/BirdScanCommunity/birdscanR/commit/337a6be473178eb9f35d545e1c7bb164ad951f76)).
- Bug Fix: `mergeVisibilityAndManualBlindTimes()` warned about overlapping manual blind times whenever more than one manual blind time was given, also when they did not overlap.
- Bug Fix: the documentation of `loadManualBlindTimes()` suggested writing the example blind times with `write.csv()`, which adds a header row that is then read as a blind time.
- Some smaller bug fixes: (#63, [commit 41288aa](https://github.com/BirdScanCommunity/birdscanR/commit/41288aa18898c95958372c5668f942b8ea9626a8)). 

# birdscanR 0.3.0

- New functionality: the function `computeDensity()` was added to calculate the density (expressed as #objects / km3). Note that this function only works for Birdscan MR1 database versions >= 1.7.0.4 as the variable feature37.speed is required for the density calculation (#40).
- New functionality: the function `getBatClassification()` was added to get the data from the MR1 SQL tables `bat_classification` and `bat_class_probability`. The function `extractDbData()` was updated to also extract the bat classification results using the prior function (#36).
- New functionality: the function `reclassToBats()` was added to change the class of the echoes to `bat` for those having a classification probability higher than a specified threshold (#36).
- New functionality: the `feature37.speed` from the new Birdscan software v 1.7 is now also being extracted when calling the function `getCollectionTable()` (#39).
- Bug fix: conditional `if (exists("mtrDayNight", envir = environment()))` caused bug in `computeMTR()` function when variable was already defined in a user script (i.e., outside of the package). This has been fixed (#38).

# birdscanR 0.2.0

- New functionality: add sample classification into crepuscularMorning, day, crepuscularNight, and night when extracting data.
- New functionality: enable mtr calculation for day, crepuscule, and night instead of merely day/night only (both options available now).
- Bug fixes: small bug fixes in the two plotting functions.
- Bug fix: sunrisesunset information was being deleted for locations at higher latitudes because of the lack of the sunrising or setting. This has been fixed.
- Dependency fix: maptools package is retiring; Moved to suntools package which has the same functionality for twilight calculations (#34).

# birdscanR 0.1.1

- Add a `NEWS.md` file to track changes to the package.
