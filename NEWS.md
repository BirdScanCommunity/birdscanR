# birdscanR (development version)

- New functionality: `compileData()` was added to compile a standardized MR1 dataset for publication to a data repository and allow easier interoperability (#44).
- New functionality: `createVPTS()` was added to create vpts csv output files in line with the ALOFT standard (#52).
- New functionality: the functions `getCollectionTable()` and `extractDbData()` now include an additional optional parameter `timeInterval` for when you want to limit the data being retrieved from the collection table (#49).
- New functionality: `extractDBData()` now includes an additional optional parameter `dbHost` to set your postgreSQL database host site (#54).
- New functionality: `getEchoFeatures()` now includes an additional optional parameter `echoSubset` for when you want to limit the data being retrieved from the `echo_rffeature_map` table to a certain subset of echoes only. In the function `extractDBData()`, the default is now to not extract the full `echo_rffeature_map` table anymore, but only for the echoes included in the `collection` table (which can now be subset based on a time interval). This improves performance for very large databases.
- Simplification: all `get` functions do not require the `dbDriverChar` argument.
- Improved documentation: README (#66), pkgdown website (#67) and styling (#70 and #74).
- Improved collaboration: Contributing guide, Code of conduct and license (#66), GitHub actions for R CMD check, pkgdown and styler (#67).

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
