# birdscanR 0.2.0.9006

-   Added functionality: The function 'computeDensity()' was added to calculate the density (expressed as #objects / km3). Note that this function only works for Birdscan MR1 database versions \>= 1.7.0.4 as the variable feature37.speed is required for the density calculation ([#40](https://github.com/BirdScanCommunity/birdscanR/issues/40){.uri}).
-   Added functionality: The function 'getBatClassification()' was added to get the data from the MR1 SQL tables 'bat_classification' and 'bat_class_probability'. The function 'extractDbData()' was updated to also extract the bat classification results using the prior function ([#36](https://github.com/BirdScanCommunity/birdscanR/issues/36){.uri}).
-   Added functionality: The function 'reclassToBats()' was added to change the class of the echoes to 'bat' for those having a classification probability higher than a specified threshold ([#36](https://github.com/BirdScanCommunity/birdscanR/issues/36){.uri}).
-   Added functionality: The 'feature37.speed' from the new Birdscan software v 1.7 is now also being extracted when calling the function 'getCollectionTable()' ([#39](https://github.com/BirdScanCommunity/birdscanR/issues/39){.uri}).
-   Bug fix: Conditional 'if (exists("mtrDayNight", envir = environment()))' caused bug in 'computeMTR()' function when variable was already defined in a user script (i.e., outside of the package). This has been fixed ([#38](https://github.com/BirdScanCommunity/birdscanR/issues/38){.uri}).

# birdscanR 0.2.0

-   Added functionality: Add sample classification into crepuscularMorning, day, crepuscularNight, and night when extracting data.
-   Added functionality: Enable mtr calculation for day, crepuscule, and night instead of merely day/night only (both options available now).\
-   Bug fixes: Small bug fixes in the two plotting functions.
-   Bug fix: sunrisesunset information was being deleted for locations at higher latitudes because of the lack of the sunrising or setting. This has been fixed.
-   Dependency fix: maptools package is retiring; Moved to suntools package which has the same functionality for twilight calculations (#34).

# birdscanR 0.1.1

-   Added a `NEWS.md` file to track changes to the package.
