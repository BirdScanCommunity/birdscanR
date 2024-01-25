# birdscanR 0.2.0.9002
* Added functionality: The function 'getBatClassification()' was added to get the data from the MR1 SQL tables 'bat_classification' and 'bat_class_probability'. The function 'extractDbData()' was updated to also extract the bat classification results using the prior function.
* Added functionality: The function 'reclassToBats()' was added to change the class of the echoes to 'bat' for those having a classification probability higher than a specified threshold.
* Added functionality: The 'feature37.speed' from the new Birdscan software v 1.7 is now also being extracted when calling the function 'getCollectionTable()'.

# birdscanR 0.2.0
* Added functionality: Add sample classification into crepuscularMorning, day, crepuscularNight, and night when extracting data. 
* Added functionality: Enable mtr calculation for day, crepuscule, and night instead of merely day/night only (both options available now).  
* Bug fixes: Small bug fixes in the two plotting functions.
* Bug fix: sunrisesunset information was being deleted for locations at higher latitudes because of the lack of the sunrising or setting. This has been fixed.
* Dependency fix: maptools package is retiring; Moved to suntools package which has the same functionality for twilight calculations (#34).

# birdscanR 0.1.1

* Added a `NEWS.md` file to track changes to the package.
