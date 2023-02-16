## R CMD check results

0 errors \| 0 warnings \| 1 note

*   This is a new release.

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Resubmission

This is a resubmission. In this version I have:

*   Changed occurrences of SQL (without quotes) to 'SQL' (with single
    quotes) in the 'Description' file as well as in function titles
    throughout.

*   Expanded the description field in the 'Description' file to include
    more details on what the package does. 

*   Added 2 references to the description field of the 'Description' file that 
    describe the methods and underlying data.
    
*   Added `@return` statements to both ‘saveMTR.R’ and ‘savePlotToFile.R’, so 
    their .Rd versions include a `\value()`.

*   Added examples to the documentation of all of the package functions.

*   Added:

    `oldOptions = options()`

    `on.exit(options(oldOptions))`

    Prior to:

    `options(scipen = 999, digits = 9)`

    In:

    computeObservationTime.R
