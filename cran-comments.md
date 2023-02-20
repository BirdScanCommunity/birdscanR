## Resubmission

This is a resubmission. In this version I have:

*   Fixed unexecutable code in man/QUERY.Rd - A comma was missing between 2 
    parameters in the example function call.

*   Removed the examples of _computeObservationTime.R_ and _createTimeBins.R_ 
    as these are functions are not exported.
    
*   Also changed all instances of "MACHINE\\\\SERVERNAME" in the documentation 
    examples to "MACHINE\\SERVERNAME" as I noticed the additional escape 
    backslashes are added automatically to the .Rd output

## R CMD check results

0 errors \| 0 warnings \| 1 note

*   This is a new release.

## Downstream dependencies

There are currently no downstream dependencies for this package.
