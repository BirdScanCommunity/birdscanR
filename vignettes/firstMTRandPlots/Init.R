#### Init ------------------------------------------------------
#' @title Script for MR1Analysis MTR initialization
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}
#' @description 1- installation and loading of packages, 2- configuration of directories, 3- loading of functions, 4- creation of two data sets
#' @return featureNames A dataframe with two columns, feature and featurenames, as well as classAbbreviation : a dataframe with two columns, class and abbrev, for example passerine-type vs pa.
#' @export
#'
# Root directory
root <- getwd()

# Directories
codeDir <- file.path( root, "Code" )
functionDir <- file.path( codeDir, "Functions" )
manualBlindTimesDir <- file.path( codeDir, "manualBlindTimes" )
dataDir <- file.path( root, "Data" )
dbDataDir <- file.path( dataDir, "DB_Data" )
mtrDataDir <- file.path( dataDir, "MTR" )
plotDir <- file.path( dataDir, "Plots" )

# Files
manualBlindTimesFile <- file.path( manualBlindTimesDir, "manualBlindTimes.csv" )

# DataFramenames
echoDataName <- "echoData"
siteDataName <- "siteData"
visibilityDataName <- "visibilityData"
protocolDataName <- "protocolData"
timeBinDataName <- "timeBinData"
availableClassesName <- "availableClasses"
rfFeaturesName <- "rfFeatures"
classProbabilitiesAndMtrFactorsName <- "classProbabilitiesAndMtrFactors"

# Source function scripts
source( paste0( functionDir, "/ExtractDBData.R" ) )
source( paste0( functionDir, "/ConvertTimeZone.R" ) )
source( paste0( functionDir, "/ComputeMTR.R" ) )
source( paste0( functionDir, "/FilterData.R" ) )
source( paste0( functionDir, "/LoadManualBlindTimes.R" ) )
source( paste0( functionDir, "/AddDayNightInfoPerEcho.R" ) )
source( paste0( functionDir, "/Twilight.R" ) )
source( paste0( functionDir, "/CreateTimeBins.R" ) )
source( paste0( functionDir, "/CreateAltitudeBins.R" ) )
source( paste0( functionDir, "/MergeVisibilityAndManualBlindTimes.R" ) )
source( paste0( functionDir, "/ComputeObservationTime.R" ) )
source( paste0( functionDir, "/Plots.R" ) )
source( paste0( functionDir, "/SaveData.R" ) )

# Create Data Directories
ifelse( !dir.exists( dataDir ), dir.create( dataDir ), FALSE )
ifelse( !dir.exists( dbDataDir ), dir.create( dbDataDir ), FALSE )
ifelse( !dir.exists( mtrDataDir ), dir.create( mtrDataDir ), FALSE )
ifelse( !dir.exists( plotDir ), dir.create( plotDir ), FALSE )

# Feature name translation
featureNames <- data.frame( "feature" = c( "feature1"	
                                           ,"feature2"	
                                           ,"feature3"	
                                           ,"feature4"	
                                           ,"feature5"	
                                           ,"feature6"	
                                           ,"feature7"	
                                           ,"feature8"	
                                           ,"feature9"	
                                           ,"feature10"
                                           ,"feature11"
                                           ,"feature12"
                                           ,"feature13"
                                           ,"feature14"
                                           ,"feature15"
                                           ,"feature16"
                                           ,"feature17"
                                           ,"feature18"
                                           ,"feature19"
                                           ,"feature20"
                                           ,"feature21"
                                           ,"feature22"
                                           ,"feature23"
                                           ,"feature24"
                                           ,"feature25"
                                           ,"feature26"
                                           ,"feature27"
                                           ,"feature28"
                                           ,"feature29"
                                           ,"feature30"
                                           ,"feature31"
                                           ,"feature32"
                                           ,"feature33"
                                           ,"feature34"
                                           ,"feature35"
                                           ,"feature36"
                                           ,"feature37"
                                           ,"feature38"
                                           ,"feature39"
                                           ,"feature40"
                                           ,"feature41"
                                           ,"feature42"
                                           ,"feature43"
                                           ,"feature44"
                                           ,"feature45"
                                           ,"feature46"
                                           ,"feature47"
                                           ,"feature48"
                                           ,"feature49"
                                           ,"feature50"
), featureNames = c( "altitude_AGL" # Altitude above ground level (m)
                     ,"azimuth" # Azimuth (true north)
                     ,"speed" # Speed (m/s)
                     ,NA
                     ,NA
                     ,"rotationFreq" # Rotation frequency in relation to sampling frequency
                     ,NA
                     ,NA
                     ,NA
                     ,NA
                     ,NA
                     ,NA
                     ,"freqRatio" # Frequency Ratio
                     ,"maxLevel" # Maximum level
                     ,"polRatio" # Polarisation ratio
                     ,"absPolarisation" # Absolute polarisation
                     ,"rcs" # Radar cross section (m^2)
                     ,"sqrt(RCS)" # Square root of radar cross section (m)
                     ,"durationOfEcho" # Duration of echo (seconds)
                     ,"durationOfEchoInSTC" # Duration of echo in STC
                     ,NA
                     ,NA
                     ,NA
                     ,"alpha" # Alpha: |AlphaEnd – AlphaStart|
                     ,"theta" # Theta: 2⋅EpsilonCalc
                     ,NA
                     ,NA
                     ,NA
                     ,NA
                     ,"altitudeLeftSideOfEcho" # Altitude left side of echo (m)
                     ,"altitudeRightSideOfEcho" # Altitude right side of echo (m)
                     ,NA
                     ,"distLeftToBottom" # Distance between left side and bottom of echo (samples)
                     ,"nSamplesInEcho" # Length of echo (samples)
                     ,"areaOfEcho" # Area of echo (seconds multiplied by meter)
                     ,NA
                     ,NA
                     ,NA
                     ,NA
                     ,NA
                     ,NA
                     ,NA
                     ,NA
                     ,NA
                     ,NA
                     ,NA
                     ,NA
                     ,NA
                     ,NA
                     ,NA
) )

classAbbreviations <- data.frame( class = c( "passerine_type", "wader_type", "swift_type", "large_bird", "unid_bird", "bird_flock", "insect", "nonbio", "precipitation" ),
                                  abbr = c( "pa", "wa", "sw", "la", "bi", "fl", "in", "nb", "pr" ) )



