
#### AnalyzeData ------------------------------------------------------
#' @title AnalyzeData
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}
#' @description This script calls all the functions in the MR1Analysis and serves as an example/start to use the functions and analyse your BirdScan MR1 data.
#' @return MTR tables and several plots
#' @export
#' 

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ---------------------------Load Settings------------------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

root <- getwd()
source( file.path( root, "Code", "Init.R" ) )

if( TRUE )
{
  
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ------------------ Load Data from database or file -------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  dbServer <- "[your-SQL-server-name]"
  dbName <- "[your-database-name]" 
  dbDriver <- "[your-database-driver]"
  
  # most recent timestamp for which data is loaded
  timing <- Sys.time()
  attr(timing,"tzone") <- 'UTC'
  timing <- as.character(timing)
  
  # Force Data to be extracted from DB
  # if set to 'FALSE', data will anyway be extracted from the DB if data is not present yet.
  forceToExtractDataFromDatabase = FALSE
  
  # list of RF Feature ID's to extract. Feature ID's can be found in rffeatures table 
  # or in [data]$rfFeatures once data was extracted from DB
  # example to get wing beat frequency and credibility: c( 167, 168 )
  listOfRfFeaturesToExtract <- c( 167, 168 )
  
  # set timezones used by the radar and the target timezone
  # Example: targetTimeZone <- "Etc/GMT-1" 
  # --> Etc/GMT-1 equals UTC+1
  # use "Etc/GMT0" (UTC) as radarTimeZone for birdscan v1.6 and greater. 
  # birdscan v1.6 and greater stores all times as UTC.
  radarTimeZone <- "Etc/GMT0"
  targetTimeZone <- "Etc/GMT-1"
  
  message( "Loading data" )
  data <- extractDbData( dbDriver = dbDriver,
                         dbServer = dbServer, 
                         dbName = dbName, 
                         radarTimeZone = radarTimeZone,
                         targetTimeZone = targetTimeZone,
                         forceToExtractDataFromDatabase = forceToExtractDataFromDatabase, 
                         listOfRfFeaturesToExtract = listOfRfFeaturesToExtract )

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# --------------------------- Sunrise/Sunset-- -------------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  # coordinates of site location (Latitude, Longitude )
  # example: siteLocation <- c( 47.494427, 8.716432 )
  siteLocation <- c( 47.494427, 8.716432 )
  
  # set min and max time_stamps of echodata as timerange for sunrise/sunset calculation
  timeRangeSunriseSunset <- c( min( data$echoData$time_stamp_targetTZ ), 
                               max( data$echoData$time_stamp_targetTZ ) )
  
  message( "computing sunrise/sunset and twilight" )
  
  # compute sunrise/sunset and dawn/dusk (civil)
  sunriseSunset <- twilight( timeRange = timeRangeSunriseSunset, 
                             latLon = siteLocation, 
                             timeZone = targetTimeZone )

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ---------------- Add day/night information to echoData ---------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  sunOrCivil = "civil"
  
  data$echoData <- addDayNightInfoPerEcho( echoData = data$echoData,
                                           sunriseSunset = sunriseSunset, 
                                           sunOrCivil = sunOrCivil )
}
  
if( TRUE )
{
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ------------------------ Filter Protocols ----------------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  # operation modes: pulselength (multiple selections possible)
  # options: S, M, L
  # example: pulseLengthSelection <- c( "S", "L" )
  pulseLengthSelection <- c( "S" )
  
  # operation modes: rotation (multiple selections possible)
  # options: 1 (rotation), 0 (nonrotation)
  # example: rotationSelection <- c( 1, 0 )
  rotationSelection <- c( 1, 0 )
  
  # subset protocolData by operation mode
  message( "filtering protocol data")
  protocolDataSubset <- filterProtocolData( protocolData = data$protocolData, 
                                            pulseTypeSelection = pulseLengthSelection, 
                                            rotationSelection = rotationSelection )

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ----------------------- Compute BlindTimes ---------------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  message( "loading and computing blindtimes")
  
  # read manual blindtimes from csv File (Filepath and -name: 'manualBlindTimesFile' defined in 'Init.R' )
  # csv contains one row per blindtime and 3 columns (start of blindtime, stop of blindtime, type of blindtime)
  # times have to be of format 'yyyy-MM-dd hh:mm:ss'
  manualBlindTimes <- loadManualBlindTimes( filePath = manualBlindTimesFile,
                                            blindTimesTZ = radarTimeZone,
                                            targetTZ = targetTimeZone )
  
  # compute blindtimes
  blindTimes <- mergeVisibilityAndManualBlindTimes( visibilityData = data$visibilityData, 
                                                    manualBlindTimes = manualBlindTimes, 
                                                    protocolData = protocolDataSubset )

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# --------------------------- Filter Echoes ----------------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  # classes used for analysis
  # Classes available listed in [data]$availableClasses
  # example: classSelection <- c( "passerine_type", "wader_type", "large_bird" )
  classSelection_allBirds <- c( "passerine_type", "wader_type", "swift_type", "large_bird", "unid_bird", "bird_flock" )
  classSelection_all <- c( "passerine_type", "wader_type", "swift_type", "large_bird", "unid_bird", "bird_flock", "insect", "nonbio", "precipitation" )
  
  # Classification Propability cutoff (0..1)
  classProbCutoff <- 0.3
  
  # altitude range
  altitudeRange_AGL_25_5000 <- c( 25, 5000 )
  
  # time range for echodata (targetTimeZone) 
  # use format "yyyy-MM-dd hh:mm"
  timeRangeEchoData <- c( "2020-01-01 00:00", "2021-01-01 00:00" )
  timeRangeEchoData <- as.POSIXct( timeRangeEchoData, 
                                   format = "%Y-%m-%d %H:%M", 
                                   tz = targetTimeZone )
  
  message( "filtering echodata" )
  
  # subset echodata
  echoDataSubset_allBirds_25_5000 <- filterEchoData( echoData = data$echoData,
                                                     timeRangeTargetTZ = timeRangeEchoData, 
                                                     protocolData = protocolDataSubset, 
                                                     classSelection = classSelection_allBirds,
                                                     classProbCutOff = classProbCutoff,
                                                     altitudeRange_AGL = altitudeRange_AGL_25_5000,
                                                     manualBlindTimes = manualBlindTimes,
                                                     echoValidator = TRUE )
  echoDataSubset_all_25_5000 <- filterEchoData( echoData = data$echoData,
                                                timeRangeTargetTZ = timeRangeEchoData, 
                                                protocolData = protocolDataSubset, 
                                                classSelection = classSelection_all,
                                                classProbCutOff = classProbCutoff,
                                                altitudeRange_AGL = altitudeRange_AGL_25_5000,
                                                manualBlindTimes = manualBlindTimes,
                                                echoValidator = TRUE )
}

if( FALSE )
{
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# --------------------- Create Altitude Bins ---------------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # Altitude Range and Bin Size
  altitudeRange_AGL_25_1025 <- c( 25, 1025 )
  
  # create altitudeBins
  message( "creating altitude bins" )
  altitudeBins_25_1025_oneBin <- createAltitudeBins( altitudeRange = altitudeRange_AGL_25_1025, 
                                                     altitudeBinSize = 1000 )
  altitudeBins_25_1025_binSize50 <- createAltitudeBins( altitudeRange = altitudeRange_AGL_25_1025, 
                                                        altitudeBinSize = 50 )
  
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ----------------------- Create Time Bins -----------------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # time range for timeBins (targetTimeZone) 
  # use format "yyyy-MM-dd hh:mm"
  timeRangeTimeBins <- c( "2020-03-01 00:00", "2020-11-01 00:00" )
  timeRangeTimeBins <- as.POSIXct( timeRangeTimeBins, 
                                   format = "%Y-%m-%d %H:%M", 
                                   tz = targetTimeZone )
  
  # timeBin size in seconds
  timeBinduration_sec <- 3600
  
  # create Timebins
  message( "creating time bins" )
  timeBins_1h <- createTimeBins( timeRange = timeRangeTimeBins, 
                                 timeBinDuration_sec = timeBinduration_sec, 
                                 timeZone = targetTimeZone, 
                                 sunriseSunset = sunriseSunset, 
                                 sunOrCivil = sunOrCivil )
   
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ----------- Compute Observation Time for each Time Bin ---------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # set blindtime types which should not be treated as blindtime but MTR = 0
  blindTimeAsMtrZero <- c( "rain" )
  
  # compute observation times
  message( "computing observation times" )
    timeBins_1h <- computeObservationTime( timeBins = timeBins_1h, 
                                         protocolData = protocolDataSubset, 
                                         blindTimes = blindTimes, 
                                         blindTimeAsMtrZero = blindTimeAsMtrZero )
   
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# -------------------------- Compute MTR -------------------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # cutoff for proportional observation times: Ignore TimeBins where 
  # "observationTime/timeBinDuration < propObsTimeCutoff"
  # in 'computeMTR' only used if parameter 'computePerDayNight' is set to
  # 'TRUE' and timeBins are shorter than day/night. In this case timeBins 
  # are combined by day/night and only time bins with a proportional 
  # observation time greater than the cutoff will be used to compute the 
  # day/night MTR and spread.
  # set value from 0-1
  propObsTimeCutoff <- 0.2
  
    # MTR per day/night, one altitude bin 25m to 1025m
  message( "computing MTR per day/night, 25m to 1025m" )
  mtr_DayNight_25mto1025m <- computeMTR( echoes = echoDataSubset_allBirds_25_5000, 
                                         classSelection = classSelection_allBirds, 
                                         altitudeBins = altitudeBins_25_1025_oneBin, 
                                         timeBins = timeBins_1h, 
                                         propObsTimeCutoff = propObsTimeCutoff, 
                                         computePerDayNight = TRUE )
  saveMTR( mtr = mtr_DayNight_25mto1025m, 
           filepath = mtrDataDir, 
           dbName = dbName, 
           rotSelection = rotationSelection, 
           pulseTypeSelection = pulseLengthSelection )
  
  # MTR per hour, one altitude bin 25m to 1025m
  message( "computing MTR per hour, 25m to 1025m" )
  mtr_1h_25mto1025m <- computeMTR( echoes = echoDataSubset_allBirds_25_5000, 
                                   classSelection = classSelection_allBirds, 
                                   altitudeBins = altitudeBins_25_1025_oneBin, 
                                   timeBins = timeBins_1h, 
                                   propObsTimeCutoff = propObsTimeCutoff, 
                                   computePerDayNight = FALSE )
  saveMTR( mtr = mtr_1h_25mto1025m, 
           filepath = mtrDataDir, 
           dbName = dbName, 
           rotSelection = rotationSelection, 
           pulseTypeSelection = pulseLengthSelection )
  
  # MTR per day/night, altitude bins of size 50m from 25m to 1025m
  message( "computing MTR per day/night, 25m to 1025m, 50m altitude bins" )
  mtr_DayNight_25mto1025m_50m <- computeMTR( echoes = echoDataSubset_allBirds_25_5000, 
                                             classSelection = classSelection_allBirds, 
                                             altitudeBins = altitudeBins_25_1025_binSize50, 
                                             timeBins = timeBins_1h, 
                                             propObsTimeCutoff = propObsTimeCutoff, 
                                             computePerDayNight = TRUE )
  saveMTR( mtr = mtr_DayNight_25mto1025m_50m, 
           filepath = mtrDataDir, 
           dbName = dbName, 
           rotSelection = rotationSelection, 
           pulseTypeSelection = pulseLengthSelection )
  
  # MTR per hour, altitude bins of size 50m from 25m to 1025m
  message( "computing MTR per hour, 25m to 1025m, 50m altitude bins" )
  mtr_1h_25mto1025m_50m <- computeMTR( echoes = echoDataSubset_allBirds_25_5000, 
                                       classSelection = classSelection_allBirds, 
                                       altitudeBins = altitudeBins_25_1025_binSize50, 
                                       timeBins = timeBins_1h, 
                                       propObsTimeCutoff = propObsTimeCutoff, 
                                       computePerDayNight = FALSE )
  saveMTR( mtr = mtr_1h_25mto1025m_50m, 
           filepath = mtrDataDir, 
           dbName = dbName, 
           rotSelection = rotationSelection, 
           pulseTypeSelection = pulseLengthSelection )
}

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ----------------------------- Plot -----------------------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# time range for plots (targetTimeZone) 
# use format "yyyy-MM-dd hh:mm"
timeRangePlot <- list( as.POSIXct( c( "2020-03-01 00:00", "2020-04-01 00:00" ), 
                                   format = "%Y-%m-%d %H:%M", tz = targetTimeZone ),
                       as.POSIXct( c( "2020-04-01 00:00", "2020-05-01 00:00" ), 
                                   format = "%Y-%m-%d %H:%M", tz = targetTimeZone ),
                       as.POSIXct( c( "2020-05-01 00:00", "2020-06-01 00:00" ), 
                                   format = "%Y-%m-%d %H:%M", tz = targetTimeZone ),
                       as.POSIXct( c( "2020-03-01 00:00", "2020-11-01 00:00" ),  
                                   format = "%Y-%m-%d %H:%M", tz = targetTimeZone ))

# plot longitudinal MTR per day and night
if( FALSE )
{
  message( "plot longitudinal MTR (all classes in MTR data)" )
  plotLongitudinalMTR( mtr = mtr_DayNight_25mto1025m, 
                       maxMTR = -1, 
                       timeRange = timeRangePlot, 
                       plotClass = "allClasses",
                       propObsTimeCutoff = propObsTimeCutoff,
                       plotSpread = TRUE, 
                       filePath = plotDir ) 
  message( "plot longitudinal MTR (passerine_type)" )
  plotLongitudinalMTR( mtr = mtr_DayNight_25mto1025m, 
                       maxMTR = -1, 
                       timeRange = timeRangePlot, 
                       plotClass = "passerine_type",
                       propObsTimeCutoff = propObsTimeCutoff, 
                       plotSpread = TRUE, 
                       filePath = plotDir ) 
}

# Exploration plot with all classes up to 5000m.
if( FALSE )
{
  message( "plot exploration" )
  plotExploration( echoData = echoDataSubset_all_25_5000, 
                   timeRange = timeRangePlot, 
                   manualBlindTimes = manualBlindTimes, 
                   visibilityData = data$visibilityData, 
                   protocolData = protocolDataSubset, 
                   sunriseSunset = sunriseSunset, 
                   maxAltitude = -1, 
                   filePath = plotDir )
}

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ------------- Load MTR data from files or compute MTR data -----------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

if( FALSE )
{
  # MTR per day/night, 25m to 1025m
  fileName <- rstudioapi::selectFile( caption = "Select MTR data with one altitude bin and day/night timebins",
                                      filter = "rds Files (*.rds)",
                                      path = mtrDataDir,
                                      existing = TRUE )
  mtr_DayNight_25mto1025m <- readRDS( fileName )
  
  # MTR per hour, one altitude bin 25m to 1025m
  fileName <- rstudioapi::selectFile( caption = "Select MTR data with one altitude bin and 1h timebins",
                                      filter = "rds Files (*.rds)",
                                      path = mtrDataDir,
                                      existing = TRUE )
  mtr_1h_25mto1025m <- readRDS( fileName )

  # MTR per day/night, altitude bins of size 50m from 25m to 1025m
  fileName <- rstudioapi::selectFile( caption = "Select MTR data with multiple altitude bins and day/night timebins",
                                      filter = "rds Files (*.rds)",
                                      path = mtrDataDir,
                                      existing = TRUE )
  mtr_DayNight_25mto1025m_50m <- readRDS( fileName )
  
  # MTR per hour, altitude bins of size 50m from 25m to 1025m
  fileName <- rstudioapi::selectFile( caption = "Select MTR data with multiple altitude bins and 1h timebins",
                                      filter = "rds Files (*.rds)",
                                      path = mtrDataDir,
                                      existing = TRUE )
  mtr_1h_25mto1025m_50m <- readRDS( fileName )
} 
