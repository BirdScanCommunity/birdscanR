#### mainMiE_MTRCalculation ===================================================
#' @title mainMiE_MTRCalculation
#' @author Birgen Haest, \email{birgen.haest@@vogelwarte.ch}; based on template by Fabien Hertner \email{fabian.hertner@@swiss-birdradar.com}
#' @description This is the main script for the calculation of the insect MTRs for all MiE Birdscans
#' @return MTR tables for each MiE study site
# =============================================================================
# Package requirements - Most of these will probably be loaded with birdscanR
#                        NEED TO CHECK WHAT CAN BE FURTHER EXCLUDED
# =============================================================================
  library(birdscanR)

# =============================================================================
# = ------------------ SET WHAT YOU WANT TO DO ------------------------------ =
# =============================================================================
  getDBData               = TRUE # Set to TRUE if you wish to extract data from a birdscan sql database
  getSunInfo              = TRUE # Set to TRUE if you want to get the sunrise/sunset information
  addDayNightInfoToEchoes = TRUE # Set to TRUE if you want to add day/night info for each echo
  filterProtocols         = TRUE # Set to TRUE to filter the protocol data 
  computeBlindTimes       = TRUE # Set to TRUE to compute the times in which the radar was blind
  filterEchoes            = TRUE # Set to TRUE to filter the echoes
  calculateMTR            = TRUE # Set to TRUE to calculate MTRs
  plotMTRs                = TRUE # Set to TRUE to create some explorative plots of the MTRs 
  
# =============================================================================
# = ----------------------- INPUT SETTINGS ---------------------------------- =
# =============================================================================
  # Set main output directory ----------------------------------------------- =
  # ===========================================================================
    mainOutputDir = file.path(".", "results")
    
  # getDBData - Input Settings ---------------------------------------------- =
  # ===========================================================================
    if (getDBData){
      # Set server and database settings
        dbServer       = "NB_BHA2019\\SQLEXPRESS"
        dbNames        = c("db_20210106_Winterthur") # Data from each sql databases in  
                                                     # this vector will be extracted
                                                     # sequentially 
        dbDriverChar   = "SQL Server"                # Set either "SQL Server" or "PostgreSQL"
      
      # Set output directory to save the extracted databased data to
        dbOutputDir    = file.path(mainOutputDir, "DBData")   
      
      # Set timezones used by each radar/database 
        radarTimeZones.gd  = list(db_20210106_Winterthur = "Etc/GMT0")
      
      # Set target timezones for each radar/dataset
      # Example: targetTimeZone = "Etc/GMT-1" (Etc/GMT-1 equals UTC+1)
      # use "Etc/GMT0" (UTC) as radarTimeZone for birdscan v1.6 
      # and greater as all times are in UTC as of this software version.
        targetTimeZones.gd = list(db_20210106_Winterthur = "Etc/GMT0")
      
      # Set list of Rf features you also want to extract
      # Vector with RF features to extract. Feature IDs can be found in the  
      # rffeatures table in the sql database.  
      # Example: Get wing beat frequency and credibility: c(167, 168)
      # Set to NULL to not extract any.
        listOfRfFeaturesToExtract = c(167,168) 
    }
    
  # getSunInfo - Input Settings --------------------------------------------- =
  # ===========================================================================
    if (getSunInfo){
      # Set paths to extracted db datafiles
        dbFiles.sun = list(db_20210106_Winterthur = file.path(mainOutputDir, "DBData", 
                                                             "db_20210106_Winterthur_DataExtract.rds"))
      # Locations of databases - list of dbName = c(Latitude, Longitude)
        siteLocations = list(db_20210106_Winterthur = c(47.494427, 8.716432))
        
      # Set target timezones
        targetTimeZones.sun = list(db_20210106_Winterthur = "Etc/GMT0")
        
      # Set the output directory to save the sunrise/sunset information to
        outputDirSunInfo = file.path(mainOutputDir, "SunData")
    }
    
  # addDayNightInfoToEchoes - Input Settings --------------------------------- =
  # ===========================================================================
    if (addDayNightInfoToEchoes){
      # Set paths to extracted db datafiles
        dbFiles.adn = list(db_20210106_Winterthur = file.path(mainOutputDir, "DBData", 
                                                              "db_20210106_Winterthur_DataExtract.rds"))
        
      # Set paths to sun information files
        sunFiles    = list(db_20210106_Winterthur = file.path(mainOutputDir, "SunData", 
                                                              "db_20210106_Winterthur_sunInfo.rds"))
        
      # Set type of twilight to use for day/night decision
        sunOrCivil = "civil" 
        
      # Set whether to save the updated echoData to the dbFiles
        saveToDB.adn = TRUE
    }
  
  # filterProtocols - Input Settings ---------------------------------------- =
  # ===========================================================================
    if (filterProtocols){
      # Set paths to extracted db datafiles
        dbFiles.fp = list(db_20210106_Winterthur = file.path(mainOutputDir, "DBData", 
                                                             "db_20210106_Winterthur_DataExtract.rds"))
      
      # Set desired pulseLength modes (multiple simultaneous selections possible)
      # options: S, M, L
        pulseLengthSelection = c("S")
      
      # Set desired rotation modes (multiple simultaneous selections possible)
      # options: 1 (rotation), 0 (nonrotation)
        rotationSelection = c(1)
        
      # Set whether to save the updated protocolData to the dbFiles
        saveToDB.fp = TRUE
    }
  
  # computeBlindTimes - Input Settings -------------------------------------- =
  # ===========================================================================
    if (computeBlindTimes){
      # Set paths to extracted db datafiles
        dbFiles.cbt = list(db_20210106_Winterthur = file.path(mainOutputDir, "DBData", 
                                                              "db_20210106_Winterthur_DataExtract.rds"))
        
      # Set whether to get the manual blind time from:
      # "mandb": the db file;  
      # "csv"  : a separate csv file 
        manBlindSource = "csv"
        
      # Set paths to manual blind times files, if manBlindSource == "csv"
        if (manBlindSource %in% "csv"){
          manblindFiles = list(db_20210106_Winterthur = file.path("data",
                                                                  "db_20210106_Winterthur_manualBlindTimes.csv"))
        }
        
      # Set timezones used by each radar/database 
        radarTimeZones.cbt  = list(db_20210106_Winterthur = "Etc/GMT0")
      
      # Set target timezones for each radar/dataset
        targetTimeZones.cbt = list(db_20210106_Winterthur = "Etc/GMT0")
        
      # Set whether to save the blind times to file
        saveBlindTimes = TRUE
        
      # Set the output directory to save the sunrise/sunset information to
        outputDirBlindTimes = file.path(mainOutputDir, "blindTimes")
    }
  
  # filterEchoes - Input Settings ------------------------------------------- =
  # ===========================================================================
    if (filterEchoes){
      # Set paths to extracted db datafiles
        dbFiles.fe = list(db_20210106_Winterthur = file.path(mainOutputDir, "DBData", 
                                                             "db_20210106_Winterthur_DataExtract.rds"))
        
      # Set classes to use in the analysis
      # Example: classSelection = c("passerine_type", "wader_type", "large_bird")
        # classSelection = c("passerine_type", "wader_type", "swift_type", 
        #                    "large_bird", "unid_bird", "bird_flock", 
        #                    "insect", "nonbio", "precipitation")
        classSelection = c("insect")
      
      # Set the classification propability cutoff (0..1) - only objects for which 
      # the chosen label also has a classification probability > than the cutoff
      # are retained; Set to NULL to not subset according to probability.
        classProbCutoff = NULL
      
      # Set the altitude range
        altitudeRange_AGL.fe = c(25, 5000)
      
      # Set timezones used by each radar/database 
        radarTimeZones.fe  = list(db_20210106_Winterthur = "Etc/GMT0")
        
      # Set target timezones for each radar/dataset
        targetTimeZones.fe = list(db_20210106_Winterthur = "Etc/GMT0")
        
      # Set the time range for echodata (in the targetTimeZone) 
      # use format "yyyy-MM-dd hh:mm"
        timeRangesEchoData = list(db_20210106_Winterthur = c("2021-01-15 00:00", "2021-01-31 00:00"))
      
      # Set whether to get the manual blind time from:
      # "mandb": the db file;  
      # "csv"  : a separate csv file 
        manBlindSource = "csv"
        
      # Set paths to manual blind times files, if manBlindSource == "csv"
        if (manBlindSource %in% "csv"){
          manblindFiles = list(db_20210106_Winterthur = file.path("data",
                                                                  "db_20210106_Winterthur_manualBlindTimes.csv"))
        }
        
      # Set whether to use the echoValidator - If set to TRUE, echoes labelled 
      # by the echo validator as “non-bio scatterer” will be excluded.
        useEchoValidator = FALSE  
        
      # Set the output directory to save the filtered dataset to 
        outputDir.fe = file.path(mainOutputDir, "DBData_filtered")
    }
  
  # calculateMTR - Input Settings ------------------------------------------- =
  # ===========================================================================
    if (calculateMTR){
      # Set paths to (filtered) db datafiles for which you wish to calculate the 
      # MTR
        dbFiles.mtr = list(db_20210106_Winterthur = 
                             file.path(mainOutputDir, "DBData_filtered",  "db_20210106_Winterthur_filtered_cut0_altRange25to5000_timeRange20210115to20210131_echoValFALSE.rds"))
      
      # Set altitude Range and Bin Size for the MTR calculations
        altitudeRange_AGL.mtr = c(25, 1025)
        altitudeBinSize       = 50
      
      # Set target timezones for each radar/dataset
        targetTimeZones.mtr = list(db_20210106_Winterthur = "Etc/GMT0")
          
      # time range for timeBins (targetTimeZone) - format: "yyyy-MM-dd hh:mm"
        timeRangesTimeBins  = list(db_20210106_Winterthur = c("2021-01-15 00:00", "2021-01-31 00:00"))
      
      # timeBin size in seconds
        timeBinduration_sec = 3600
        
      # Set paths to sun information files
        sunFiles    = list(db_20210106_Winterthur = file.path(mainOutputDir, "SunData", 
                                                              "db_20210106_Winterthur_sunInfo.rds"))
        
      # Set type of twilight to use for day/night decision
        sunOrCivil  = "civil"
        
      # Set paths to blindtimes files
        blindFiles  = list(db_20210106_Winterthur = file.path(mainOutputDir, "blindTimes", 
                                                              "db_20210106_Winterthur_overallBlindTimes.rds"))
        
      # set blindtime types which should not be treated as blindtime but MTR = 0
        blindTimeAsMtrZero = c("rain")
        
      # cutoff for proportional observation times: Ignore TimeBins where 
      # "observationTime/timeBinDuration < propObsTimeCutoff"
      # in 'computeMTR' only used if parameter 'computePerDayNight' is set to
      # 'TRUE' and timeBins are shorter than day/night. In this case timeBins 
      # are combined by day/night and only time bins with a proportional 
      # observation time greater than the cutoff will be used to compute the 
      # day/night MTR and spread.
      # set value from 0-1
        propObsTimeCutoff = 0.2
      
      # Set classes for which you want the MTR
      # Example: classSelection = c("passerine_type", "wader_type", "large_bird")
        # classSelection = c("passerine_type", "wader_type", "swift_type", 
        #                    "large_bird", "unid_bird", "bird_flock", 
        #                    "insect", "nonbio", "precipitation")
        classSelection.mtr = c("insect")
      
      # Set whether to compute MTR per timebin or per day/night
      # TRUE: MTR is computed per day and night;
      # FALSE: MTR is computed for each time bin   
        computePerDayNight = FALSE
        
      # Set whether to save the MTR to file
        saveMTR2File = TRUE
        
      # Set the output directory to save the filtered dataset to 
        outputDir.mtr = file.path(mainOutputDir, "MTRData")
    }
    
  # plotMTRs - Input Settings ----------------------------------------------- =
  # ===========================================================================
    if (plotMTRs){
      # Set which kind of plot you want
        plotLong    = TRUE
        plotExplore = TRUE
      
      # Set timezones used by each radar/database 
        radarTimeZones.plot  = list(db_20210106_Winterthur = "Etc/GMT0")
      
      # Set target timezones for each radar/dataset
        targetTimeZones.plot = list(db_20210106_Winterthur = "Etc/GMT0")
        
      # Set time range for plots (in targetTimeZone) ; 
      # A plot is created for each timerange
      # use format "yyyy-MM-dd hh:mm"
        timeRangePlot  = list(db_20210106_Winterthur = list(c("2021-01-15 00:00", "2021-01-22 00:00"),
                                                            c("2021-01-23 00:00", "2021-01-31 00:00")))
        
      # Set output path for plots
        outputDir.plots = file.path(mainOutputDir, "Plots")
        
      # Settings plotLong
      # =======================================================================
        if (plotLong){
          # Set paths to mtr files 
            mtrFiles.plot = list(db_20210106_Winterthur = 
                                 file.path(mainOutputDir, "MTRData", "mtr_db_20210106_Winterthur_alt25to1025per50m_time20210115to20210131per3600s_cut0.2.rds"))
            
          # Set the class of which the MTR data should be plotted. 
          # If not set or set to “allClasses”, MTR of all classes will be plotted.
            plotClass = "allClasses"
            
          # Set the maximum value of the y-Scale of the plot to the given value. 
          # If negative or not set, the y-Scale is auto-scaled.
            maxMTR.plot = -1

          # Set the propObsTimeCutOff 
          # If the MTR is computed per day and night, time bins with a proportional 
          # observation time smaller than propObsTimeCutoff are ignored when 
          # combining the time bins. If the MTR is computed for each time bin, 
          # the parameter is ignored.
            propObsTimeCutoff.plot = 0.2
            
          # Set if the spread (first and third quartile) should be plotted
            plotSpread = TRUE
        }
        
      # Settings plotExplore
      # =======================================================================
        if (plotExplore){
          # Set paths to (filtered) db datafiles 
            dbFiles.plot = list(db_20210106_Winterthur = 
                                 file.path(mainOutputDir, "DBData_filtered", "db_20210106_Winterthur_filtered_cut0_altRange25to5000_timeRange20210115to20210131_echoValFALSE.rds"))
          
          # Set whether to get the manual blind time from:
          # "mandb": the db file;  
          # "csv"  : a separate csv file 
            manBlindSource.plot = "csv"
            
          # Set paths to manual blind times files, if manBlindSource == "csv"
            if (manBlindSource.plot %in% "csv"){
              manblindFiles.plot = list(db_20210106_Winterthur = file.path("data",
                                                                           "db_20210106_Winterthur_manualBlindTimes.csv"))
            }
            
          # Set paths to sun information files
            sunFiles.plot = list(db_20210106_Winterthur = file.path(mainOutputDir, "SunData", 
                                                                    "db_20210106_Winterthur_sunInfo.rds"))
            
          # Set  the maximum value of the y-Scale of the plot to the given value. 
          # If negative or not set, the y-Scale is auto-scaled.
            maxAltitude.plot = -1
        }
    }
  
  
# =============================================================================
# =============================================================================
# = ------------- START PROCESSING - THERE BE DRAGONS HERE ------------------ =
# = ------------- TAKE CARE WHEN CHANGING THINGS AS OF HERE ----------------- =
# =============================================================================
# =============================================================================
# Create main output directory
# =============================================================================
  dir.create(mainOutputDir, showWarnings = F, recursive = T)
  
# =============================================================================
# Get data from sql database, if requested 
# =============================================================================
  if (getDBData){
    # most recent timestamp for which data is loaded
      # timing = Sys.time()
      # attr(timing,"tzone") = 'UTC'
      # timing = as.character(timing)
        
    # Do for each of the input databases
    # =========================================================================
      for (cDatabase in dbNames){
        # Print progress message
        # =====================================================================
          message(paste0("Extracting data from ", cDatabase))
        
        # Get current radar and target timezone
        # =====================================================================
          cRadarTimeZone  = radarTimeZones.gd[[cDatabase]]
          cTargetTimeZone = targetTimeZones.gd[[cDatabase]]
        
        # Get data
        # =====================================================================
          data = extractDbData(dbDriverChar                   = dbDriverChar,
                               dbServer                       = dbServer, 
                               dbName                         = cDatabase, 
                               saveDbToFile                   = TRUE,
                               dbDataDir                      = dbOutputDir,
                               radarTimeZone                  = cRadarTimeZone,
                               targetTimeZone                 = cTargetTimeZone,
                               listOfRfFeaturesToExtract      = listOfRfFeaturesToExtract)
          
        # Print progress message
        # =====================================================================
          message(paste0("Finished extracting data from ", cDatabase))
      }
  }

# =============================================================================
# Get Sunrise/Sunset information, if requested
# =============================================================================
  if (getSunInfo){
    # Do for each of the input databases
    # =========================================================================
      for (cDatabase in names(dbFiles.sun)){
        # Print information message
        # =====================================================================
          message(paste0("Computing sunrise/sunset and twilight for ", 
                         cDatabase, ".."))
        
        # Get current file name 
        # =====================================================================
          cFileName = dbFiles.sun[[cDatabase]]
          
        # Read current database data
        # =====================================================================
          cData = readRDS(cFileName)
        
        # Set min and max time_stamps of echodata as timerange for 
        # sunrise/sunset calculation
        # =====================================================================
          timeRangeSunriseSunset = c(min(cData$echoData$time_stamp_targetTZ), 
                                     max(cData$echoData$time_stamp_targetTZ))  
      
        # Compute sunrise/sunset and dawn/dusk (civil)
        # =====================================================================
          sunriseSunset = twilight(timeRange = timeRangeSunriseSunset, 
                                   latLon    = siteLocations[[cDatabase]], 
                                   timeZone  = targetTimeZones.sun[[cDatabase]])
          
        # Save them to a file 
        # =====================================================================
          dir.create(outputDirSunInfo, recursive = T, showWarnings = F)
          saveRDS(sunriseSunset, file = file.path(outputDirSunInfo, 
                                                  paste0(cDatabase, "_sunInfo.rds")))
          
        # Print information message
        # =====================================================================
          message(paste0("Finished computing sunrise/sunset and twilight for ", 
                         cDatabase, ".."))
      }
  }
  
# =============================================================================
# Add day/night information to echoData, if requested
# =============================================================================
  if (addDayNightInfoToEchoes){
    # Do for each of the input databases
    # =========================================================================
      for (cDatabase in names(dbFiles.adn)){
        # Print information message
        # =====================================================================
          message(paste0("Adding day/night information per echo to ", 
                         cDatabase, ".."))
        
        # Get current db file name 
        # =====================================================================
          cFileName.db  = dbFiles.adn[[cDatabase]]
          
        # Get current sun file name 
        # =====================================================================
          cFileName.sun = sunFiles[[cDatabase]]
          
        # Read current database data
        # =====================================================================
          cData = readRDS(cFileName.db)
        
        # Read current sun information data
        # =====================================================================
          csunriseSunset = readRDS(cFileName.sun)
          
        # Add day/night infor per echo
        # =====================================================================
          cData$echoData = addDayNightInfoPerEcho(echoData      = cData$echoData,
                                                  sunriseSunset = csunriseSunset, 
                                                  sunOrCivil    = sunOrCivil)
          
        # Save updated database to the database file
        # =====================================================================
          if (saveToDB.adn){
            saveRDS(cData, file = cFileName.db)
          }
          
        # Print information message
        # =====================================================================
          message(paste0("Finished adding day/night information per echo to ", 
                         cDatabase, ".."))
      }
  }

# =============================================================================
# Filter Protocols, if requested 
# =============================================================================
  if (filterProtocols){
    # Do for each of the input databases
    # =========================================================================
      for (cDatabase in names(dbFiles.fp)){
        # Print information message
        # =====================================================================
          message(paste0("Filtering protocolData of ", cDatabase, ".."))
        
        # Get current db file name 
        # =====================================================================
          cFileName  = dbFiles.fp[[cDatabase]]
          
        # Read current database data
        # =====================================================================
          cData = readRDS(cFileName)
        
        # Filter protocol data
        # =====================================================================
          protocolDataSubset = filterProtocolData(protocolData       = cData$protocolData, 
                                                  pulseTypeSelection = pulseLengthSelection, 
                                                  rotationSelection  = rotationSelection)  
          
        # Save updated database to the database file
        # =====================================================================
          if (saveToDB.fp){
            saveRDS(cData, file = cFileName)
          }
          
        # Print information message
        # =====================================================================
          message(paste0("Finished filtering protocolData of ", cDatabase, ".."))
      }
  }
 
# =============================================================================
# Compute BlindTimes, if requested 
# =============================================================================
  if (computeBlindTimes){
    # Do for each of the input databases
    # =========================================================================
      for (cDatabase in names(manblindFiles)){
        # Print information message
        # =====================================================================
          message(paste0("Loading and computing blind times for ", cDatabase, ".."))
        
        # Get current radar and target timezone
        # =====================================================================
          cRadarTimeZone  = radarTimeZones.cbt[[cDatabase]]
          cTargetTimeZone = targetTimeZones.cbt[[cDatabase]]
        
        # Get current manual blind times 
        # =====================================================================
          # CASE: manBlindSource == "csv"
          # ===================================================================
            if (manBlindSource %in% "csv"){
              # Set the current man blind file name
              # ===============================================================
                cFileName.blind  = manblindFiles[[cDatabase]]
                
              # Read manual blindtimes from csv File 
              # csv contains one row per blindtime and 3 columns 
              # (start of blindtime, stop of blindtime, type of blindtime)
              # times have to be of format 'yyyy-MM-dd hh:mm:ss'
              # =====================================================================
                cManualBlindTimes = loadManualBlindTimes(filePath     = cFileName.blind,
                                                         blindTimesTZ = cRadarTimeZone,
                                                         targetTZ     = cTargetTimeZone)
          # CASE: manBlindSource == "mandb"
          # ===================================================================        
            } else if (manBlindSource %in% "mandb"){
              cManualBlindTimes = cData$manualVisibilityTable
            }
        
        # Get current database filename 
        # =====================================================================
          cFileName.db  = dbFiles.cbt[[cDatabase]]
          
        # Read current database data
        # =====================================================================
          cData = readRDS(cFileName.db)
          
        # compute blindtimes
        # =====================================================================
          blindTimes = mergeVisibilityAndManualBlindTimes(visibilityData   = cData$visibilityData, 
                                                          manualBlindTimes = cManualBlindTimes, 
                                                          protocolData     = cData$protocolData)

        # Save blind times to file, if requested
        # =====================================================================
          if (saveBlindTimes){
            dir.create(outputDirBlindTimes, showWarnings = F, recursive = T)
            saveRDS(blindTimes, file = file.path(outputDirBlindTimes, 
                                                 paste0(cDatabase, 
                                                        "_overallBlindTimes.rds")))
          }
          
        # Print information message
        # =====================================================================
          message(paste0("Finished computing blind times for ", cDatabase, ".."))
      }
  }

# =============================================================================
# Filter Echoes, if requested
# =============================================================================
  if (filterEchoes){
    # Do for each of the input databases
    # =========================================================================
      for (cDatabase in names(dbFiles.fe)){
        # Print information message
        # =====================================================================
          message(paste0("Filtering echo data for ", cDatabase, ".."))
        
        # Convert the time range input to a POSIXct object
        # =====================================================================
          ctimeRangeEchoData = as.POSIXct(timeRangesEchoData[[cDatabase]], 
                                          format = "%Y-%m-%d %H:%M", 
                                          tz     = targetTimeZones.fe[[cDatabase]])
        
        # Get current database filename 
        # =====================================================================
          cFileName.db  = dbFiles.fe[[cDatabase]]
          
        # Read current database data
        # =====================================================================
          cData = readRDS(cFileName.db)
          
        # Get current radar and target timezone
        # =====================================================================
          cRadarTimeZone  = radarTimeZones.fe[[cDatabase]]
          cTargetTimeZone = targetTimeZones.fe[[cDatabase]]
          
        # Get current manual blind times 
        # =====================================================================
          # CASE: manBlindSource == "csv"
          # ===================================================================
            if (manBlindSource %in% "csv"){
              # Set the current man blind file name
              # ===============================================================
                cFileName.blind  = manblindFiles[[cDatabase]]
                
              # Read manual blindtimes from csv File 
              # csv contains one row per blindtime and 3 columns 
              # (start of blindtime, stop of blindtime, type of blindtime)
              # times have to be of format 'yyyy-MM-dd hh:mm:ss'
              # =====================================================================
                cManualBlindTimes = loadManualBlindTimes(filePath     = cFileName.blind,
                                                         blindTimesTZ = cRadarTimeZone,
                                                         targetTZ     = cTargetTimeZone)
          # CASE: manBlindSource == "mandb"
          # ===================================================================
            } else if (manBlindSource %in% "mandb"){
              cManualBlindTimes = cData$manualVisibilityTable
            }
          
        # Subset echodata
        # =====================================================================
          echoDataSubset = filterEchoData(echoData          = cData$echoData,
                                          timeRangeTargetTZ = ctimeRangeEchoData, 
                                          protocolData      = cData$protocolData, 
                                          classSelection    = classSelection,
                                          classProbCutOff   = classProbCutoff,
                                          altitudeRange_AGL = altitudeRange_AGL.fe,
                                          manualBlindTimes  = cManualBlindTimes,
                                          echoValidator     = useEchoValidator)
          cData$echoData = echoDataSubset
          
        # Save the filtered dataset to a file, including also all filter settings, 
        # and the other tables in the original dataset
        # =====================================================================
          dir.create(outputDir.fe, showWarnings = F, recursive = T)
          cData$echoFiltersApplied = list(classProbCutoff    = classProbCutoff,
                                          altitudeRange_AGL  = altitudeRange_AGL.fe,
                                          targetTimeZone     = cTargetTimeZone,
                                          timeRangeEchoData  = ctimeRangeEchoData,
                                          useEchoValidator   = useEchoValidator)
          if (is.null(classProbCutoff)){classProbCutoff.char = 0} 
          cOutputFile = file.path(outputDir.fe, 
                                  paste0(cDatabase, "_filtered_", 
                                         "cut", classProbCutoff.char, "_",
                                         "altRange", paste(altitudeRange_AGL.fe, collapse = "to"), "_", 
                                         "timeRange", paste(format(ctimeRangeEchoData, "%Y%m%d"), 
                                                            collapse = "to"), 
                                         "_",
                                         "echoVal", as.character(useEchoValidator), 
                                         ".rds"))
          saveRDS(cData, cOutputFile)
          
        # Print information message
        # =====================================================================
          message(paste0("Finished filtering echoes for ", cDatabase, ".."))
      }
  }
  
# =============================================================================
# Calculate MTR, if requested
# =============================================================================  
  if (calculateMTR){
    # Do for each of the input databases
    # =========================================================================
      for (cDatabase in names(dbFiles.mtr)){
        # Print information message
        # =====================================================================
          if (computePerDayNight){
            message(paste0("Computing MTR for ", cDatabase, ", using:\n", 
                           "Classes: ", paste(classSelection.mtr, collapse = ", "), "\n",
                           "For altitudes: ", paste(altitudeRange_AGL.mtr, collapse = " to "), 
                           " in bins of ", altitudeBinSize, "m\n", 
                           "on a nightly/daily basis"))
          } else {
            message(paste0("Computing MTR for ", cDatabase, ", using:\n", 
                           "Classes: ", paste(classSelection.mtr, collapse = ", "), "\n",
                           "For altitudes: ", paste(altitudeRange_AGL.mtr, collapse = " to "), 
                           " in bins of ", altitudeBinSize, "m\n", 
                           "Timebins of: ", timeBinduration_sec, " seconds or ",
                           timeBinduration_sec/3600, " hours"))
          }
        
        # Convert the timebin time range input to a POSIXct object
        # =====================================================================
          cTimeRangeTimeBins = as.POSIXct(timeRangesTimeBins[[cDatabase]], 
                                          format = "%Y-%m-%d %H:%M", 
                                          tz     = targetTimeZones.mtr[[cDatabase]])
          
        # Get current database filename 
        # =====================================================================
          cFileName.db  = dbFiles.mtr[[cDatabase]]
          
        # Read current database data
        # =====================================================================
          cData = readRDS(cFileName.db)
          
        # Get current sun file name 
        # =====================================================================
          cFileName.sun = sunFiles[[cDatabase]]
	  
        # Read current sun information data
        # =====================================================================
          csunriseSunset = readRDS(cFileName.sun)
          
        # Get current blind file name 
        # =====================================================================
          cFileName.blind = blindFiles[[cDatabase]]
	  
        # Read current sun information data
        # =====================================================================
          blindTimes = readRDS(cFileName.blind)
          
        # Calculate the MTR
        # =====================================================================
          cMTR = computeMTR(echoes                      = cData$echoData, 
                            classSelection              = classSelection.mtr, 
                            altitudeRange               = altitudeRange_AGL.mtr,
                            altitudeBinSize             = altitudeBinSize, 
                            timeRange                   = cTimeRangeTimeBins, 
                            timeBinDuration_sec         = timeBinduration_sec, 
                            timeZone                    = targetTimeZones.mtr[[cDatabase]], 
                            sunriseSunset               = csunriseSunset, 
                            sunOrCivil                  = sunOrCivil,
                            protocolData                = cData$protocolData, 
                            blindTimes                  = blindTimes, 
                            blindTimeAsMtrZero          = blindTimeAsMtrZero,
                            propObsTimeCutoff           = propObsTimeCutoff, 
                            computePerDayNight          = computePerDayNight,
                            computeAltitudeDistribution = TRUE)
          
        # Save the mTR to file, if requested
        # =====================================================================
          if (saveMTR2File){
            if (computePerDayNight){
              cOutputFile = paste0("mtr_", cDatabase, 
                                   "_alt", paste(altitudeRange_AGL.mtr, 
                                                 collapse = "to"), 
                                   "per", altitudeBinSize, "m_", 
                                   "time", 
                                   paste(format(cTimeRangeTimeBins, "%Y%m%d"), 
                                         collapse = "to"), 
                                   "perDayNight_", 
                                   "cut", propObsTimeCutoff, ".rds")
            } else {
              cOutputFile = paste0("mtr_", cDatabase, 
                            "_alt", paste(altitudeRange_AGL.mtr, 
                                          collapse = "to"), 
                            "per", altitudeBinSize, "m_", 
                            "time", paste(format(cTimeRangeTimeBins, "%Y%m%d"), 
                                          collapse = "to"), 
                            "per", timeBinduration_sec, "s_", 
                            "cut", propObsTimeCutoff, ".rds")
            }
            saveMTR(mtr                = cMTR, 
                    filepath           = outputDir.mtr,
                    fileName           = cOutputFile)
          }
      }
  }
  
# =============================================================================
# Plot MTRs, if requested
# =============================================================================  
  if (plotMTRs){
    # Do for each of the input databases
    # =========================================================================
      for (cDatabase in names(dbFiles.plot)){
        # Convert the timeRangePlot input to a POSIXct object
        # =====================================================================
          posixCTListCon = function(x){
            as.POSIXct(x, format = "%Y-%m-%d %H:%M", tz = targetTimeZones.plot[[cDatabase]])
          }
          cTimeRangePlot = lapply(timeRangePlot[[cDatabase]], posixCTListCon)
          
        # Get current radar and target timezone
        # =====================================================================
          cRadarTimeZone  = radarTimeZones.plot[[cDatabase]]
          cTargetTimeZone = targetTimeZones.plot[[cDatabase]]
          
        # plot longitudinal, if requested
        # =====================================================================
          if (plotLong){
            # Print message
            # =================================================================
              message("Plotting longitudinal MTR..")  
            
            # Get current mtr filename 
            # =================================================================
              cFileName.mtr  = mtrFiles.plot[[cDatabase]]
              
            # Read current mtr data
            # =================================================================
              cMTR = readRDS(cFileName.mtr)
            
            # Make Plot 
            # =================================================================
              plotLongitudinalMTR(mtr               = cMTR, 
                                  maxMTR            = maxMTR.plot, 
                                  timeRange         = cTimeRangePlot, 
                                  plotClass         = plotClass,
                                  propObsTimeCutoff = propObsTimeCutoff.plot,
                                  plotSpread        = plotSpread, 
                                  filePath          = outputDir.plots) 
          }
        
        # Make Exploration plot, if requested
        # =====================================================================
          if (plotExplore){
            # Print message
            # =================================================================
              message("Plotting exploration..")
              
            # Get current database filename 
            # =================================================================
              cFileName.db  = dbFiles.plot[[cDatabase]]
              
            # Read current database data
            # =================================================================
              cData = readRDS(cFileName.db)  
          
            # Get current sun file name 
            # =================================================================
              cFileName.sun = sunFiles.plot[[cDatabase]]
            
            # Read current sun information data
            # =================================================================
              csunriseSunset = readRDS(cFileName.sun)
              
            # Get current manual blind times 
            # =================================================================
              # CASE: manBlindSource.plot == "csv"
              # ===============================================================
                if (manBlindSource.plot %in% "csv"){
                  # Set the current man blind file name
                  # ===========================================================
                    cFileName.blind  = manblindFiles.plot[[cDatabase]]
                    
                  # Read manual blindtimes from csv File 
                  # csv contains one row per blindtime and 3 columns 
                  # (start of blindtime, stop of blindtime, type of blindtime)
                  # times have to be of format 'yyyy-MM-dd hh:mm:ss'
                  # ===========================================================
                    cManualBlindTimes = loadManualBlindTimes(filePath     = cFileName.blind,
                                                             blindTimesTZ = cRadarTimeZone,
                                                             targetTZ     = cTargetTimeZone)
                    
              # CASE: manBlindSource.plot == "mandb"
              # ===============================================================       
                } else if (manBlindSource.plot %in% "mandb"){
                  cManualBlindTimes = cData$manualVisibilityTable
                }
              
            # Make Plot 
            # =================================================================
              plotExploration(echoData         = cData$echoData, 
                              timeRange        = cTimeRangePlot, 
                              manualBlindTimes = cManualBlindTimes, 
                              visibilityData   = cData$visibilityData, 
                              protocolData     = cData$protocolData, 
                              sunriseSunset    = csunriseSunset, 
                              maxAltitude      = maxAltitude.plot, 
                              filePath         = outputDir.plots)
          }
      }
  }


