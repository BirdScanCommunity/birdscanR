

#' @title addFeatSummToMTR
#' @author Birgen Haest, Baptiste Schmid, 
#' @description [addFeatSummToMTR()] add summary statistics (n, mean, SD) of 
#' selected features to MTR/Density/VPTS tables
#' @param mtrDensVPTS dataframe with the echo data from the data list created with
#' [computeMTR()], [computeDensity()], or [createVPTS()].
#' @param echoData dataframe with the echo data from the data list created with
#' [extractDbData()] or [compileData()].
#' @param inputVariable Name of a feature included in the [echoData].
#' @param classSelection character string vector with all classes which should
#' be used to calculate the MTR. The MTR and number of Echoes will be calculated
#' for each class as well as for all classes together.
#' @param nCores maximal number of cores used for parallesisation. Default value is 2.


## original script

# =============================================================================
# Author:           Birgen Haest
# =============================================================================
# Functionality:
#   Script to get the average per altitude-time bin for the direction and RCS
# =============================================================================
# =============================================================================
# Packages and sourced functions
# =============================================================================
  library(parallel)
  library(doParallel)
  library(foreach)
  library(circhelp)

# Set nrCores to use for parallel processing
#   IF set to 1, processing is not PARALLELIZED, 
#   IF > 1, processing is PARALLELIZED
# =============================================================================
# default number of cores:
    nrCores = 2 



# =============================================================================
# Input Settings
# =============================================================================
  # Set main input directory containing the input mtr files, the output 
  #   directory to store the results in, and filter for the mtr files. 
  # ===========================================================================
    mainInputDir     = file.path(".", "data")
    mainOutputDir    = file.path(".", "results")
    inputFilesFilter = "mtr_binSize_"
    preScriptToDelete = "mtr_binSize_"
    
    altitudeSubset   = list(TRUE, c(50, 1000))
    classSelection   = c("insect", "passerine_type", "wader_type", "swift_type", 
                         "large_bird", "unid_bird", "bird_flock")
    
    featsToSummarize = c(
                         "feature2.azimuth",
                         # "RCS2_RCS_max_lowpassed_InCm",
                         "RCS2_RCS_max_lowpassed",
                         "WFF_predicted"
                         # "ACMaxPeakFreq"
                         )
    functionToApply  = c(                 # This should be a characer vector of the same length
                         "circularMean",  #  as featsToSummarize, defining the funtion to 
                         # "mean",          #  apply on the respective feature.
                         "mean",          #  Current options are:
                         "mean"           #   - "mean": add (1) variable that is the
                        #                   #             arithmetic mean and (2) one
                        )                 #             that is the sd.
                                          #   - "circularMean": add (1) variable that is the
                                          #             circular mean and (2) one
                                          #             that is the circular sd (for circular 
                                          #             data like flight directions)
                                          
# =============================================================================  
# =============================================================================
# # ------------------  END OF INPUT SETTINGS  ------------------------------ #
# =============================================================================

    
# =============================================================================
# # -------------------  START OF PROCESSING -------------------------------- #
# =============================================================================
# ============================================================================= 
# Get list of files to process
# =============================================================================
  mtrFiles = list.files(path = mainInputDir, pattern = inputFilesFilter)
 
# Create the output directory
# =============================================================================
  dir.create(mainOutputDir, showWarnings = F, recursive = T)
 
# Start the parallel cluster
# =============================================================================
  if (nrCores > 1){
    cParCluster = parallel::makeCluster(nrCores, type = "PSOCK")
    doParallel::registerDoParallel(cParCluster)
  }
          
# Do for each of the mtr files - NON-PARALLELIZED Version
# =============================================================================
  if (nrCores == 1){
    for (cFile in mtrFiles){
      # Report progress
      # =======================================================================
        message(paste0("Starting to add feature summaries to the mtr file of ", cFile))
        message("++++++++++++++++++++++++++++++++++++++++++++++++++++")
        message("++++++++++++++++++++++++++++++++++++++++++++++++++++")
  
      # Read the mtr file
      # =======================================================================
        mtrOverview = readRDS(file.path(mainInputDir, cFile))
        
      # Add dbName to the mtr database
      # =======================================================================
        mtrOverview$mtrDbName = gsub("\\.rds", "", cFile)
        # patternToMatch = "(?<=mtr_binSize_).*?(?=_filtered_)"
        patternToMatch = paste0("(?<=", inputFilesFilter, ").*?(?=_filtered_)")
        mtrOverview$dbName = stringr::str_extract(mtrOverview$mtrDbName, patternToMatch)
        
      # Subset altitudes, if requested
      # =======================================================================
        if (altitudeSubset[[1]]){
          mtrOverview = mtrOverview[(mtrOverview$altitudeChunkBegin >= altitudeSubset[[2]][1]) & 
                                      (mtrOverview$altitudeChunkEnd <= altitudeSubset[[2]][2]), ]
        } 
        
      # Set the prescript part of the mtr filename to delete
      # =======================================================================
        # allDbs         = sort(unique(mtrOverview$mtrDbName))
        prescript = inputFilesFilter
        
      # Get the data of the current database
      # =======================================================================
        cDbFile   = paste0(gsub(prescript, "", mtrOverview$mtrDbName[1]), 
                           ".rds")
        cEchoData = readRDS(file.path(mainInputDir, cDbFile)) 
            
      # Do for each of the requested features
      # =======================================================================
        for (cFeatureID in 1:length(featsToSummarize)){
          # Get the name of the current feature
          # ===================================================================
            cFeature = featsToSummarize[cFeatureID]
          
          # Add variable(s) to the mtrOverview to hold the new summary feature
          # ===================================================================
            for (cClass in classSelection){
              # CASE: Do feature MEAN and SD
              # ===============================================================
                if (functionToApply[cFeatureID] %in% "mean"){
                  mtrOverview[, paste0(cClass, "_", cFeature, "_mean")] = NA
                  mtrOverview[, paste0(cClass, "_", cFeature, "_sd")]   = NA
                  mtrOverview[, paste0(cClass, "_", cFeature, "_mean_MTRFactorWeighted")] = NA
                  mtrOverview[, paste0(cClass, "_", cFeature, "_sd_MTRFactorWeighted")]   = NA
              # CASE: Do feature CIRCULAR MEAN and SD
              # ===============================================================
                } else if (functionToApply[cFeatureID] %in% "circularMean"){
                  mtrOverview[, paste0(cClass, "_", cFeature, "_circMean")]   = NA
                  mtrOverview[, paste0(cClass, "_", cFeature, "_circLength")] = NA
                  mtrOverview[, paste0(cClass, "_", cFeature, "_circSD")]     = NA
                  mtrOverview[, paste0(cClass, "_", cFeature, "_circMean_MTRFactorWeighted")]   = NA
                  mtrOverview[, paste0(cClass, "_", cFeature, "_circLength_MTRFactorWeighted")] = NA
                  mtrOverview[, paste0(cClass, "_", cFeature, "_circSD_MTRFactorWeighted")]     = NA
                }
            }
          
          # Do for each of the time-altitude bins of this database
          # ===================================================================
            # rowscDb = which(mtrOverview$mtrDbName %in% allDbs[cDbId])
            for (cRow in 1:nrow(mtrOverview)){
              # Do for each of the classes
              # ===============================================================
                for (cClass in classSelection){
                  # Check whether there are any mtr values before getting any 
                  # values
                  # ===========================================================
                    if ((mtrOverview[cRow, paste0("mtr.", cClass)] != 0) && 
                       !(is.na((mtrOverview[cRow, paste0("mtr.", cClass)])))){
                      # Get the id of all samples that fall withing the timeframe and 
                      #  and altitudinal bin
                      # =======================================================
                        cSampleRows = which((cEchoData$echoData$time_stamp_targetTZ >= mtrOverview$timeChunkBegin[cRow]) &
                                            (cEchoData$echoData$time_stamp_targetTZ < mtrOverview$timeChunkEnd[cRow]) & 
                                            (cEchoData$echoData$feature1.altitude_AGL >= mtrOverview$altitudeChunkBegin[cRow]) & 
                                            (cEchoData$echoData$feature1.altitude_AGL < mtrOverview$altitudeChunkEnd[cRow]) &
                                            (cEchoData$echoData$class %in% cClass))
                        valuesToProcess     = cEchoData$echoData[cSampleRows, cFeature]
                        elementsToKeep      = which(!is.na(valuesToProcess))
                        valuesToProcess     = valuesToProcess[elementsToKeep]
                        mtrFactorsToProcess = cEchoData$echoData[cSampleRows, "mtr_factor_rf"]
                        mtrFactorsToProcess = mtrFactorsToProcess[elementsToKeep]
                        
                      # Get the mean value for the feature if there are any samples 
                      #  for this time-altitude bin
                      # =======================================================
                        if (length(valuesToProcess) > 1){
                          # Calculate mtr weights
                          # ===================================================
                            mtrFactorWeights    = mtrFactorsToProcess/max(mtrFactorsToProcess)
                          
                          # CASE: Do feature MEAN and SD
                          # ===================================================
                            if (functionToApply[cFeatureID] %in% "mean"){
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_mean")] = mean(valuesToProcess)
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_sd")]   = sd(valuesToProcess)
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_mean_MTRFactorWeighted")] = weighted.mean(x = valuesToProcess, 
                                                                                                                          w = mtrFactorWeights)
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_sd_MTRFactorWeighted")]   = descriptio::weighted.sd(x       = valuesToProcess, 
                                                                                                                                    weights = mtrFactorWeights)
                          # CASE: Do feature CIRCULAR MEAN and SD
                          # ===================================================
                            } else if (functionToApply[cFeatureID] %in% "circularMean"){
                              cCircData = circular::circular(x        = valuesToProcess,
                                                             units    = "degrees", 
                                                             zero     = pi/2,
                                                             rotation = "clock")
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circMean")]   = circular::mean.circular(cCircData)
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circLength")] = circular::rho.circular(cCircData)
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circSD")]     = circular::sd.circular(cCircData)
                              cCircDataRadians = circular::rad(cCircData)
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circMean_MTRFactorWeighted")]   = circular::deg(circhelp::weighted_circ_mean(x = cCircDataRadians, 
                                                                                                                                                             w = mtrFactorWeights))
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circLength_MTRFactorWeighted")] = circhelp::weighted_circ_rho(x = cCircDataRadians, 
                                                                                                                                              w = mtrFactorWeights)
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circSD_MTRFactorWeighted")]     = circhelp::weighted_circ_sd(x = cCircDataRadians, 
                                                                                                                                             w = mtrFactorWeights)
                            }
                        } else if (length(valuesToProcess) == 1){
                          # CASE: Do feature MEAN and SD
                          # ===================================================
                            if (functionToApply[cFeatureID] %in% "mean"){
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_mean")] = valuesToProcess
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_sd")]   = 0
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_mean_MTRFactorWeighted")] = valuesToProcess
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_sd_MTRFactorWeighted")]   = 0
                          # CASE: Do feature CIRCULAR MEAN and SD
                          # ===================================================
                            } else if (functionToApply[cFeatureID] %in% "circularMean"){
                              cCircData = circular::circular(x        = valuesToProcess,
                                                             units    = "degrees", 
                                                             zero     = pi/2,
                                                             rotation = "clock")
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circMean")]   = cCircData
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circLength")] = 1
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circSD")]     = 0
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circMean_MTRFactorWeighted")]   = cCircData
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circLength_MTRFactorWeighted")] = 1
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circSD_MTRFactorWeighted")]     = 0
                            }
                        }  
                    }
                }
            }
            
          # Report progress
          # ===================================================================
            message("Finished adding feature summary of: ", cFeature, " for ", cFile)
            message("----------------------------------------------------")  
        }
        
      # Save the mtrOverview with the additional feature summaries
      # =======================================================================
        saveRDS(mtrOverview, 
                file.path(mainOutputDir,
                          gsub("\\.rds", "_withFeatSumms\\.rds", cFile)))
          
      # Report progress
      # =======================================================================
        message(paste0("Finished adding feature summaries to ", cFile))
        message("++++++++++++++++++++++++++++++++++++++++++++++++++++")
        message("++++++++++++++++++++++++++++++++++++++++++++++++++++")
    }
  }
 
# Do for each of the mtr files - PARALLELIZED Version
# =============================================================================
  if (nrCores > 1){
    foreach (cFileIndex = 1:length(mtrFiles)) %dopar% {
      # Set the name of the current file
      # =======================================================================
        cFile = mtrFiles[cFileIndex]
        
      # Report progress
      # =======================================================================
        message(paste0("Starting to add feature summaries to the mtr file of ", cFile))
        message("++++++++++++++++++++++++++++++++++++++++++++++++++++")
        message("++++++++++++++++++++++++++++++++++++++++++++++++++++")
        
      # Read the mtr file
      # =======================================================================
        mtrOverview = readRDS(file.path(mainInputDir, cFile))
        
      # Add dbName to the mtr database
      # =======================================================================
        mtrOverview$mtrDbName = gsub("\\.rds", "", cFile)
        # patternToMatch = "(?<=mtr_binSize_).*?(?=_filtered_)"
        patternToMatch = paste0("(?<=", inputFilesFilter, ").*?(?=_filtered_)")
        mtrOverview$dbName = stringr::str_extract(mtrOverview$mtrDbName, patternToMatch)
        
      # Subset altitudes, if requested
      # =======================================================================
        if (altitudeSubset[[1]]){
          mtrOverview = mtrOverview[(mtrOverview$altitudeChunkBegin >= altitudeSubset[[2]][1]) & 
                                      (mtrOverview$altitudeChunkEnd <= altitudeSubset[[2]][2]), ]
        } 
        
      # Set the prescript part of the mtr filename to delete
      # =======================================================================
        # allDbs         = sort(unique(mtrOverview$mtrDbName))
        prescript = preScriptToDelete
        
      # Get the data of the current database
      # =======================================================================
        cDbFile   = paste0(gsub(prescript, "", mtrOverview$mtrDbName[1]), 
                           ".rds")
        cEchoData = readRDS(file.path(mainInputDir, cDbFile)) 
            
      # Do for each of the requested features
      # =======================================================================
        for (cFeatureID in 1:length(featsToSummarize)){
          # Get the name of the current feature
          # ===================================================================
            cFeature = featsToSummarize[cFeatureID]
          
          # Add variable(s) to the mtrOverview to hold the new summary feature
          # ===================================================================
            for (cClass in classSelection){
              # CASE: Do feature MEAN and SD
              # ===============================================================
                if (functionToApply[cFeatureID] %in% "mean"){
                  mtrOverview[, paste0(cClass, "_", cFeature, "_mean")] = NA
                  mtrOverview[, paste0(cClass, "_", cFeature, "_sd")]   = NA
                  mtrOverview[, paste0(cClass, "_", cFeature, "_mean_MTRFactorWeighted")] = NA
                  mtrOverview[, paste0(cClass, "_", cFeature, "_sd_MTRFactorWeighted")]   = NA
              # CASE: Do feature CIRCULAR MEAN and SD
              # ===============================================================
                } else if (functionToApply[cFeatureID] %in% "circularMean"){
                  mtrOverview[, paste0(cClass, "_", cFeature, "_circMean")]   = NA
                  mtrOverview[, paste0(cClass, "_", cFeature, "_circLength")] = NA
                  mtrOverview[, paste0(cClass, "_", cFeature, "_circSD")]     = NA
                  mtrOverview[, paste0(cClass, "_", cFeature, "_circMean_MTRFactorWeighted")]   = NA
                  mtrOverview[, paste0(cClass, "_", cFeature, "_circLength_MTRFactorWeighted")] = NA
                  mtrOverview[, paste0(cClass, "_", cFeature, "_circSD_MTRFactorWeighted")]     = NA
                }
            }
          
          # Do for each of the time-altitude bins of this database
          # ===================================================================
            # rowscDb = which(mtrOverview$mtrDbName %in% allDbs[cDbId])
            for (cRow in 1:nrow(mtrOverview)){
              # Do for each of the classes
              # ===============================================================
                for (cClass in classSelection){
                  # Check whether there are any mtr values before getting any 
                  # values
                  # ===========================================================
                    if ((mtrOverview[cRow, paste0("mtr.", cClass)] != 0) && 
                       !(is.na((mtrOverview[cRow, paste0("mtr.", cClass)])))){
                      # Get the id of all samples that fall withing the timeframe and 
                      #  and altitudinal bin
                      # =======================================================
                        cSampleRows = which((cEchoData$echoData$time_stamp_targetTZ >= mtrOverview$timeChunkBegin[cRow]) &
                                            (cEchoData$echoData$time_stamp_targetTZ < mtrOverview$timeChunkEnd[cRow]) & 
                                            (cEchoData$echoData$feature1.altitude_AGL >= mtrOverview$altitudeChunkBegin[cRow]) & 
                                            (cEchoData$echoData$feature1.altitude_AGL < mtrOverview$altitudeChunkEnd[cRow]) &
                                            (cEchoData$echoData$class %in% cClass))
                        valuesToProcess     = cEchoData$echoData[cSampleRows, cFeature]
                        elementsToKeep      = which(!is.na(valuesToProcess))
                        valuesToProcess     = valuesToProcess[elementsToKeep]
                        mtrFactorsToProcess = cEchoData$echoData[cSampleRows, "mtr_factor_rf"]
                        mtrFactorsToProcess = mtrFactorsToProcess[elementsToKeep]
                        
                      # Get the mean value for the feature if there are any samples 
                      #  for this time-altitude bin
                      # =======================================================
                        if (length(valuesToProcess) > 1){
                          # Calculate mtr weights
                          # ===================================================
                            mtrFactorWeights    = mtrFactorsToProcess/max(mtrFactorsToProcess)
                          
                          # CASE: Do feature MEAN and SD
                          # ===================================================
                            if (functionToApply[cFeatureID] %in% "mean"){
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_mean")] = mean(valuesToProcess)
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_sd")]   = sd(valuesToProcess)
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_mean_MTRFactorWeighted")] = weighted.mean(x = valuesToProcess, 
                                                                                                                          w = mtrFactorWeights)
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_sd_MTRFactorWeighted")]   = descriptio::weighted.sd(x       = valuesToProcess, 
                                                                                                                                    weights = mtrFactorWeights)
                          # CASE: Do feature CIRCULAR MEAN and SD
                          # ===================================================
                            } else if (functionToApply[cFeatureID] %in% "circularMean"){
                              cCircData = circular::circular(x        = valuesToProcess,
                                                             units    = "degrees", 
                                                             zero     = pi/2,
                                                             rotation = "clock")
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circMean")]   = circular::mean.circular(cCircData)
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circLength")] = circular::rho.circular(cCircData)
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circSD")]     = circular::sd.circular(cCircData)
                              cCircDataRadians = circular::rad(cCircData)
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circMean_MTRFactorWeighted")]   = circular::deg(circhelp::weighted_circ_mean(x = cCircDataRadians, 
                                                                                                                                                             w = mtrFactorWeights))
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circLength_MTRFactorWeighted")] = circhelp::weighted_circ_rho(x = cCircDataRadians, 
                                                                                                                                              w = mtrFactorWeights)
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circSD_MTRFactorWeighted")]     = circhelp::weighted_circ_sd(x = cCircDataRadians, 
                                                                                                                                             w = mtrFactorWeights)
                            }
                        } else if (length(valuesToProcess) == 1){
                          # CASE: Do feature MEAN and SD
                          # ===================================================
                            if (functionToApply[cFeatureID] %in% "mean"){
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_mean")] = valuesToProcess
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_sd")]   = 0
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_mean_MTRFactorWeighted")] = valuesToProcess
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_sd_MTRFactorWeighted")]   = 0
                          # CASE: Do feature CIRCULAR MEAN and SD
                          # ===================================================
                            } else if (functionToApply[cFeatureID] %in% "circularMean"){
                              cCircData = circular::circular(x        = valuesToProcess,
                                                             units    = "degrees", 
                                                             zero     = pi/2,
                                                             rotation = "clock")
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circMean")]   = cCircData
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circLength")] = 1
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circSD")]     = 0
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circMean_MTRFactorWeighted")]   = cCircData
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circLength_MTRFactorWeighted")] = 1
                              mtrOverview[cRow, paste0(cClass, "_", cFeature, "_circSD_MTRFactorWeighted")]     = 0
                            }
                        }  
                    }
                }
            }
            
          # Report progress
          # ===================================================================
            message("Finished adding feature summary of: ", cFeature, " for ", cFile)
            message("----------------------------------------------------")  
        }
        
      # Save the mtrOverview with the additional feature summaries
      # =======================================================================
        saveRDS(mtrOverview, 
                file.path(mainOutputDir,
                          gsub("\\.rds", "_withFeatSumms\\.rds", cFile)))
          
      # Report progress
      # =======================================================================
        message(paste0("Finished adding feature summaries to ", cFile))
        message("++++++++++++++++++++++++++++++++++++++++++++++++++++")
        message("++++++++++++++++++++++++++++++++++++++++++++++++++++")
    }
  }
   
# Stop the cluster
# =============================================================================
  if (nrCores > 1){
    parallel::stopCluster(cParCluster) 
  }  
  