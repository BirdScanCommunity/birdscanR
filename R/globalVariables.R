# Avoid notes on global variables in devtools::check()
# =============================================================================
  utils::globalVariables(c("yStart", "yStop", "xStart", "xStop", "type", 
                           "time_stamp_targetTZ", "feature1.altitude_AGL",
                           "timeChunkDate", "nEchoes", "dayOrNight",
                           "mtrFirstQuartile", "mtrThirdQuartile", 
                           "obs", "obsType", 
                           ".", "timeChunkId", "observationTime_h",
                           "mtr_factor_rf", "altitudeChunkId", 
                           "mtr_echo", 
                           "timeChunkMiddle"))