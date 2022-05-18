#### createAltitudeBins ------------------------------------------------------ 
#' @title createAltitudeBins 
#' @author Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; with edits by Birgen Haest, \email{birgen.haest@@vogelwarte.ch}  
#' @description create AltitudeBins used for MTR computation
#' 
#' @param altitudeRange numeric vector of length 2 with the start and end of the altitude range in meter a.g.l. 
#' @param altitudeBinSize numeric, size of the altitude bins in meter. 
#'
#' @return dataframe with altitude bins
#' @export
#'
#' @examples
#' altitudeRange_AGL_25_1025 = c( 25, 1025 )
#' message( "creating altitude bins" )
#' altitudeBins_25_1025_oneBin = createAltitudeBins( altitudeRange = altitudeRange_AGL_25_1025, altitudeBinSize = 1000 )
#' altitudeBins_25_1025_binSize50 = createAltitudeBins( altitudeRange = altitudeRange_AGL_25_1025, altitudeBinSize = 50 )
createAltitudeBins = function(altitudeRange, altitudeBinSize){
  
  sequence     = seq(altitudeRange[1], altitudeRange[2], altitudeBinSize)
  
  altitudeBins = data.frame(id          = seq(1, (length(sequence)-1), by = 1), 
                            begin       = sequence[1:(length(sequence)-1)], 
                            end         = sequence[2:length(sequence)], 
                            size        = NA_real_, 
                            avgAltitude = NA_real_)
  
  altitudeBins$size        = altitudeBins$end - altitudeBins$begin
  altitudeBins$avgAltitude = ((altitudeBins$begin + altitudeBins$end)) / 2
  
  return(altitudeBins)
}

