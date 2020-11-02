#' @export
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' 
#' @title Load multiple FW13 RAWS timeseries objects from a local directory
#'
#' @param nwsIDs Vector of NWS RAWS station identifiers.
#' @param meta Tibble of FW13 station metadata.
#' @param newDownload Logical flag stating whether or not to download and override existing data.
#' @param baseUrl Base URL for data queries.
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @return List of timeseries objects each containing 'meta' and 'data'.
#'
#' @description Loads FW13 station metadata and data from the \code{rawsDataDir}. If the
#' data is not in this directory, this will download and save the data. 
#' 
#' @examples
#' \donttest{
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("NaturalEarthAdm1")
#' 
#' library(RAWSmet)
#' setRawsDataDir("~/Data/RAWS/")
#'
#' stationMeta <- fw13_loadMeta()
#' 
#' nwsIDs <- c("021503", "500726", "020401")
#' stationData <- fw13_loadMultiple(nwsIDs = nwsIDs, meta = stationMeta)
#' 
#' dplyr::glimpse(stationData)
#' }
#'
#' @seealso \code{fw13_createTimeseriesObject}
#' @seealso \code{fw13_load}
#' @seealso \code{setRawsDataDir}
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}

fw13_loadMultiple <- function(
  nwsIDs = NULL,
  meta = NULL,
  newDownload = FALSE,
  baseUrl = "https://cefa.dri.edu/raws/fw13/",
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(nwsIDs)
  
  dataDir <- getRawsDataDir()
  
  # ----- Load data for each station -------------------------------------------
  
  stationList <- list()
  counter <- 1
  
  for ( nwsID in nwsIDs ) {
    
    if ( verbose )
      message(sprintf("Loading data for %s (%d/%d)", nwsID, counter, length(nwsIDs)))
    
    res <- try({
      
      # Load or download data for each station 
      stationTimeseriesObject <-
        fw13_load(
          nwsID = nwsID,
          meta = meta,
          newDownload = newDownload,
          baseUrl = baseUrl,
          verbose = verbose)
      
      # Add this station's raws_timeseries object to the list
      stationList[[nwsID]] <- stationTimeseriesObject
      
    }, silent = verbose)
    
    counter <- counter + 1
    
  }
  
  class(stationList) <- c("rawsList", class(stationList))
  
  # ----- Return ---------------------------------------------------------------
  
  return(stationList)
  
}
