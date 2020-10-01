#' @export
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' 
#' @title Load multiple WRCC RAWS timeseries objects from a local directory
#'
#' @param wrccIDs Vector of RAWS station identifiers (will be upcased).
#' @param meta Tibble of WRCC station metadata.
#' @param year Year to access station data for.
#' @param forceDownload Logical flag stating whether or not to download and override existing data.
#' @param baseUrl Base URL for data queries.
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @return List of timeseries objects each containing 'meta' and 'data'.
#'
#' @description Loads WRCC station metadata and data from the \code{rawsDataDir}. If the
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
#' stationMeta <- wrcc_loadMeta(stateCode = "WA")
#' 
#' wrccIDs <- c("waWENU", "waWASH", "waWALD")
#' stationData <- wrcc_loadMultiple(wrccIDs = wrccIDs, meta = stationMeta, year = 2020)
#' 
#' dplyr::glimpse(stationData)
#' }
#'
#' @seealso \code{wrcc_createTimeseriesObject}
#' @seealso \code{wrcc_load}
#' @seealso \code{setRawsDataDir}
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}

wrcc_loadMultiple <- function(
  wrccIDs = NULL,
  meta = NULL,
  year = NULL,
  forceDownload = FALSE,
  baseUrl = "https://wrcc.dri.edu/cgi-bin/wea_list2.pl",
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(wrccIDs)
  MazamaCoreUtils::stopIfNull(year)
  
  if ( !is.numeric(year) ) {
    stop("Parameter 'year' must be numeric.")
  }
  
  dataDir <- getRawsDataDir()
  
  # ----- Load data for each station -------------------------------------------
  
  stationList <- list()
  counter <- 1
  
  for ( wrccID in wrccIDs ) {
    
    if ( verbose )
      message(sprintf("Loading data for %s (%d/%d)", wrccID, counter, length(wrccIDs)))
    
    res <- try({
      
      # Load or download data for each station 
      stationTimeseriesObject <-
        wrcc_load(
          wrccID = wrccID,
          meta = meta,
          year = year,
          forceDownload = forceDownload,
          baseUrl = baseUrl,
          verbose = verbose)
      
      # Add this station's raws_timeseries object to the list
      stationList[[wrccID]] <- stationTimeseriesObject
      
    }, silent = verbose)
    
    counter <- counter + 1
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(stationList)
  
}
