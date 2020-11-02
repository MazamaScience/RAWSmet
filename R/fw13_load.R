#' @export
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' 
#' @title Load FW13 RAWS timeseries object from a local directory
#'
#' @param nwsID NWS RAWS station identifier.
#' @param meta Tibble of FW13 station metadata.
#' @param baseUrl Base URL for data queries.
#' @param verbose Logical flag controlling detailed progress statements.
#' @param newDownload Logical flag stating whether or not to download and override existing data.
#' 
#' @return Timeseries object with 'meta' and 'data'.
#'
#' @description Loads FW13 station metadata and data from the \code{rawsDataDir}. 
#' If the data is not in this directory, this will download and save the data. 
#' 
#' @examples
#' \dontrun{
#' library(RAWSmet)
#' setRawsDataDir("~/Data/RAWS/")
#' 
#' # For creation of metadata
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("NaturalEarthAdm1")
#' 
#' meta <- fw13_loadMeta()
#' 
#' rawsObject <- fw13_load(nwsID = 451702, meta = meta)
#' head(rawsObject$data)
#' }
#'
#' @seealso \code{fw13_createTimeseriesObject}
#' @seealso \code{setRawsDataDir}
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}

fw13_load <- function(
  nwsID = NULL,
  meta = NULL,
  newDownload = FALSE,
  baseUrl = "https://cefa.dri.edu/raws/fw13/",
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(nwsID)
  
  currentYear <- lubridate::now(tzone = "UTC") %>% lubridate::year()

  dataDir <- getRawsDataDir()
  
  # ----- Check for local data -------------------------------------------------
  
  # NOTE:  We save the file with the currentYear. This guarantees that updated
  # NOTE:  files will be obtained at least every year.
  
  fileName = sprintf("fw13_%s_%d.rda", nwsID, currentYear)
  filePath = file.path(dataDir, fileName)
  
  if ( file.exists(filePath) && newDownload == FALSE ) {
    
    if( verbose ) {
      message(sprintf("Loading data from %s", filePath))
    }
    
    # If local data exists, load and return it.
    rawsObject <- get(load(filePath))
    
  } else {
    
    if ( verbose ) {
      if( !newDownload )
        message("Could not find local data.")
      message(paste("Downloading and saving data to", filePath))
    }
    
    # If local data does not exist, download and return it.
    rawsObject <- fw13_createTimeseriesObject(nwsID = nwsID, meta = meta, baseUrl = baseUrl)
    
    # Save this object so it may be loaded in the future
    save(rawsObject, file = filePath)
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(rawsObject)
  
}
