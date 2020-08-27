#' @export
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' 
#' @title Load FW13 RAWS timeseries object from a local directory
#'
#' @param nwsID NWS RAWS station identifier.
#' @param meta Tibble of FW13 station metadata.
#' @param year Year to access station data for.
#' @param baseUrl Base URL for data queries.
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @return Timeseries object with 'meta' and 'data'.
#'
#' @description Loads FW13 station metadata and data from the \code{rawsDataDir}. If the
#' data is not in this directory, this will download and save the data. 
#' 
#' @examples
#' \dontrun{
#' library(RAWSmet)
#' 
#' setRawsDataDir("~/Data/RAWS/")
#'
#' stationMeta <- fw13_createMetadata()
#' meta <- fw13_load(nwsID = 451702, meta = stationMeta)
#' 
#' dplyr::glimpse(meta)
#' }
#'
#' @seealso \code{fw13_createTimeseriesObject}
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}

fw13_load <- function(
  nwsID = NULL,
  meta = NULL,
  year = NULL,
  baseUrl = "https://cefa.dri.edu/raws/fw13/",
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(nwsID)
  MazamaCoreUtils::stopIfNull(year)
  
  if ( !is.numeric(year) ) {
    stop("Parameter 'year' must be numeric.")
  }
  
  dataDir <- getRawsDataDir()
  
  # ----- Check for local data -------------------------------------------------
  
  fileName = sprintf("fw13_%s_%d.rda", nwsID, year)
  filePath = file.path(dataDir, fileName)
  
  if ( file.exists(filePath) ) {
    
    if( verbose ) {
      message("Loading data from local directory.")
    }
    
    # If local data exists, load and return it.
    rawsObject <- get(load(filePath))
    
  } else {
    
    if ( verbose ) {
      message("Could not find local data.")
      message(paste("Downloading and saving data to", filePath, "."))
    }
    
    # If local data does not exist, download and return it.
    rawsObject <- fw13_createTimeseriesObject(nwsID = nwsID, meta = meta, baseUrl = baseUrl)
    
    # Temp solution until raws_filter~() functions are created
    rawsObject$data <- rawsObject$data %>% dplyr::filter(stringr::str_sub(.data$datetime, 0, 4) == year)
    
    # Save this object so it may be loaded in the future
    save(rawsObject, file = filePath)
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(rawsObject)
  
}
