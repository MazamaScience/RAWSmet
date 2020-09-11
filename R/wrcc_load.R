#' @export
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' 
#' @title Load WRCC RAWS timeseries object from a local directory
#'
#' @param wrccID RAWS station identifier (will be upcased)
#' @param meta Tibble of FW13 station metadata.
#' @param year Year to access station data for.
#' @param baseUrl Base URL for data queries.
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @return Timeseries object with 'meta' and 'data'.
#'
#' @description Loads WRCC station metadata and data from the \code{rawsDataDir}. If the
#' data is not in this directory, this will download and save the data. 
#' 
#' @examples
#' \dontrun{
#' library(RAWSmet)
#' 
#' setRawsDataDir("~/Data/RAWS/")
#'
#' stationMeta <- wrcc_createMetadata(stateCode = "WA", wrccIDs = "waWENU")
#' meta <- wrcc_load(wrccID = "waWENU", meta = stationMeta)
#' 
#' dplyr::glimpse(meta)
#' }
#'
#' @seealso \code{wrcc_createTimeseriesObject}
#' @seealso \code{setRawsDataDir}
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}

wrcc_load <- function(
  wrccID = NULL,
  meta = NULL,
  year = NULL,
  baseUrl = "https://wrcc.dri.edu/cgi-bin/wea_list2.pl",
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(wrccID)
  MazamaCoreUtils::stopIfNull(year)
  
  if ( !is.numeric(year) ) {
    stop("Parameter 'year' must be numeric.")
  }
  
  dataDir <- getRawsDataDir()
  
  # ----- Check for local data -------------------------------------------------
  
  fileName = sprintf("wrcc_%s_%d.rda", wrccID, year)
  filePath = file.path(dataDir, fileName)
  
  if ( file.exists(filePath) ) {
    
    if ( verbose ) {
      message(sprintf("Loading data from %s", filePath))
    }
    
    # If local data exists, load and return it.
    rawsObject <- get(load(filePath))
    
  } else {
    
    if ( verbose ) {
      message("Could not find local data.")
      message(paste("Downloading and saving data to", filePath, "."))
    }
    
    # If local data does not exist, download and return it.
    rawsObject <- wrcc_createTimeseriesObject(wrccID = wrccID, meta = meta, baseUrl = baseUrl)
    
    # Temp solution until raws_filter~() functions are created
    rawsObject$data <- rawsObject$data %>% dplyr::filter(stringr::str_sub(.data$datetime, 0, 4) == year)
    
    # Save this object so it may be loaded in the future
    save(rawsObject, file = filePath)
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(rawsObject)
  
}
