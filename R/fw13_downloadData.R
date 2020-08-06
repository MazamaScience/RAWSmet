#' @export
#' @import MazamaCoreUtils
#'
#' @title Download RAWS FW13 data
#'
#' @param stationID Station identifier.
#' @param baseUrl Base URL for data queries.
#' 
#' @description Request data from a particular station. Data are returned as a 
#' single character string containing the RAWS output.
#'
#' Station identifiers can be found at https://cefa.dri.edu/raws/RAWSfw13list.xlsx.
#' 
#' @return String containing RAWS data.
#' 
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}
#' @examples
#' \dontrun{
#' library(RAWSmet)
#' 
#' fileString <- fw13_downloadData(stationID = 500742)
#' print(readr::read_lines(fileString)[1:10])
#' }

fw13_downloadData <- function(
  stationID = NULL,
  baseUrl = "https://cefa.dri.edu/raws/fw13/"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(stationID)
  
  suppressWarnings({
    stationID <- as.numeric(stationID)
  })
  
  if ( is.na(stationID) )
    stop("Station ID must be numeric or able to be coereced to numeric.")
  
  # ----- Request parameters ---------------------------------------------------
  
  # Format URL
  fw13Url <- paste0(baseUrl, stationID, ".fw13")
  
  # ----- Download data --------------------------------------------------------
  
  if ( MazamaCoreUtils::logger.isInitialized() )
    logger.trace("Downloading RAWS data for stationID %s", stationID)
  
  suppressWarnings({
    r <- httr::GET(fw13Url)
  })
  
  # NOTE:  Log the error but just return an empty string (aka "No data")
  # NOTE:  for downstream processing.
  if ( httr::http_error(r) ) {
    if ( MazamaCoreUtils::logger.isInitialized() ) {
      logger.error("RAWS data service failed for stationID: %s", stationID)
      logger.error("RAWS data service failed with: %s", httr::content(r))
    }
    return("")
  }
  
  # No error so return the content (which might be an HTML formatted error message)
  fileString <- httr::content(r, 'text', encoding = 'UTF-8')
  
  return(fileString)
  
}
