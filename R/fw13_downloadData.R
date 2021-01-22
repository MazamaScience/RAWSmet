#' @export
#' @import MazamaCoreUtils
#'
#' @title Download RAWS FW13 data
#'
#' @param nwsID NWS RAWS station identifier.
#' @param baseUrl Base URL for data queries.
#' 
#' @return String containing RAWS data.
#' 
#' @description Request data for a particular station. Data are returned as a 
#' single character string containing the RAWS output.
#'
#' Station identifiers can be found at https://cefa.dri.edu/raws/RAWSfw13list.xlsx
#' and can be downloaded with \code{\link{fw13_createMeta}}.
#' 
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}
#' 
#' @examples
#' \dontrun{
#' library(RAWSmet)
#' 
#' fileString <- fw13_downloadData(nwsID = 500742)
#' print(readr::read_lines(fileString)[1:10])
#' }

fw13_downloadData <- function(
  nwsID = NULL,
  baseUrl = "https://cefa.dri.edu/raws/fw13/"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(nwsID)
  
  # Guarantee it is zero padded six characters
  nwsID <- sprintf("%06s", as.character(nwsID))
  
  # ----- Request parameters ---------------------------------------------------
  
  # Format URL
  url <- paste0(baseUrl, nwsID, ".fw13")
  
  # ----- Download data --------------------------------------------------------
  
  if ( MazamaCoreUtils::logger.isInitialized() )
    logger.trace("Downloading RAWS data for nwsID %s", nwsID)
  
  suppressWarnings({
    r <- httr::GET(url)
  })
  
  # NOTE:  Log the error but just return an empty string (aka "No data")
  # NOTE:  for downstream processing.
  if ( httr::http_error(r) ) {
    if ( MazamaCoreUtils::logger.isInitialized() ) {
      logger.warn("RAWS data service failed for nwsID: '%s'", nwsID)
      logger.warn("RAWS data service failed with: %s", httr::content(r))
    }
    return("")
  }
  
  # No error so return the content (which might be an HTML formatted error message)
  fileString <- httr::content(r, 'text', encoding = 'UTF-8')
  
  return(fileString)
  
}
