#' @export
#' @import MazamaCoreUtils
#'
#' @title Download RAWS data
#'
#' @param unitID station identifier (will be upcased)
#' @param startdate desired start date (integer or character representing YYYYMMDD[HH])
#' @param enddate desired end date (integer or character representing YYYYMMDD[HH])
#' @param baseUrl base URL for data queries
#' @description Request data from a particular station for the desired time period.
#' Data are returned as a single character string containing the RAWS output.
#'
#' Monitor unitIDs can be found at https://raws.dri.edu/.
#' 
#' @return String containing RAWS data.
#' 
#' @references \href{https://raws.dri.edu/}{RAWS USA Climate Archive}
#' @examples
#' \dontrun{
#' library(RAWSmet)
#' 
#' fileString <- raws_downloadData(unitID = 'WENU')
#' print(readr::read_lines(fileString)[1:10])
#' }

raws_downloadData <- function(
  unitID = NULL,
  startdate = strftime(lubridate::now(tzone = "UTC"),"%Y%m0101", tz = "UTC"),
  enddate = strftime(lubridate::now(tzone = "UTC"),"%Y%m%d23", tz = "UTC"),
  baseUrl = "https://wrcc.dri.edu/cgi-bin/wea_list2.pl"
) {
  
  if ( MazamaCoreUtils::logger.isInitialized() )
    logger.debug(" ----- wrcc_downloadData() ----- ")
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( is.null(unitID) ) {
    if ( MazamaCoreUtils::logger.isInitialized() )
      logger.error("Required parameter 'unitID' is missing")
    stop(paste0("Required parameter 'unitID' is missing"))
  }
  
  # Get UTC times
  starttime <- MazamaCoreUtils::parseDatetime(startdate, timezone = "UTC")
  endtime <- MazamaCoreUtils::parseDatetime(enddate, timezone = "UTC")
  
  # Create CGI parameters
  .params <- list(
    stn = toupper(unitID),
    smon = strftime(starttime,"%m", tz = "UTC"),
    sday = strftime(starttime,"%d", tz = "UTC"),
    syea = strftime(starttime,"%y", tz = "UTC"),
    emon = strftime(endtime,"%m", tz = "UTC"),
    eday = strftime(endtime,"%d", tz = "UTC"),
    eyea = strftime(endtime,"%y", tz = "UTC"),
    'Submit Info' = 'Submit Info',
    dfor = '04',
    src = 'W',
    miss = '08',
    flag = 'N',
    Dfmt = '01',
    Tfmt = '01',
    Head = '01',
    Deli = '01',
    unit = 'M',
    WsMon = '01',
    WsDay = '01',
    WeMon = '12',
    WeDay = '12',
    WsHou = '00',
    WeHou = '24',
    .cgifields = c('unit', 'flag', 'srce')
  )
  
  # ----- Download data --------------------------------------------------------
  
  if ( MazamaCoreUtils::logger.isInitialized() )
    logger.trace("Downloading WRCC data for unitID %s", unitID)
  
  suppressWarnings({
    r <- httr::POST(baseUrl, body = .params)
  })
  
  # NOTE:  Log the error but just return an empty string (aka "No data")
  # NOTE:  for downstream processing.
  if ( httr::http_error(r) ) {
    if ( MazamaCoreUtils::logger.isInitialized() ) {
      logger.error("WRCC data service failed for unitID: %s", unitID)
      logger.error("WRCC data service failed with: %s", httr::content(r))
    }
    return("")
  }
  
  # No error so return the content (which might be an HTML formatted error message)
  fileString <- httr::content(r, 'text', encoding = 'UTF-8')
  
  return(fileString)
  
}
