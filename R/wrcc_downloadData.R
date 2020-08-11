#' @export
#' @import MazamaCoreUtils
#'
#' @title Download RAWS data
#'
#' @param stationID Station identifier (will be upcased).
#' @param startdate Desired start date (integer or character representing YYYYMMDD[HH]).
#' @param enddate Desired end date (integer or character representing YYYYMMDD[HH]).
#' @param baseUrl Base URL for data queries.
#' 
#' @description Request data from a particular station for the desired time period.
#' Data are returned as a single character string containing the RAWS output.
#'
#' Station identifiers can be found at https://raws.dri.edu/.
#' 
#' @return String containing RAWS data.
#' 
#' @references \href{https://raws.dri.edu/}{RAWS USA Climate Archive}
#' @examples
#' \dontrun{
#' library(RAWSmet)
#' 
#' fileString <- wrcc_downloadData(stationID = 'waWENU')
#' print(readr::read_lines(fileString)[1:10])
#' }

wrcc_downloadData <- function(
  stationID = NULL,
  startdate = strftime(lubridate::now(tzone = "UTC"),"%Y%m0101", tz = "UTC"),
  enddate = strftime(lubridate::now(tzone = "UTC"),"%Y%m%d23", tz = "UTC"),
  baseUrl = "https://wrcc.dri.edu/cgi-bin/wea_list2.pl"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  stopIfNull(stationID)

  wrccID <- stationID
  
  # Strip off the stateCode portion
  if ( stringr::str_count(wrccID) == 6 )
    wrccID <- stringr::str_sub(wrccID, 3, 6)
  
  # ----- Request parameters ---------------------------------------------------
  
  # Get UTC times
  starttime <- MazamaCoreUtils::parseDatetime(startdate, timezone = "UTC")
  endtime <- MazamaCoreUtils::parseDatetime(enddate, timezone = "UTC")
  
  # Create CGI parameters
  .params <- list(
    stn = toupper(wrccID),
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
    logger.trace("Downloading WRCC data for stationID %s", stationID)
  
  suppressWarnings({
    r <- httr::POST(baseUrl, body = .params)
  })
  
  # NOTE:  Log the error but just return an empty string (aka "No data")
  # NOTE:  for downstream processing.
  if ( httr::http_error(r) ) {
    if ( MazamaCoreUtils::logger.isInitialized() ) {
      logger.error("WRCC data service failed for stationID: %s", stationID)
      logger.error("WRCC data service failed with: %s", httr::content(r))
    }
    return("")
  }
  
  # No error so return the content (which might be an HTML formatted error message)
  fileString <- httr::content(r, 'text', encoding = 'UTF-8')
  
  return(fileString)
  
}
