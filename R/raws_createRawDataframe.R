#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Obtain RAWS data and parse into a tibble
#'
#' @param unitID Station identifier (will be upcased).
#' @param startdate Desired start date (integer or character representing YYYYMMDD[HH]).
#' @param enddate Desired end date (integer or character representing YYYYMMDD[HH]).
#' @param baseUrl Base URL for data queries.
#'
#' @return Raw tibble of RAWS data.
#'
#' @description Obtains station data from a WRCC webservice and converts
#' it into a quality controlled, metadata enhanced "raw" tibble
#' ready for use with all \code{raw_~} functions.
#'
#' Steps involved include:
#'
#' \enumerate{
#'  \item{download data text}
#'  \item{parse data text}
#' }
#'
#' @examples
#' \dontrun{
#' library(RAWSmet)
#'
#' tbl <- raws_createRawDataframe(unitID = 'WENU')
#' dplyr::glimpse(tbl)
#' }
#'
#' @seealso \code{\link{raws_downloadData}}
#' @seealso \code{\link{raws_parseData}}
#'
#' @references \href{https://raws.dri.edu/}{RAWS USA Climate Archive}

raws_createRawDataframe <- function(
  unitID = NULL,
  startdate = strftime(lubridate::now(tzone = "UTC"),"%Y%m0101", tz = "UTC"),
  enddate = strftime(lubridate::now(tzone = "UTC"),"%Y%m%d23", tz = "UTC"),
  baseUrl = "https://wrcc.dri.edu/cgi-bin/wea_list2.pl"
) {
  
  if( MazamaCoreUtils::logger.isInitialized() )
    logger.debug(" ----- wrcc_createRawDatafram() ----- ")
  
  # ----- Validate parameters ---------------------------------------------------
  
  if ( is.null(unitID) && MazamaCoreUtils::logger.isInitialized() ) {
    logger.error("Required parameter 'unitID' is missing")
    stop(paste0("Required parameter 'unitID' is missing"))
  }
  
  # Read in RAWS data
  if( MazamaCoreUtils::logger.isInitialized() )
    logger.trace("Downloading WRCC data ...")

  fileString <- raws_downloadData(unitID, startdate, enddate, baseUrl)

  # Catch HTML errors
  if( stringr::str_detect(fileString, "Access to WRCC historical raws data is limited") ||
      stringr::str_detect(fileString, stringr::regex("^\\s+.\\w.+\\w.+(\\n\\n)")) ) {
    stop("Could not access data for that time range or unit ID")
  }
  
  if( MazamaCoreUtils::logger.isInitialized() )
    logger.trace("Parsing data ...")

  # Read fwf raw data into a tibble
  tbl <- raws_parseData(fileString)
  
  # Add source of raw data
  tbl$rawSource <- "WRCC"
  
  # ----- Return ----------------------------------------------------------------

  return(tbl)
  
}
