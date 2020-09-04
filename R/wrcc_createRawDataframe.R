#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Obtain RAWS data and parse into a tibble
#'
#' @param wrccID Station identifier (will be upcased).
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
#' tbl <- wrcc_createRawDataframe(wrccID = 'waWENU')
#' dplyr::glimpse(tbl)
#' }
#'
#' @seealso \code{\link{wrcc_downloadData}}
#' @seealso \code{\link{wrcc_parseData}}
#'
#' @references \href{https://raws.dri.edu/}{RAWS USA Climate Archive}

wrcc_createRawDataframe <- function(
  wrccID = NULL,
  startdate = strftime(lubridate::now(tzone = "UTC"), "%Y%m0101", tz = "UTC"),
  enddate = strftime(lubridate::now(tzone = "UTC"), "%Y%m%d23", tz = "UTC"),
  baseUrl = "https://wrcc.dri.edu/cgi-bin/wea_list2.pl"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(wrccID)
  
  if ( length(wrccID) > 1 )
    stop("Parameter 'wrccID' must be of length 1")
  
  # ----- Download/parse data --------------------------------------------------
  
  # Read in RAWS data
  fileString <- wrcc_downloadData(wrccID, startdate, enddate, baseUrl)
  
  # Catch HTML errors
  if ( stringr::str_detect(fileString, "Access to WRCC historical raws data is limited") ||
       stringr::str_detect(fileString, stringr::regex("^\\s+.\\w.+\\w.+(\\n\\n)")) ) {
    stop("Could not access data for that time range or wrccID")
  }
  
  # Read fwf raw data into a tibble
  tbl <- wrcc_parseData(fileString)

  # ----- Return ---------------------------------------------------------------
  
  return(tbl)
  
}
