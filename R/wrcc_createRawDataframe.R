#' @keywords WRCC
#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Obtain RAWS data and parse into a tibble
#'
#' @param startdate Desired start date (integer or character representing YYYYMMDD[HH]).
#' @param enddate Desired end date (integer or character representing YYYYMMDD[HH]).
#' @param unitID Station identifier (will be upcased).
#' @param clusterDiameter Diameter in meters used to determine the number of clusters (see \code{addClustering}).
#' @param baseUrl Base URL for data queries.
#' @param saveFile Optional filename where raw CSV will be written.
#' @param flagAndKeep Flag, rather then remove, bad data during the QC process.
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
#' tbl <- wrcc_createRawDataframe(20150701, 20150930, unitID = 'WENU')
#' dplyr::glimpse(tbl)
#' }
#'
#' @seealso \code{\link{wrcc_downloadData}}
#' @seealso \code{\link{wrcc_parseData}}
#'
#' @references \href{https://raws.dri.edu/}{RAWS USA Climate Archive}

wrcc_createRawDataframe <- function(
  startdate = strftime(lubridate::now(tzone = "UTC"),"%Y010100", tz = "UTC"),
  enddate = strftime(lubridate::now(tzone = "UTC"),"%Y%m%d23", tz = "UTC"),
  unitID = NULL,
  baseUrl = "https://wrcc.dri.edu/cgi-bin/wea_list2.pl"
) {
  
  logger.debug(" ----- wrcc_createRawDatafram() ----- ")
  
  # ----- Validate parameters ---------------------------------------------------
  
  if ( is.null(unitID) ) {
    logger.error("Required parameter 'unitID' is missing")
    stop(paste0("Required parameter 'unitID' is missing"))
  }
  
  # Read in RAWS data
  logger.trace("Downloading WRCC data ...")
  fileString <- wrcc_downloadData(startdate, enddate, unitID, baseUrl)
  
  # Read fwf raw data into a tibble
  logger.trace("Parsing data ...")
  tbl <- wrcc_parseData(fileString)
  
  # Add source of raw data
  tbl$rawSource <- "WRCC"
  
  # Return ---------------------------------------------------------------------

  return(tbl)
  
}
