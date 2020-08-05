#' @export
#' @importFrom rlang .data
#' @import MazamaCoreUtils 
#'
#' @title Obtain RAWS FW13 data and parse into a tibble
#'
#' @param stationID Station identifier.
#' @param startdate Desired start date (integer or character representing YYYYMMDD[HH]).
#' @param enddate Desired end date (integer or character representing YYYYMMDD[HH]).
#' @param baseUrl Base URL for data queries.
#'
#' @return Raw tibble of RAWS data.
#'
#' @description Obtains station data from a webservice and converts
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
#' tbl <- fw13_createRawDataframe(stationID = 500742)
#' dplyr::glimpse(tbl)
#' }
#'
#' @seealso \code{\link{raws_downloadData}}
#' @seealso \code{\link{raws_parseData}}
#'
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}

fw13_createRawDataframe <- function(
  stationID = NULL,
  startdate = strftime(lubridate::now(tzone = "UTC"), "%Y-%m-01 01:00:00 %Z", tz = "UTC"),
  enddate = strftime(lubridate::now(tzone = "UTC"), "%Y-%m-%d 23:00:00 %Z", tz = "UTC"),
  baseUrl = "https://cefa.dri.edu/raws/fw13/"
) {
  
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(stationID)
  
  suppressWarnings({
    stationID <- as.numeric(stationID)
  })
  
  if ( is.na(stationID) )
    stop("Could not coerce station ID to numeric")
  
  # TODO: check if start/end dates are valid datetimes
  
  # ----- Download/parse data --------------------------------------------------
  
  # Read in FW13 data
  fileString <- fw13_downloadData(stationID, baseUrl)
  
  # TODO: Catch HTML errors
  if( fileString == "" )
    stop("Could not find data for the given station ID.")
  
  # Read fwf raw data into a tibble
  
  # Set these manually. The website that stores this information times out often.
  widths = c(3, 6, 8, 4, 1, 1, 3, 3, 3, 3, 2, 3, 3, 3, 3, 2, 5, 1, 2, 2, 1, 1 , 1, 4, 3, 3, 1)
  col_types <- "cnnncnnnnnnnnnnnncnnnnnnnnc"
  col_names = c(letters, "aa")
  
  col_positions <- readr::fwf_widths(
    widths = widths,
    col_names = col_names
  )
  
  # Read in raw data
  df <- readr::read_fwf(
    file = fileString, 
    col_positions = col_positions, 
    col_types = col_types
  )
  
  # Make observation times machine readable
  df <- df %>%
    dplyr::mutate(d = replace(.data$d, nchar(.data$d) == 3, paste0("0", .data$d[nchar(.data$d) == 3]))) %>%
    dplyr::mutate(d = replace(.data$d, nchar(.data$d) == 1, "0000"))
  
  # Add simpler datestamp column
  datestamp <- paste0(df$c, df$d)
  
  df$datetime <- MazamaCoreUtils::parseDatetime(datestamp, timezone = "UTC")
  
  # Get observations in specified time range
  df <- df %>%
    dplyr::filter(strftime(.data$datetime, tz = 'UTC') >= startdate & strftime(.data$datetime, tz = 'UTC') <= enddate)
  
  # ----- Return ---------------------------------------------------------------
  
  return(df)
  
}
