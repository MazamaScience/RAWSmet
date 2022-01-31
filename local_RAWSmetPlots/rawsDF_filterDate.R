#' @export
#' @importFrom rlang .data
#' 
#' @title Date filtering for \emph{rawsDF} objects
#' 
#' @param rawsDF \emph{raws_timeseries} object.
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param days Number of days to include in the filterDate interval.
#' @param weeks Number of weeks to include in the filterDate interval.
#' @param timezone Olson timezone used to interpret dates.
#' 
#' @return A subset of the given \emph{rawsDF} object.
#' 
#' @description Subsets a \emph{rawsDF} object by date. This function
#' always filters to day-boundaries.
#' 
#' Dates can be anything that is understood by \code{lubridate::ymd()}
#' including either of the following recommended formats:
#' 
#' \itemize{
#' \item{\code{"YYYYmmdd"}}
#' \item{\code{"YYYY-mm-dd"}}
#' }
#' 
#' @note The returned data will run from the beginning of \code{startdate} until
#' the \strong{beginning} of \code{enddate} -- \emph{i.e.} no values associated
#' with \code{enddate} will be returned.
#' 
#' @seealso \link{rawsDF_filter}
#' @examples
#' \donttest{
#' library(RAWSmet)
#' 
#' rawsObject <- example_fw13_Saddle_Mountain
#' rawsDF <- rawsObject %>% raws_toRawsDF()
#' 
#' data201708 <- rawsDF %>% rawsDF_filterDate(20170801, 20170901, timezone = "America/Los_Angeles")
#' head(data201708$data)
#' }
#'
rawsDF_filterDate <- function(
  rawsDF = NULL, 
  startdate = NULL, 
  enddate = NULL, 
  days = NULL, 
  weeks = NULL,
  timezone = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(rawsDF)
  
  if ( !rawsDF_isRawsDF(rawsDF) )
    stop("parameter 'rawsDF' is not a valid 'rawsDF' object")
  
  if ( nrow(rawsDF) == 0 )
    stop("Parameter 'rawsDF' has no data.")
  
  if ( is.null(startdate) && !is.null(enddate) )
    stop("At least one of 'startdate' or 'enddate' must be specified")
  
  # Timezone determination precedence assumes that if you are passing in
  # POSIXct times then you know what you are doing.
  #   1) get timezone from startdate if it is POSIXct
  #   2) use passed in timezone
  #   3) get timezone from rawsDF
  
  if ( lubridate::is.POSIXt(startdate) ) {
    timezone <- lubridate::tz(startdate)
  } else {
    if ( is.null(timezone) ) {
      timezone <- rawsDF$timezone
    }
  }
  
  # ----- Get the start and end times ------------------------------------------
  
  if ( !is.null(days) ) {
    days <- days
  } else if ( !is.null(weeks) ) {
    days <- weeks * 7
  } else {
    days <- 7 # default
  }
  
  dateRange <- MazamaCoreUtils::dateRange(
    startdate = startdate, 
    enddate = enddate, 
    timezone = timezone,
    unit = "sec",
    ceilingEnd = FALSE,
    days = days
  )
  
  # ----- Subset the "rawsDF" object ----------------------------------------------
  
  if (dateRange[1] > rawsDF$datetime[length(rawsDF$datetime)] |
      dateRange[2] < rawsDF$datetime[1]) {
    
    message(sprintf(
      "rawsDF (%s) does not contain requested date range", 
      rawsDF$locationName
    ))
    
    data <- rawsDF[0,]
    
  } else {
    
    data <- 
      rawsDF %>%
      dplyr::filter(.data$datetime >= dateRange[1]) %>%
      dplyr::filter(.data$datetime < dateRange[2])
    
  }
  
  rawsDF <- data
  
  # ----- Return ---------------------------------------------------------------
  
  return(rawsDF)
  
}
