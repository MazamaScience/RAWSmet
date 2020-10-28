#' @export
#' @importFrom rlang .data
#' 
#' @title Date filtering for \emph{raws_timeseries} objects
#' 
#' @param rawsObject \emph{raws_timeseries} object.
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param days Number of days to include in the filterDate interval.
#' @param weeks Number of weeks to include in the filterDate interval.
#' @param timezone Olson timezone used to interpret dates.
#' 
#' @description Subsets a \emph{raws_timeseries} object by date. This function
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
#' @return A subset of the given \emph{raws_timeseries} object.
#' 
#' @seealso \link{raws_filter}
#' @examples
#' \donttest{
#' library(RAWSmet)
#' 
#' rawsObject <- example_fw13SaddleMountain
#' 
#' data201708 <- rawsObject %>% raws_filterDate(20170801, 20170901, timezone = "America/Los_Angeles")
#' head(data201708$data)
#' }
#'
raws_filterDate <- function(
  rawsObject = NULL, 
  startdate = NULL, 
  enddate = NULL, 
  days = NULL, 
  weeks = NULL,
  timezone = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(rawsObject)
  
  if ( !raws_isRaws(rawsObject) )
    stop("Parameter 'rawsObject' is not a valid 'raws_timeseries' object.")
  
  if ( raws_isEmpty(rawsObject) )
    stop("Parameter 'rawsObject' has no data.")
  
  # Remove any duplicate data records
  rawsObject <- raws_distinct(rawsObject)
  
  if ( is.null(startdate) && !is.null(enddate) )
    stop("At least one of 'startdate' or 'enddate' must be specified")
  
  # Timezone determination precedence assumes that if you are passing in
  # POSIXct times then you know what you are doing.
  #   1) get timezone from startdate if it is POSIXct
  #   2) use passed in timezone
  #   3) get timezone from rawsObject
  
  if ( lubridate::is.POSIXt(startdate) ) {
    timezone <- lubridate::tz(startdate)
  } else {
    if ( is.null(timezone) ) {
      timezone <- rawsObject$meta$timezone
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
  
  # ----- Subset the "rawsObject" object ----------------------------------------------
  
  if (dateRange[1] > rawsObject$data$datetime[length(rawsObject$data$datetime)] |
      dateRange[2] < rawsObject$data$datetime[1]) {
    
    warning("rawsObject does not contain requested date range")
    data <- rawsObject$data[0,]

  } else {
    
    data <- 
      rawsObject$data %>%
      dplyr::filter(.data$datetime >= dateRange[1]) %>%
      dplyr::filter(.data$datetime < dateRange[2])
    
  }
  
  rawsObject$data <- data
  
  # ----- Return ---------------------------------------------------------------
  
  # Remove any duplicate data records
  rawsObject <- raws_distinct(rawsObject)
  
  return(rawsObject)
  
}
