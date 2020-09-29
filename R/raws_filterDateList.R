#' @export
#' @importFrom rlang .data
#' 
#' @title Date filtering for a list of \emph{raws_timeseries} objects
#' 
#' @param rawsObjects list of \emph{raws_timeseries} object.
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param days Number of days to include in the filterDate interval.
#' @param weeks Number of weeks to include in the filterDate interval.
#' @param timezone Olson timezone used to interpret dates.
#' 
#' @description Subsets a list of \emph{raws_timeseries} objects by date. 
#' This function always filters to day-boundaries.
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
#' @return A list of subsets of the elements of the given list of 
#' \emph{raws_timeseries} objects filtered by date.
#' 
#' @seealso \link{raws_filterDate}
#' @seealso \link{raws_filterList}
#' @examples
#' \donttest{
#' library(RAWSmet)
#' 
#' rawsObjects <- example_fw13List
#' 
#' data201708 <- rawsObjects %>% raws_filterDateList(
#'                                  startdate = 20170801, 
#'                                  enddate = 20170901, 
#'                                  timezone = "America/Los_Angeles")
#' head(data201708)
#' }
#'
raws_filterDateList <- function(
  rawsObjects = NULL, 
  startdate = NULL, 
  enddate = NULL, 
  days = NULL, 
  weeks = NULL,
  timezone = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(rawsObjects)
  
  for ( id in names(rawsObjects) ) {
    if ( !raws_isRaws(rawsObjects[[id]]) )
      stop(sprintf("Element '%s' in 'rawsObjects' is not a valid 'raws_timeseries' object.", id))
    if ( raws_isEmpty(rawsObjects[[id]]) )
      stop(sprintf("Element '%s' in 'rawsObject' has no data.", id))
    
    # Remove any duplicate data records
    rawsObjects[[id]] <- raws_distinct(rawsObjects[[id]])
  }
  
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
      stop("Parameter 'timezone' must not be null.")
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

  # ----- Subset each element in "rawsObjects" ---------------------------------
  
  for ( id in names(rawsObjects) ) {
    
    # Check if each element contains the requested date range
    if (dateRange[1] > rawsObjects[[id]]$data$datetime[length(rawsObjects[[id]]$data$datetime)] |
        dateRange[2] < rawsObjects[[id]]$data$datetime[1])
      stop(sprintf("Element '%s' in 'rawsObjects' does not contain requested date range", id))
    
    # Filter each element by the requested dates
    rawsObjects[[id]]$data <- 
      rawsObjects[[id]]$data %>%
      dplyr::filter(.data$datetime >= dateRange[1]) %>%
      dplyr::filter(.data$datetime < dateRange[2])
    
    # Remove any duplicate data records
    rawsObjects[[id]] <- raws_distinct(rawsObjects[[id]])
  }
  # ----- Return ---------------------------------------------------------------
  
  return(rawsObjects)
  
}
