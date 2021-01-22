#' @export
#' @importFrom rlang .data
#' 
#' @title Date filtering for a list of \emph{raws_timeseries} objects
#' 
#' @param rawsList list of \emph{raws_timeseries} object.
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param days Number of days to include in the filterDate interval.
#' @param weeks Number of weeks to include in the filterDate interval.
#' @param timezone Olson timezone used to interpret dates.
#' 
#' @return A list of subsets of the elements of the given list of 
#' \emph{raws_timeseries} objects filtered by date.
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
#' @seealso \link{raws_filterDate}
#' @seealso \link{rawsList_filter}
#' @examples
#' \donttest{
#' library(RAWSmet)
#' 
#' rawsList <- example_fw13List
#' 
#' data201708 <- 
#'   rawsList %>% 
#'   rawsList_filterDate(
#'     startdate = 20170801, 
#'     enddate = 20170901, 
#'     timezone = "America/Los_Angeles"
#'   )
#'   
#' head(data201708)
#' }
#'
rawsList_filterDate <- function(
  rawsList = NULL, 
  startdate = NULL, 
  enddate = NULL, 
  days = NULL, 
  weeks = NULL,
  timezone = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(rawsList)
  
  for ( id in names(rawsList) ) {
    if ( !raws_isRaws(rawsList[[id]]) )
      stop(sprintf("Element '%s' in 'rawsList' is not a valid 'raws_timeseries' object.", id))
    if ( raws_isEmpty(rawsList[[id]]) )
      stop(sprintf("Element '%s' in 'rawsObject' has no data.", id))
    
    # Remove any duplicate data records
    rawsList[[id]] <- raws_distinct(rawsList[[id]])
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

  # ----- Filter each element in "rawsList" ---------------------------------
  
  rawsList <- rawsList %>% purrr::map(~ raws_filterDate(.x, startdate, enddate, days))
  
  # ----- Return ---------------------------------------------------------------
  
  return(rawsList)
  
}
