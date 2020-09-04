#' @export
#' 
#' @name raws_isRaws
#' @title Test for correct structure for a \emph{raws_timeseries} object
#' 
#' @param rawsObject \emph{raws_timeseries} object
#' 
#' @description The \code{rawsObject} is checked for the 'raws_timeseries' class name
#' and presence of core \code{meta} and \code{data} columns.
#' 
#' Core \code{meta} columns include:
#' 
#' \itemize{
#'   \item{\code{nwsID} -- NWS station identifier (for FW13 data)}
#'   \item{\code{wrccID} -- WRCC station identifier (for WRCC data)}
#'   \item{\code{countryCode} -- ISO 3166-1 alpha-2}
#'   \item{\code{stateCode} -- ISO 3166-2 alpha-2}
#'   \item{\code{siteName} -- English language station name}
#'   \item{\code{agency} -- responsible agency (in WRCC data)}
#'   \item{\code{longitude} -- decimal degrees E}
#'   \item{\code{latitude} -- decimal degrees N}
#'   \item{\code{elevation} -- elevation of station in m}
#'   \item{\code{timezone} -- Olson time zone}
#' }
#' 
#' Core \code{data} columns include:
#' 
#' \itemize{
#'   \item{\code{datetime} -- measurement time (UTC)}
#'   \item{\code{temperature} -- temperature (C)}
#'   \item{\code{humidity} -- humidity (\%)}
#'   \item{\code{windSpeed} -- wind speed (m/s)}
#'   \item{\code{windDirection} -- wind direction (degrees)}
#'   \item{\code{maxGustSpeed} -- speed of max gust (m/s)}
#'   \item{\code{maxGustDirection} -- direction of max gust (degrees)}
#'   \item{\code{precipitation} -- precipitation (mm/h)}
#'   \item{\code{solarRadiation} -- solar radiation (W/m^2)}
#' }
#' 
#' @return \code{TRUE} if \code{rawsObject} has the correct structure, 
#' \code{FALSE} otherwise.
#' 
#' @examples 
#' \dontrun{
#' library(RAWSmet)
#' 
#' setRawsDataDir("~/Data/RAWS")
#' 
#' rawsObj <- fw13_load(451702, year = 2005)
#' 
#' raws_isRaws(rawsObj)
#' }
#' 
raws_isRaws <- function(
  rawsObject = NULL
) {
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(rawsObject)
  
  if ( !("raws_timeseries" %in% class(rawsObject)) )
    return(FALSE)
  if ( !("meta" %in% names(rawsObject)) )
    return(FALSE)
  if ( !("data" %in% names(rawsObject)) )
    return(FALSE)
  
  requiredNamesMeta <- c('nwsID', 'wrccID', 'siteName', 
                         'longitude', 'latitude', 'elevation', 
                         'agency', 'countryCode', 'stateCode', 'timezone')
  
  if ( !all(requiredNamesMeta %in% names(rawsObject$meta)) )
    return(FALSE)
  
  requiredNamesData <- c('datetime', 'temperature', 'humidity', 'windSpeed', 
                         'windDirection', 'maxGustSpeed', 'maxGustDirection',
                         'precipitation', 'solarRadiation')
  
  if ( !all(requiredNamesData %in% names(rawsObject$data)) )
    return(FALSE)
  
  if ( any(duplicated(rawsObject$data$datetime)) )
    warning("Duplicate timesteps found in 'raws_timeseries' object.")
  
  # Nothing failed so return TRUE
  return(TRUE)
  
}


#' @export
#' 
#' @name raws_isEmpty
#' @title Test for an epty \emph{raws_timeseries} object
#' 
#' @param rawsObject \emph{raws_timeseries} object
#' 
#' @description Conveniently tests if a given \emph{raws_timeseries} object
#' is empty. 
#' 
#' @return \code{TRUE} if no data exists in the \code{raws_timeseries} object.
#' \code{FALSE} otherwise
#' 
#' @examples 
#' \dontrun{
#' library(RAWSmet)
#' 
#' setRawsDataDir("~/Data/RAWS")
#' 
#' rawsObj <- fw13_load(451702, year = 2005)
#' 
#' raws_isEmpty(rawsObj)
#' }
#' 
raws_isEmpty <- function(
  rawsObject = NULL
) {
  if ( !raws_isRaws(rawsObject) )
    stop("Parameter 'rawsObject' is not a valid 'raws_timeseries' object.")
  return( nrow(rawsObject$data) == 0 )
}


#' @export
#' @importFrom rlang .data
#' 
#' @name raws_distinct
#' @title Retain only distinct data records in \code{rawsObject$data}
#' 
#' @param rawsObject \emph{raws_timeseries} object
#' 
#' @description Guarantees that records in a \emph{raws_timeseries} object
#' are distinct and are arranged in \code{datetime} order.
#' 
#' @return A \emph{raws_timeseries} object with no duplicated data records.
#' 
raws_distinct <- function(
  rawsObject = NULL
) {
  
  if ( !raws_isRaws(rawsObject) )
    stop("Parameter 'rawsObject' is not a valid 'raws_timeseries' object.")
  
  rawsObject$data <-
    rawsObject$data %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$datetime)
  
  return(rawsObject)
  
}


#' @name raws_extract
#' @title Extract dataframes from \emph{raws_timeseries} objects
#' 
#' @description These functions are convenient wrappers for extracting the dataframes that
#' comprise a \emph{raws_timeseries} object. These functions are designed to be useful when
#' manipulating data in a pipeline chain using \code{\%>\%}.
#' 
#' @param rawsObject \emph{raws_timeseries} object to extract dataframe from
#' 
#' @return A dataframe from the given \emph{raws_timeseries} object
#' 
#' @examples
#' \dontrun{
#' library(RAWSmet)
#' 
#' setRawsDataDir("~/Data/RAWS")
#' setSpatialDataDir("~/Data/Spatial")
#' 
#' fw13_load(451702, year = 2005) %>%
#'   raws_extractData(forOpenair = T) %>%
#'   openair::timePlot(
#'     pollutant = "temperature"
#'   )
#'    
#' fw13_load(451702, year = 2005) %>%
#'   raws_extractMeta() %>%
#'   head()
#' }
#' 
#' @rdname raws_extract
#' 
NULL


#' @export
#' @param forOpenair Logical instructions to clean data for use with openair
#' @rdname raws_extract
#' 
raws_extractData <- function(
  rawsObject = NULL,
  forOpenair = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(rawsObject)
  
  if ( !("raws_timeseries" %in% class(rawsObject)) )
    stop("Parameter 'rawsObject' is not a valid raws_timeseries object")
  
  # ----- Set up data for openair ----------------------------------------------
  
  # Duplicate 'datetime' column as 'date'
  if( forOpenair ) {
    rawsObject$data$date <- rawsObject$data$datetime
  }
  
  # ----- Return ----------------------------------------------------------------
  
  return(rawsObject$data)
  
}


#' @export
#' @rdname raws_extract
#' 
raws_extractMeta <- function(
  rawsObject = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(rawsObject)
  
  if ( !("raws_timeseries" %in% class(rawsObject)) )
    stop("Parameter 'rawsObject' is not a valid raws_timeseries object")
  
  # ----- Return ----------------------------------------------------------------
  
  return(rawsObject$meta)
  
}