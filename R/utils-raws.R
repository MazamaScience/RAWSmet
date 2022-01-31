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
#'   \item{\code{deviceDeploymentID} -- unique identifier (see \pkg{MazmaLocationUtils})}
#'   \item{\code{deviceID} -- device identifier}
#'   \item{\code{locationID} -- location identifier (see \pkg{MazmaLocationUtils})}
#'   \item{\code{locationName} -- English language name}
#'   \item{\code{longitude} -- decimal degrees E}
#'   \item{\code{latitude} -- decimal degrees N}
#'   \item{\code{elevation} -- elevation of station in m}
#'   \item{\code{countryCode} -- ISO 3166-1 alpha-2}
#'   \item{\code{stateCode} -- ISO 3166-2 alpha-2}
#'   \item{\code{timezone} -- Olson time zone}
#'   \item{\code{nwsID} -- NWS station identifier (for FW13 data)}
#'   \item{\code{wrccID} -- WRCC station identifier (for WRCC data)}
#'   \item{\code{agencyName} -- responsible agency (in WRCC data)}
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
#' \donttest{
#' library(RAWSmet)
#'
#' raws_isRaws(example_fw13_Saddle_Mountain)
#' raws_isRaws(example_wrcc_Saddle_Mountain)
#' }
#'
raws_isRaws <- function(
  rawsObject = NULL
) {
  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(rawsObject)

  if ( !("raws_timeseries" %in% class(rawsObject)) )
    return(invisible(FALSE))

  # Check that it is a valid 'sts' object
  if ( !MazamaTimeSeries::sts_isValid(rawsObject) )
    return(invisible(FALSE))

  requiredNamesMeta <- c(
    "deviceDeploymentID",
    "deviceID",
    "locationID",
    "locationName",
    "longitude",
    "latitude",
    "elevation",
    "countryCode",
    "stateCode",
    "timezone",
    "nwsID",
    "wrccID",
    "agencyName"
  )

  if ( !all(requiredNamesMeta %in% names(rawsObject$meta)) )
    return(invisible(FALSE))

  requiredNamesData <- c(
    "datetime",
    "temperature",
    "humidity",
    "windSpeed",
    "windDirection",
    "maxGustSpeed",
    "maxGustDirection",
    "precipitation",
    "solarRadiation"
  )

  if ( !all(requiredNamesData %in% names(rawsObject$data)) )
    return(invisible(FALSE))

  # Nothing failed so return TRUE
  return(invisible(TRUE))

}


#' @export
#'
#' @name raws_isEmpty
#' @title Test for an empty \emph{raws_timeseries} object
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
#' \donttest{
#' library(RAWSmet)
#'
#' raws_isEmpty(example_fw13_Saddle_Mountain)
#' raws_isEmpty(example_wrcc_Saddle_Mountain)
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

  # Use function from MazamaTimeSeries
  rawsObject <- MazamaTimeSeries::sts_distinct(rawsObject)

  return(rawsObject)

}


#' @name raws_getDataframe
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
#' \donttest{
#' library(RAWSmet)
#'
#' data <-
#'   example_fw13_Saddle_Mountain %>%
#'   raws_getData(forOpenair = TRUE)
#'
#' # openair::timePlot(
#' #  data,
#' #  pollutant = "temperature"
#' # )
#'
#' example_fw13_Saddle_Mountain %>%
#'   raws_getMeta() %>%
#'   head()
#' }
#'
#' @rdname raws_getDataframe
#'
NULL


#' @export
#' @param forOpenair Logical instructions to clean data for use with openair
#' @rdname raws_getDataframe
#'
raws_getData <- function(
  rawsObject = NULL,
  forOpenair = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(rawsObject)

  if ( !("raws_timeseries" %in% class(rawsObject)) )
    stop("Parameter 'rawsObject' is not a valid raws_timeseries object")

  # ----- Set up data for openair ----------------------------------------------

  # Duplicate columns 'datetime', 'windSpeed', 'windDirection' as 'date', 'ws', 'wd'
  if( forOpenair ) {
    rawsObject$data$date <- rawsObject$data$datetime
    rawsObject$data$ws <- rawsObject$data$windSpeed
    rawsObject$data$wd <- rawsObject$data$windDirection
  }

  # ----- Return ----------------------------------------------------------------

  return(rawsObject$data)

}


#' @export
#' @rdname raws_getDataframe
#'
raws_getMeta <- function(
  rawsObject = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(rawsObject)

  if ( !("raws_timeseries" %in% class(rawsObject)) )
    stop("Parameter 'rawsObject' is not a valid raws_timeseries object")

  # ----- Return ----------------------------------------------------------------

  return(rawsObject$meta)

}

