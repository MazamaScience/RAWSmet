#' @export
#' @importFrom rlang .data
#' @importFrom dplyr all_of
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Obtain RAWS data and create a timeseries object.
#'
#' @param nwsID Station identifier found in 'meta'.
#' @param meta Tibble of RAWS metadata containing \code{nwsID}.
#' @param baseUrl Base URL for data queries.
#'
#' @return Timeseries object with 'meta' and 'data'.
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
#'  \item{standardized names}
#' }
#'
#' @examples
#' \dontrun{
#' library(RAWSmet)
#'
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial")
#' 
#' nws_500726 <- fw13_createTimeseriesObject(nwsID = 500726)
#' 
#' }
#'
#' @seealso \code{\link{fw13_createMetadata}}
#' @seealso \code{\link{fw13_createRawDataframe}}
#'
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}
#' @references \href{https://fam.nwcg.gov/fam-web/weatherfirecd/13.htm}{FW13 Data Format}

fw13_createTimeseriesObject <- function(
  nwsID = NULL,
  meta = NULL,
  baseUrl = "https://cefa.dri.edu/raws/fw13/"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(nwsID)
  
  # Guarantee it is zero padded six characters
  nwsID <- sprintf("%06s", as.character(nwsID))
  
  # ----- Create 'meta' --------------------------------------------------------
  
  if ( is.null(meta) )
    meta <- fw13_createMetadata()
  
  # Subset to a single record
  meta <- dplyr::filter(meta, nwsID == !!nwsID)
  
  # ----- Create 'data' --------------------------------------------------------
  
  # * Download/parse -----
  
  tbl <- fw13_createRawDataframe(
    nwsID = nwsID,
    baseUrl = baseUrl
  )

  # * Conversion to metric -----
  
  # measurementType: 1 = US, 2 = metric
  
  # Convert temperature from deg F to deg C
  tempConvert <- function(type, temp) { 
    if (type == 1) 
      return(5/9 * (temp - 32)) 
    else 
      return(temp)
  }
  
  temperature <- mapply(tempConvert, tbl$measurementType, tbl$dryBulbTemp)
  
  # Convert wind speeds from miles-per-hour to meters-per-second
  speedConvert <- function(type, speed) {
    if (type == 1)
      return(1609.344 * speed * 1/3600)
    else
      return(speed)
  }
  # TODO:  Change all wind speeds to meters per second
  # speed <- mapply(speedConvert, tbl$measurementType, tbl$avWindSpeed)
  
  # TODO:  Change precipitation to millimeters per hour
  
  
  # * Harmonize ----
  
  # Define the set of standard columns that will always be returned
  standardColumns <- c(
    "datetime", "temperature", "humidity", "fuelMoisture",
    "windSpeed", "windDirection", "maxGustSpeed", "maxGustDirection",
    "precipitation", "solarRadiation",
    "minHumidity", "maxHumidity"
  )
  
  # TODO:  Use metric versions of data
  data <-
    tbl %>%
    dplyr::mutate(
      "datetime" = paste0(.data$observationDate, .data$observationTime),
      "temperature" = temperature,
      "humidity" = (.data$minRelHumidity + .data$maxRelHumidity)/2,
      "fuelMoisture" = .data$fuelMoisture,
      "windSpeed" = .data$avWindSpeed,
      "windDirection" = .data$windDirection,
      "maxGustSpeed" = .data$maxGustSpeed,
      "maxGustDirection" = .data$maxGustDirection,
      "precipitation" = .data$precipAmount,
      "solarRadiation" = .data$solarRadiation,
      "minHumidity" = .data$minRelHumidity,
      "maxHumidity" = .data$maxRelHumidity,
      "monitorType" = "FW13"
    ) %>%
    dplyr::select(all_of(standardColumns))
  
  # * Convert datetime to UTC ----

  UTC_offset <-
    MazamaSpatialUtils::SimpleTimezones@data %>%
    dplyr::filter(.data$timezone == meta$timezone) %>%
    dplyr::pull("UTC_offset")
  
  # NOTE:  The 'datetime' column is "local standard time all-year-round" for
  # NOTE:  which no timezone exists. So we have to convert it first to UTC
  # NOTE:  and then shift it by the UTC offset.
  
  UTC_time <-
    MazamaCoreUtils::parseDatetime(data$datetime, timezone = "UTC") +
    lubridate::dhours(UTC_offset)
  
  data$datetime <- UTC_time
  
  # ----- Return ---------------------------------------------------------------
  
  # Combine meta and data dataframes into a list
  raws <- list(meta = meta, data = data)
  class(raws) <- c("raws_timeseries", class(raws))
  
  return(raws)
  
}
