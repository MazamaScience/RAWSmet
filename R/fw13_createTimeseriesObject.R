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
  
  # * Harmonize ----
  
  # Station can only have one 'monitorType'
  monitorType <- "FW13"
  
  # Define the set of standard columns that will always be returned
  standardColumns <- c(
    "datetime", "temperature", "humidity",
    "windSpeed", "windDirection", "maxGustSpeed", "maxGustDirection",
    "precipitation", "solarRadiation",
    "batteryVoltage"
  )
  
  data <-
    tbl %>%
    dplyr::mutate(
      "datetime" = paste0(.data$observationDate, .data$observationTime),
      
      
      # TODO:  FINISH THIS
      
      "temperature" = .data$AvAirTemp,
      "humidity" = .data$RelHumidty,
      "windSpeed" = .data$WindSpeed,
      "windDirection" = .data$WindDirec,
      "maxGustSpeed" = .data$MxGustSpeed,
      "maxGustDirection" = .data$DirMxGust,
      "precipitation" = .data$Precip,
      "solarRadiation" = .data$SolarRad,
      "batteryVoltage" = .data$BatteryVoltage,
      "fuelTemperature" = .data$FuelTemp,
      "fuelMoisture" = .data$AvFuelMoistr,
      "monitorType" = .data$monitorType
    ) %>%
    dplyr::select(all_of(standardColumns))
  
  
  
  
  
  
  # # Define the set of standard columns that will always be returned
  # standardColumns <- c(
  #   "LST_datestamp", "recordType", "nwsID", "observationDate", "observationTime",
  #   "observationType", "weatherCode", "dryBulbTemp", "atmosMoisture",
  #   "windDirection", "avWindSpeed", "fuelMoisture", "maxTemp", "minTemp",
  #   "maxRelHumidity", "minRelHumidity", "percipDuration", "percipAmount",
  #   "wetFlag", "herbaceousGreenness", "shrubGreenness", "moistureType",
  #   "measurementType", "seasonCode", "solarRadiation", "maxGustDirection",
  #   "maxGustSpeed", "snowFlag"
  # )
  # 
  # data <-
  #   tbl %>%
  #   dplyr::select(all_of(standardColumns))
  
  
  # * Convert datetime to UTC ----
  
  UTC_offset <-
    MazamaSpatialUtils::SimpleTimezones@data %>%
    dplyr::filter(.data$timezone == meta$timezone) %>%
    dplyr::pull("UTC_offset")
  
  # NOTE:  The 'datetime' column is "local standard time all-year-round" for
  # NOTE:  which no timezone exists. So we have to convert it first to UTC
  # NOTE:  and then shift it by the UTC offset.
  
  UTC_time <-
    MazamaCoreUtils::parseDatetime(data$LST_datestamp, timezone = "UTC") +
    lubridate::dhours(UTC_offset)
  
  data$datetime <- UTC_time
  
  # ----- Return ---------------------------------------------------------------
  
  # Combine meta and data dataframes into a list
  raws <- list(meta = meta, data = data)
  class(raws) <- c("raws_timeseries", class(raws))
  
  return(raws)
  
}
