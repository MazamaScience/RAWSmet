#' @export
#' @importFrom rlang .data
#' @importFrom dplyr all_of
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Obtain RAWS data and create a timeseries object.
#'
#' @param wrccID Station identifier found in 'meta'.
#' @param meta Tibble of RAWS metadata containing \code{wrccID}.
#' @param startdate Desired start date (integer or character representing YYYYMMDD[HH]).
#' @param enddate Desired end date (integer or character representing YYYYMMDD[HH]).
#' @param baseUrl Base URL for data queries.
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @return Timeseries object with 'meta' and 'data'.
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
#'  \item{standardized names}
#' }
#'
#' @examples
#' \dontrun{
#' library(RAWSmet)
#'
#' waWALD <- wrcc_createTimeseriesObject(wrccID = 'waWALD')
#' 
#' }
#'
#' @seealso \code{\link{wrcc_downloadData}}
#' @seealso \code{\link{wrcc_parseData}}
#'
#' @references \href{https://raws.dri.edu/}{RAWS USA Climate Archive}

wrcc_createTimeseriesObject <- function(
  wrccID = NULL,
  meta = NULL,
  startdate = strftime(lubridate::now(tzone = "UTC"), "%Y%m0101", tz = "UTC"),
  enddate = strftime(lubridate::now(tzone = "UTC"), "%Y%m%d23", tz = "UTC"),
  baseUrl = "https://wrcc.dri.edu/cgi-bin/wea_list2.pl",
  verbose = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  stopIfNull(wrccID)

  if ( length(wrccID) > 1 )
    stop("Parameter 'wrccID' must be of length 1")
  
  # ----- Create 'meta' --------------------------------------------------------
  
  if ( is.null(meta) ) {
    
    if ( verbose )
      message(sprintf("Creating metadata for wrccID = %s...", wrccID))

    meta <- wrcc_createMetadata(wrccIDs = wrccID)
    
  } else {
    
    meta <- dplyr::filter(meta, wrccID == !!wrccID)
    
  }
  
  # ----- Create 'data' --------------------------------------------------------
  
  # * Download/parse -----
  
  tbl <- wrcc_createRawDataframe(
    wrccID = wrccID,
    startdate = startdate,
    enddate = enddate,
    baseUrl = baseUrl
  )
  
  # * Harmonize ----
  
  # Station can only have one 'monitorType'
  monitorType <- unique(tbl$monitorType)
  
  # Define the set of standard columns that will always be returned
  standardColumns <- c(
    "datetime", "AvAirTemp", "RelHumidty",
    "WindSpeed", "WindDirec", "MxGustSpeed", "DirMxGust",
    "Precip", "SolarRad", "BatteryVoltage", "FuelTemp",
    "AvFuelMoistr"
  )
  
  # If any of the standard columns don't exist, replace them with NA
  for ( column in standardColumns) {
    if ( !column %in% names(tbl) ) {
      tbl[column] <- as.character(NA)
    }
  }
  
  data <-
    tbl %>%
    dplyr::mutate(
      "datetime" = paste0("20",.data$LST_datestamp),
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
