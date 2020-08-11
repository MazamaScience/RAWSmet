#' @export
#' @importFrom rlang .data
#' @importFrom dplyr all_of
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Obtain RAWS data and create a timeseries object.
#'
#' @param stationID Station identifier found in 'meta'.
#' @param meta Tibble of RAWS metadata containing \code{stationID}.
#' @param startdate Desired start date (integer or character representing YYYYMMDD[HH]).
#' @param enddate Desired end date (integer or character representing YYYYMMDD[HH]).
#' @param baseUrl Base URL for data queries.
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
#' waWALD <- wrcc_createTimeseriesObject(stationID = 'waWALD')
#' 
#' }
#'
#' @seealso \code{\link{wrcc_downloadData}}
#' @seealso \code{\link{wrcc_parseData}}
#'
#' @references \href{https://raws.dri.edu/}{RAWS USA Climate Archive}

wrcc_createTimeseriesObject <- function(
  stationID = NULL,
  meta = NULL,
  startdate = strftime(lubridate::now(tzone = "UTC"), "%Y%m0101", tz = "UTC"),
  enddate = strftime(lubridate::now(tzone = "UTC"), "%Y%m%d23", tz = "UTC"),
  baseUrl = "https://wrcc.dri.edu/cgi-bin/wea_list2.pl"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  stopIfNull(stationID)

  if ( length(stationID) > 1 )
    stop("Parameter 'stationID' must be of length 1")
  
  # ----- Create 'meta' --------------------------------------------------------
  
  if ( is.null(meta) ) {
    
    meta <- wrcc_createMetadata(stationIDs = stationID)
    
  } else {
    
    meta <- dplyr::filter(meta, stationID == !!stationID)
    
  }
  
  # ----- Create 'data' --------------------------------------------------------
  
  # * Download/parse -----
  
  tbl <- wrcc_createRawDataframe(
    stationID = stationID,
    startdate = startdate,
    enddate = enddate,
    baseUrl = baseUrl
  )
  
  # * Harmonize ----
  
  # Station can only have one 'monitorType'
  monitorType <- unique(tbl$monitorType)
  
  # Define the set of standard columns that will always be returned
  standardColumns <- c(
    "datetime", "temperature", "humidity",
    "windSpeed", "windDirection", "maxGustSpeed", "maxGustDirection",
    "precipitation", "solarRadiation"
  )
  
  if ( monitorType == "WRCC_TYPE1") {
    
    # # A tibble: 6 x 13
    #   LST_datestamp Precip WindSpeed WindDirec AvAirTemp FuelTemp RelHumidty BatteryVoltage AvFuelMoistr DirMxGust MxGustSpeed SolarRad monitorType
    #   <chr>         <chr>  <chr>     <chr>     <chr>     <chr>    <chr>      <chr>          <chr>        <chr>     <chr>       <chr>    <chr>      
    # 1 2008010000    1100   0.447     94        13.89     33.89    NA         13             NA           94        1.341       0        WRCC_TYPE1 
    # 2 2008010100    1100   0         93        12.78     37.22    NA         13             NA           94        1.341       0        WRCC_TYPE1 
    
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
    
  } else if ( monitorType == "WRCC_TYPE2") {
    
    # # A tibble: 6 x 11
    #   LST_datestamp Precip WindSpeed WindDirec AvAirTemp RelHumidty BatteryVoltage DirMxGust MxGustSpeed SolarRad monitorType
    #   <chr>         <chr>  <chr>     <chr>     <chr>     <chr>      <chr>          <chr>     <chr>       <chr>    <chr>      
    # 1 2008010000    171.5  5.812     245       22.22     35         12.9           241       7.6         0        WRCC_TYPE2 
    # 2 2008010100    171.5  5.812     250       21.67     36         12.9           234       8.494       0        WRCC_TYPE2 

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
        "monitorType" = .data$monitorType
      ) %>%
      dplyr::select(all_of(standardColumns))
    
  }
  
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
