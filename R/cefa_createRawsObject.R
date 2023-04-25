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
#' @param verbose Logical flag controlling detailed progress statements.
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
#'
#' The columns of the 'data' dataframe include:
#'
#' \itemize{
#'  \item{datetime: UTC datetime of observation}
#'  \item{temperature: temperature (°C)}
#'  \item{humidity: average relative humidity (\%)}
#'  \item{windSpeed: average wind speed (m/s)}
#'  \item{windDirection: average wind direction (°)}
#'  \item{maxGustSpeed: max gust speed (m/s)}
#'  \item{maxGustDirection: max gust direction (°)}
#'  \item{precipitation: precipitation (mm/hr)}
#'  \item{solarRadiation: solar radiation (W/m^2)}
#'  \item{fuelMoisture: 10-hour time lag fuel moisture}
#'  \item{fuelTemperature: NA for FW13 data}
#' }
#'
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(RAWSmet)
#' setRawsDataDir("~/Data/RAWS")
#'
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("NaturalEarthAdm1.rda")
#'
#' nws_500726 <- cefa_createRawsObject(nwsID = 500726)
#'
#' }, silent = FALSE)
#' }
#'
#' @seealso \code{\link{cefa_createMeta}}
#' @seealso \code{\link{cefa_parseData}}
#'
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}
#' @references \href{https://fam.nwcg.gov/fam-web/weatherfirecd/13.htm}{FW13 Data Format}

cefa_createRawsObject <- function(
  nwsID = NULL,
  meta = NULL,
  baseUrl = "https://cefa.dri.edu/raws/fw13/",
  verbose = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(nwsID)

  # Guarantee it is zero padded six characters
  nwsID <- sprintf("%06s", as.character(nwsID))

  # ----- Create 'meta' --------------------------------------------------------

  if ( is.null(meta) ) {
    meta <- cefa_loadMeta(verbose = verbose)
  }

  # Subset to a single record
  meta <- dplyr::filter(meta, nwsID == !!nwsID)

  # ----- Download/parse data --------------------------------------------------

  tbl <-
    cefa_downloadData(
      nwsID = nwsID,
      baseUrl = baseUrl
    ) %>%
    cefa_parseData()

  # > dplyr::glimpse(tbl, width = 75)
  # Rows: 72,832
  # Columns: 27
  # $ recordType          <chr> "W13", "W13", "W13", "W13", "W13", "W13", "W1
  # $ nwsID               <chr> "021503", "021503", "021503", "021503", "0215
  # $ observationDate     <chr> "20100730", "20100730", "20100730", "20100730
  # $ observationTime     <chr> "1100", "1200", "1300", "1400", "1500", "1600
  # $ observationType     <chr> "R", "R", "R", "R", "R", "R", "R", "R", "R",
  # $ weatherCode         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N
  # $ dryBulbTemp         <dbl> 99, 99, 102, 103, 104, 104, 104, 102, 96, 92,
  # $ atmosMoisture       <dbl> 37, 32, 22, 29, 28, 27, 28, 30, 37, 42, 37, 4
  # $ windDirection       <dbl> 192, 223, 230, 202, 215, 220, 204, 195, 187,
  # $ avWindSpeed         <dbl> 6, 7, 8, 7, 7, 6, 5, 4, 4, 4, 4, 3, 6, 6, 2,
  # $ fuelMoisture        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N
  # $ maxTemp             <dbl> 99, 99, 102, 103, 104, 104, 104, 104, 104, 10
  # $ minTemp             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  # $ maxRelHumidity      <dbl> 37, 37, 37, 37, 37, 37, 37, 37, 37, 42, 42, 4
  # $ minRelHumidity      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  # $ precipDuration      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  # $ precipAmount        <dbl> NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  # $ wetFlag             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N
  # $ herbaceousGreenness <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N
  # $ shrubGreenness      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N
  # $ moistureType        <chr> "2", "2", "2", "2", "2", "2", "2", "2", "2",
  # $ measurementType     <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1",
  # $ seasonCode          <chr> "3", "3", "3", "3", "3", "3", "3", "3", "3",
  # $ solarRadiation      <dbl> 698, 909, 967, 917, 812, 660, 480, 297, 92, 6
  # $ maxGustDirection    <dbl> 206, 214, 220, 246, 213, 213, 233, 241, 202,
  # $ maxGustSpeed        <dbl> 11, 11, 13, 12, 14, 13, 10, 8, 8, 7, 9, 6, 12
  # $ snowFlag            <chr> "N", "N", "N", "N", "N", "N", "N", "N", "N",

  # ----- Convert to metric ----------------------------------------------------

  # Details from: https://fam.nwcg.gov/fam-web/weatherfirecd/13.htm

  # measurementType: 1 = US, 2 = metric
  #
  # Measurement Type code: 1=U.S.,2=Metric. Affects temperature (Fahrenheit or
  # Celsius), wind (miles or kilometers per hour), and precipitation (decimal
  # inches or millimeters - 1 = US (Precipitation amount – 24 hour), 2 = Metric
  # (Precipitation amount – 24 hour), 3 = US (Precipitation amount – hourly),
  # 4 = Metric (Precipitation amount – hourly).

  # Convert temperature from deg F to deg C
  tempConvert <- function(type, temp) {
    if (type == 1)
      return(5/9 * (temp - 32))
    else
      return(temp)
  }

  temperature <-
    mapply(tempConvert, tbl$measurementType, tbl$dryBulbTemp) %>%
    round(1)

  # Convert wind speeds from miles-per-hour to meters-per-second
  #
  # Average windspeed over a 10-minute period (miles or kilometers per hour
  # based on Measurement Type code).
  #
  # Speed of peak gust during the hour. (miles or kilometers per hour based on
  # Measurement Type code).

  speedConvert <- function(type, speed) {
    if (type == 1)
      return(1609.344 * speed * 1/3600)
    else
      return(speed)
  }

  windSpeed <-
    mapply(speedConvert, tbl$measurementType, tbl$avWindSpeed) %>%
    round(1)

  mxGustSpeed <-
    mapply(speedConvert, tbl$measurementType, tbl$maxGustSpeed) %>%
    round(1)

  # Convert precipitation to millimeters per hour
  #
  # Precipitation amount based on Measurement Type code [col. 63]. Blanks=no
  # precipitation. U.S. measurement: inches with implied decimal nn.nnn format;
  # trace shown as 00005. Metric measurement: measured in millimeters, no
  # implied decimal; trace shown as 00001.

  precipConvert <- function(type, amount) {
    if (type == 1)
      return(25.4 * amount/1000)
    else
      return(amount)
  }

  precipHourly <-
    c(NA, diff(tbl$precipAmount)) %>%
    round(1)

  # Handle daily precip reset.
  # NOTE: On the first hour of each LST day, precipHourly will be negative
  #       To get the amount of precipitation in the first hour of each day,
  #       we must add this negative value to the previous hourly measurement.
  #
  #       For example:
  #       precipitation precipHourly temp actualPrecip
  #                   1           NA   NA           NA
  #                   1            0    1            0
  #                   2            1    2            1
  #                   1           -1    1            1
  bop <- precipHourly + dplyr::lag(tbl$precipAmount)
  precipHourly[precipHourly < 0 & !is.na(precipHourly)] <- bop[precipHourly < 0 & !is.na(precipHourly)]

  precipitation <-
    mapply(precipConvert, tbl$measurementType, precipHourly) %>%
    round(1)

  precipitation[is.nan(precipitation)] <- 0

  # ----- Harmonize names ------------------------------------------------------

  # Define the set of standard columns that will always be returned
  standardDataVars <- c(
    "datetime", "temperature", "humidity",
    "windSpeed", "windDirection", "maxGustSpeed", "maxGustDirection",
    "precipitation", "solarRadiation",
    "fuelMoisture", "fuelTemperature",
    "monitorType"
  )

  # TODO:  Use metric versions of data
  data <-
    tbl %>%
    dplyr::mutate(
      "datetime" = paste0(.data$observationDate, .data$observationTime),
      "temperature" = temperature,
      "humidity" = (.data$minRelHumidity + .data$maxRelHumidity)/2,
      "windSpeed" = windSpeed,
      "windDirection" = .data$windDirection,
      "maxGustSpeed" = mxGustSpeed,
      "maxGustDirection" = .data$maxGustDirection,
      "precipitation" = precipitation,
      "solarRadiation" = .data$solarRadiation,
      "fuelMoisture" = .data$fuelMoisture,
      "fuelTemperature" = as.numeric(NA),
      "monitorType" = "FW13"
    ) %>%
    dplyr::select(all_of(standardDataVars))

  # ----- Convert datetime to UTC ----------------------------------------------

  UTC_STD_offset <-
    MazamaSpatialUtils::SimpleTimezones %>%
    dplyr::filter(.data$timezone == meta$timezone) %>%
    dplyr::pull("UTC_STD_offset")

  # NOTE:  The 'datetime' column is "local standard time all-year-round" for
  # NOTE:  which no timezone exists. So we have to convert it first to UTC
  # NOTE:  and then shift it by the UTC_STD_offset.
  # NOTE:  When we subtract a UTC_STD_offset of, e.g. -8 (PST), we will get the
  # NOTE:  correct UTC time that is 8 hours later than the US West Coast clock time.

  UTC_time <-
    MazamaCoreUtils::parseDatetime(data$datetime, timezone = "UTC") -
    lubridate::dhours(UTC_STD_offset)

  data$datetime <- UTC_time

  # ----- Return ---------------------------------------------------------------

  # Combine meta and data dataframes into a list
  raws <- list(meta = meta, data = data)
  class(raws) <- c("raws_timeseries", "sts", class(raws))

  return(raws)

}


# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  nwsID = 500726
  meta = NULL
  baseUrl = "https://cefa.dri.edu/raws/cefa/"
  verbose = FALSE

}
