# Creating json output files for the Smoke Planner

################################################################################
# Notes from phone call with Joel Dubowy on 2022-02-16
#
# https://smoke-planner-test.airfire.org/graphs\
#
# https://airfire-data-exports.s3.us-west-2.amazonaws.com/smoke-planner/uw-4km-wrf/i90_j1_stats.json
#
# https://airfire-data-exports.s3.us-west-2.amazonaws.com/smoke-planner/uw-4km-wrf/i90_j1.json
#
# - remove "_stats" from url to get raw daily files
# - raw daily files have day/night/24hr max/24hr min
#
# raw daily files are needed first
# generate stats files from raw daily files
# stats files are used for generating graphs
#
# day/night -- talk to Andy about cutoff
#
################################################################################

# Load required MazamaSpatialUtils
library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")

library(dplyr)
library(MazamaTimeSeries)

# Load RAWSmet
library(RAWSmet)
setRawsDataDir("~/Data/RAWS")

# Download/reload metadata from the CEFA site ("fw13" data)
meta <- cefa_loadMeta(verbose = TRUE)

# See what stations are available
meta_leaflet(meta)

# Use a nwsID to download historical data
Tumalo_Ridge <- cefa_load("352621", meta)

# > dim(Tumalo_Ridge$data)
# [1] 113746     12
#
# > range(Tumalo_Ridge$data$datetime)
# [1] "2004-11-15 19:00:00 UTC" "2018-01-01 07:00:00 UTC"
#
# > pryr::object_size(Tumalo_Ridge)
# 14,566,296 B

# ----- Find day/night daily averages ------------------------------------------

raws <- Tumalo_Ridge

# > dplyr::glimpse(raws$data, width = 75)
# Rows: 113,746
# Columns: 12
# $ datetime         <dttm> 2004-11-15 19:00:00, 2004-11-15 20:00:00, 2004-…
# $ temperature      <dbl> 0.0000000, 10.5555556, 10.5555556, 11.1111111, 1…
# $ humidity         <dbl> 77.5, 77.5, 77.5, 76.0, 76.0, 73.0, 70.0, 68.0, …
# $ windSpeed        <dbl> 4.47040, 3.57632, 3.12928, 2.68224, 1.78816, 1.7…
# $ windDirection    <dbl> 0, 173, 173, 181, 189, 272, 44, 230, 134, 170, 1…
# $ maxGustSpeed     <dbl> 9.38784, 7.15264, 6.70560, 5.81152, 4.91744, 4.4…
# $ maxGustDirection <dbl> 0, 174, 175, 171, 172, 258, 59, 198, 122, 166, 1…
# $ precipitation    <dbl> NA, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.…
# $ solarRadiation   <dbl> 188, 222, 190, 217, 98, 31, 4, 0, 0, 0, 0, 0, 0,…
# $ fuelMoisture     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
# $ fuelTemperature  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
# $ monitorType      <chr> "FW13", "FW13", "FW13", "FW13", "FW13", "FW13", …

timeInfo <- MazamaTimeSeries::timeInfo(
  raws$data$datetime,
  raws$meta$longitude,
  raws$meta$latitude,
  raws$meta$timezone
)

# > dplyr::glimpse(timeInfo, width = 75)
# Rows: 113,746
# Columns: 10
# $ localStandardTime_UTC <dttm> 2004-11-15 11:00:00, 2004-11-15 12:00:00, …
# $ daylightSavings       <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, F…
# $ localTime             <dttm> 2004-11-15 11:00:00, 2004-11-15 12:00:00, …
# $ sunrise               <dttm> 2004-11-15 07:01:25, 2004-11-15 07:01:25, …
# $ sunset                <dttm> 2004-11-15 16:38:42, 2004-11-15 16:38:42, …
# $ solarnoon             <dttm> 2004-11-15 11:50:18, 2004-11-15 11:50:18, …
# $ day                   <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, …
# $ morning               <lgl> TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
# $ afternoon             <lgl> FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE,…
# $ night                 <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, T…

raws_day <- raws_night <- raws

raws_day$data <-
  raws$data %>%
  dplyr::select(-c("fuelMoisture", "fuelTemperature", "monitorType")) %>%
  dplyr::slice(which(timeInfo$day))

raws_night$data <-
  raws$data %>%
  dplyr::select(-c("fuelMoisture", "fuelTemperature", "monitorType")) %>%
  dplyr::slice(which(timeInfo$night))


dailyMeanBrick <-
  raws_day %>%
  sts_summarize(
    unit = "day",
    FUN = mean,
    na.rm = TRUE,
    minCount = 3
  ) %>%
  sts_extractData() %>%
  dplyr::mutate(across(-1, round, 1))

# TODO:  merge with uniform daily time axis

# TODO:  output json

# TODO:  same for night






# ===== INTERNAL FUNCTIONS =====================================================

#' @param sts \emph{sts} object.
#' @param timezone Olson timezone used to interpret dates.
#' @param unit Unit used to summarize by (\emph{e.g.} "day").
#' @param FUN Function used to summarize time series.
#' @param ... Additional arguments to be passed to \code{FUN}

sts_summarize <- function(
  sts,
  timezone = NULL,
  unit = c("day", "week", "month", "year"),
  FUN = NULL,
  ...,
  minCount = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(sts)
  unit <- match.arg(unit)
  MazamaCoreUtils::stopIfNull(FUN)
  MazamaCoreUtils::stopIfNull(minCount)

  if ( sts_isEmpty(sts) )
    stop("'sts' has no data")

  if ( length(unique(sts$meta$timezone)) > 1 )
    stop("'sts' has muliple timezones")

  # # Use internal function to determine the timezone to use
  # timezone <- .determineTimezone(sts, NULL, timezone, verbose = TRUE)
  timezone <- sts$meta$timezone[1]

  # ----- Summarize by time period ---------------------------------------------

  # See:  https://www.statology.org/aggregate-daily-data-in-r/
  # See:  https://www3.nd.edu/~steve/computing_with_data/24_dplyr/dplyr.html
  # See:  https://dplyr.tidyverse.org/articles/colwise.html

  customFUN <- function(x, ...) {
    if ( sum(!is.na(x)) >= minCount ) {
      return(FUN(x, ...))
    } else {
      return(NA)
    }
  }

  newData <-

    sts$data %>%

    # Create 'timeUnit' in the desired timezone as a grouping variable
    dplyr::mutate(
      timeUnit = lubridate::floor_date(lubridate::with_tz(.data$datetime, tz = timezone), unit)
    ) %>%
    dplyr::select(-.data$datetime) %>%
    dplyr::group_by(.data$timeUnit) %>%

    # Summarize using FUN (will ignore 'timeUnit' column)
    dplyr::summarize(across(everything(), customFUN, ...)) %>%

    # Replace +/-Inf and NaN with NA
    dplyr::mutate(across(everything(), function(x) { x[!is.finite(x)] <- NA; return(x) })) %>%

    # New, daily 'datetime'
    dplyr::rename(datetime = .data$timeUnit)


  # ----- Create the 'sts' object ----------------------------------------------

  sts$data <- newData

  # ----- Return ---------------------------------------------------------------

  return(invisible(sts))

}


