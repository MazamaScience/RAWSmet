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

# # See what stations are available
meta %>%
  dplyr::filter(stateCode %in% c("WA")) %>%
  meta_leaflet()

# Use a nwsID to download historical data
Fire_Academy <- cefa_load("451721", meta)

# > dim(Fire_Academy$data)
# [1] 113746     12
#
# > range(Fire_Academy$data$datetime)
# [1] "2004-11-15 19:00:00 UTC" "2018-01-01 07:00:00 UTC"
#
# > pryr::object_size(Fire_Academy)
# 14,566,296 B

raws <- Fire_Academy

# ----- Find day/night daily averages ------------------------------------------

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

# TODO: Option to use localStandardTime_UTC for day definitions

raws$data <-
  raws$data %>%
  # Remove non-numeric and all-missing columns
  dplyr::select(-c("fuelMoisture", "fuelTemperature", "monitorType"))

raws_day$data <-
  raws$data %>%
  # Daytime only
  dplyr::slice(which(timeInfo$day))

raws_night$data <-
  raws$data %>%
  # Nighttime only
  dplyr::slice(which(timeInfo$night))

# ----- Calculate full-day min/max ---------------------------------------------

dailyMin <-
  raws %>%
  sts_summarize(
    unit = "day",
    FUN = min,
    na.rm = TRUE,
    minCount = 3
  ) %>%
  sts_extractData() %>%
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 1)))

dailyMax <-
  raws %>%
  sts_summarize(
    unit = "day",
    FUN = max,
    na.rm = TRUE,
    minCount = 3
  ) %>%
  sts_extractData() %>%
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 1)))

# ----- Calculate daytime mean -------------------------------------------------

# dayMin <-
#   raws_day %>%
#   sts_summarize(
#     unit = "day",
#     FUN = min,
#     na.rm = TRUE,
#     minCount = 3
#   ) %>%
#   sts_extractData() %>%
#   dplyr::mutate(across(where(is.numeric), \(x) round(x, 1)))

dayMean <-
  raws_day %>%
  sts_summarize(
    unit = "day",
    FUN = mean,
    na.rm = TRUE,
    minCount = 3
  ) %>%
  sts_extractData() %>%
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 1)))

# dayMax <-
#   raws_day %>%
#   sts_summarize(
#     unit = "day",
#     FUN = max,
#     na.rm = TRUE,
#     minCount = 3
#   ) %>%
#   sts_extractData() %>%
#   dplyr::mutate(across(where(is.numeric), \(x) round(x, 1)))

dayStd <-
  raws_day %>%
  sts_summarize(
    unit = "day",
    FUN = sd,
    na.rm = TRUE,
    minCount = 3
  ) %>%
  sts_extractData() %>%
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 1)))

# ----- Calculate nighttime mean -----------------------------------------------

# nightMin <-
#   raws_night %>%
#   sts_summarize(
#     unit = "day",
#     FUN = min,
#     na.rm = TRUE,
#     minCount = 3
#   ) %>%
#   sts_extractData() %>%
#   dplyr::mutate(across(where(is.numeric), \(x) round(x, 1)))

nightMean <-
  raws_night %>%
  sts_summarize(
    unit = "day",
    FUN = mean,
    na.rm = TRUE,
    minCount = 3
  ) %>%
  sts_extractData() %>%
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 1)))

# nightMax <-
#   raws_night %>%
#   sts_summarize(
#     unit = "day",
#     FUN = max,
#     na.rm = TRUE,
#     minCount = 3
#   ) %>%
#   sts_extractData() %>%
#   dplyr::mutate(across(where(is.numeric), \(x) round(x, 1)))

# ----- Regular time axis ------------------------------------------------------

# Create the full time axis
datetime <-
  seq(min(dailyMin$datetime), max(dailyMin$datetime), by = "day") %>%
  # NOTE:  round_date() is required unless we use LST
  lubridate::round_date(unit = "day")
dailyDF <- data.frame(datetime = datetime)

# Merge data onto regular axis

dailyMin <-
  dailyDF %>%
  dplyr::left_join(dailyMin, by = "datetime")

dailyMax <-
  dailyDF %>%
  dplyr::left_join(dailyMax, by = "datetime")

# dayMin <-
#   dailyDF %>%
#   dplyr::left_join(dayMin, by = "datetime")

dayMean <-
  dailyDF %>%
  dplyr::left_join(dayMean, by = "datetime")

# dayMax <-
#   dailyDF %>%
#   dplyr::left_join(dayMax, by = "datetime")

dayStd <-
  dailyDF %>%
  dplyr::left_join(dayStd, by = "datetime")

# nightMin <-
#   dailyDF %>%
#   dplyr::left_join(nightMin, by = "datetime")

nightMean <-
  dailyDF %>%
  dplyr::left_join(nightMean, by = "datetime")

# nightMax <-
#   dailyDF %>%
#   dplyr::left_join(nightMax, by = "datetime")

# ----- Calculate UV -----------------------------------------------------------

calculateUV <- function(
  windSpeed,
  windDir
) {
  # NOTE:  Go from Atmospheric 0 = N, clockwise increase to U = E, V = N
  physicsAngle <- pi * (90 - windDir) / 180
  return(list(
    U = round(windSpeed * cos(physicsAngle), 2),
    V = round(windSpeed * sin(physicsAngle), 2)
  ))
}

uv_daytime_ave <- calculateUV(dayMean$windSpeed, dayMean$windDir)
uv_nighttime_ave <- calculateUV(nightMean$windSpeed, nightMean$windDir)

# ----- Create JSON ------------------------------------------------------------

jsonObj <-
  list(
    start = strftime(dailyDF$datetime[1], "%Y-%m-%d"),
    lat = raws$meta$latitude,
    lon = raws$meta$longitude,
    elev = raws$meta$elevation,
    missing = NULL,
    grid = NULL,
    land = NULL,
    data = list(
      # NOTE:  Report temperature in deg Kelvin
      TEMP2_DAYTIME_AVE = dayMean$temperature + 273.15,
      TEMP2_NIGHTTIME_AVE = nightMean$temperature + 273.15,
      TEMP2_MAX = dailyMax$temperature + 273.15,
      TEMP2_MIN = dailyMin$temperature + 273.15,
      #
      RH2_DAYTIME_AVE = dayMean$humidity,
      RH2_NIGHTTIME_AVE = nightMean$humidity,
      RH2_MAX = dailyMax$humidity,
      RH2_MIN = dailyMin$humidity,
      #
      WINDSPEED_DAYTIME_AVE = dayMean$windSpeed,
      WINDSPEED_NIGHTTIME_AVE = nightMean$windSpeed,
      WINDSPEED_MAX = dailyMax$windSpeed,
      WINDSPEED_MIN = dailyMin$windSpeed,
      #
      U_DAYTIME_AVE = uv_daytime_ave[['U']],
      U_NIGHTTIME_AVE = uv_nighttime_ave[['U']],
      #
      V_DAYTIME_AVE = uv_daytime_ave[['V']],
      V_NIGHTTIME_AVE = uv_nighttime_ave[['V']],
      #
      WDIR_DAYTIME_STD = dayStd$windDirection
    )
  )

jsonString <- jsonlite::toJSON(
  jsonObj,
  auto_unbox = TRUE,
  null = 'null',
  na = 'null',
  pretty = TRUE
)

cat(jsonString, file = "cefa_353621.json")

# ===== INTERNAL FUNCTIONS =====================================================

