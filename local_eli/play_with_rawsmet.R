library(RAWSmet)
library(openair)
library(dplyr)

allStationMetaData <- fw13_createMetadata()
waStations <- allStationMetaData %>% dplyr::filter(stateCode == "WA")

head(waStations)
# A tibble: 6 x 8
# nwsID  longitude latitude elevation siteName    countryCode stateCode timezone           
# <chr>      <dbl>    <dbl>     <dbl> <chr>       <chr>       <chr>     <chr>              
# 1 451209     -123.     46.3      2000 ABERNATHY   US          WA        America/Los_Angeles
# 2 452001     -120.     48.7      5161 AENEAS LO   US          WA        America/Los_Angeles
# 3 450131     -123.     48.0      1630 BUCK KNOLL  US          WA        America/Los_Angeles
# 4 450321     -124.     47.4       650 BLACK KNOB  US          WA        America/Los_Angeles
# 5 452132     -120.     48.0      3156 CAMP 4      US          WA        America/Los_Angeles
# 6 451207     -123.     46.3       213 CASTLE ROCK US          WA        America/Los_Angeles

start <- MazamaCoreUtils::parseDatetime(20170801, timezone = "America/Los_Angeles")
end <- MazamaCoreUtils::parseDatetime(20170808, timezone = "America/Los_Angeles")

startRAWS <- MazamaCoreUtils::parseDatetime(20200801, timezone = "America/Los_Angeles")
endRAWS <- MazamaCoreUtils::parseDatetime(20200808, timezone = "America/Los_Angeles")

enumclawFW13 <- fw13_createTimeseriesObject(nwsID = 451702)
enumclawFW13$data <- dplyr::filter(enumclawFW13$data, datetime >= start & datetime < end)

enumclawRAWS <- raws_createTimeseriesObject(stationID = "waWENU", startdate = 20200801, enddate = 20200808)

# Lets compare data from august 2020 (from RAWS) to august 2017 (from FW13)

enumclawFW13$data$date <- enumclawFW13$data$datetime
enumclawRAWS$data$date <- enumclawRAWS$data$datetime

openair::timePlot(enumclawFW13$data, pollutant = "temperature")
openair::timePlot(enumclawRAWS$data, pollutant = "temperature")

openair::windRose(enumclawFW13$data, ws = "windSpeed", wd = "windDirection")
openair::windRose(enumclawRAWS$data, ws = "windSpeed", wd = "windDirection")

