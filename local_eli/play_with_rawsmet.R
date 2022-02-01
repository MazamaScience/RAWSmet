library(RAWSmet)
library(openair)
library(dplyr)

cefaMeta <- cefa_createMeta()

enumclawFW13 <- cefa_createRawsObject(nwsID = 451702, meta)
quilceneFW13 <- cefa_createRawsObject(nwsID = 450207, meta)

augustData <- filter(enumclawFW13$data, stringr::str_sub(datetime, 6, 7) == "08")
novembData <- filter(enumclawFW13$data, stringr::str_sub(datetime, 6, 7) == "11")
allData2005 <- filter(enumclawFW13$data, stringr::str_sub(datetime, 0, 4) == "2005")

enumclawFW13$data$date <- enumclawFW13$data$datetime
quilceneFW13$data$date <- quilceneFW13$data$datetime

augustData$date <- augustData$datetime
novembData$date <- novembData$datetime
augustData$wd = augustData$windDirection
augustData$ws = augustData$windSpeed
allData2005$wd = allData2005$windDirection
allData2005$ws = allData2005$windSpeed

start <- MazamaCoreUtils::parseDatetime(20050801, timezone = "UTC")
end <- MazamaCoreUtils::parseDatetime(20050901, timezone = "UTC")

august2005 <- filter(augustData, datetime >= start & datetime < end)

# Enumclaw and Mill Creek
overallTempTrendEnum <- smoothTrend(enumclawFW13$data, pollutant = "temperature", type = "season")
overallTempTrendQuil <- smoothTrend(quilceneFW13$data, pollutant = "temperature", type = "season")

enumMonthlyTemp <- smoothTrend(enumclawFW13$data, pollutant = "temperature", type = "month")

# 2005
tempPolarFreq2005 <- polarFreq(allData2005, pollutant = "temperature", 
                               type = "month", statistic = "mean")
      
summary2005 <- summaryPlot(select(allData2005, c("date", "temperature", "humidity")), type = "month")

tempHumRadScatter <- scatterPlot(allData2005, x = "temperature", y = "humidity", z = "solarRadiation", type = "season")

all2005TimeVariation <- timeVariation(allData2005, pollutant = "temperature")

tempTrend2005 <- smoothTrend(allData2005, pollutant = "temperature")

# August (2005-2017)
augustWindPlot <- openair::windRose(augustData, type = "year")

solarRadPolarFreq <- polarFreq(augustData, pollutant = "solarRadiation", type = "year", statistic = "mean")

augustTimeVariation <- timeVariation(augustData, pollutant = "temperature")

augustTempTrend <- smoothTrend(augustData, pollutant = "temperature")
# August 2005
humidPercentile <- percentileRose(august2005, pollutant = "humidity", type = "year", 
                                  percentile = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                                  key.position = "right", smooth = T)

tempPercentile <- percentileRose(august2005, pollutant = "temperature", type = "year", 
                                 percentile = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                                 key.position = "right", smooth = T)

tempAugust2005 <- timePlot(august2005, pollutant = "temperature")

# November(2005-2017)
tempCalPlot <- calendarPlot(novembData, pollutant = "precipitation")
