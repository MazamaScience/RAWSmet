library(RAWSmet)
library(dplyr)
library(leaflet)
library(leaflet.minicharts)

cefaMeta <- cefa_createMeta()

waStationIDs <- cefaMeta$nwsID[cefaMeta$stateCode == "WA" & !is.na(cefaMeta$stateCode)]
allDataWA <- lapply(waStationIDs, cefa_createRawsObject, cefaMeta)

august2015AvgList <- list()
for ( station in allDataWA ) {
  august2015Data <- station$data %>% filter(stringr::str_sub(datetime, 6, 7) == "08" & stringr::str_sub(datetime, 0, 4) == "2015")
  avgWS <- mean(august2015Data$windSpeed, na.rm = T)
  avgWD <- mean(august2015Data$windDirection, na.rm = T)
  lat <- station$meta$latitude
  lon <- station$meta$longitude
  theta <- avgWD * pi/180
  dy <- sin(theta) * avgWS/10
  dx <- cos(theta) * avgWS/10
  august2015AvgList[[station$meta$nwsID]] <-
    dplyr::tibble(
      nwsID = station$meta$nwsID,
      stationName = station$meta$locationName,
      lat = lat,
      lon = lon,
      newlat = lat + dy,
      newlon = lon + dx,
      avgWS = avgWS,
      avgWD = avgWD
    )
}

data <- dplyr::bind_rows(august2015AvgList)
data <- data[-21, ]

data$popup = paste("<strong>Station:</strong>", data$stationName, "<br>Average Wind Speed:", round(data$avgWS, 2), "<br>Average Wind Direction:", round(data$avgWD, 2))

leaflet(data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addFlows(lng0 = data$lon, lat0 = data$lat, lng1 = data$newlon, lat1 = data$newlat, maxThickness =  1,
           popup = popupArgs(html = data$popup))
