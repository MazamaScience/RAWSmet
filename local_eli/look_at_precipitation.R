setSpatialDataDir("~/Data/Spatial")
tsObj <- fw13_createRawsObject(nwsID = 451702)
start <- MazamaCoreUtils::parseDatetime(20051101, timezone = "UTC")
end <- MazamaCoreUtils::parseDatetime(20051110, timezone = "UTC")
tsObj$data$date <- tsObj$data$datetime
filtered <- dplyr::filter(a$data, date >= start & date < end)
filtered$precipPerHour <- filtered$precipitation / filtered$precipDuration
filtered <- filtered %>% dplyr::select(c("date", "precipitation", "precipDuration", "precipPerHour"))

openair::timePlot(filtered, pollutant = "precipPerHour")
