# Example timeseries plots

library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")

library(RAWSmet)
setRawsDataDir("~/Data/RAWS")

meta <- fw13_loadMeta(verbose = TRUE)

raws_leaflet(meta)

bend_1 <- fw13_load("352621", meta)

range(bend_1$data$datetime)

data_2017 <- 
  bend_1 %>% 
  raws_filterDate(20170901, 20171101, timezone = "America/Los_Angeles") %>%
  raws_extractData(forOpenair = TRUE)

AirSensor::timeseriesTbl_multiPlot(data_2017)

openair::timePlot(
  data_2017,
  pollutant = c("temperature", "humidity"),
  avg.time = "hour",
  main = "Temperature and Humidity in Bend, OR, September 2017", 
  key = TRUE,
  name.pol = c("temperature (°C)", "humidity (%)"), 
  ylab = ""
)

