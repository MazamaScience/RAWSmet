# What is up with the fw13 version?

library(RAWSmet)
library(MazamaSpatialUtils)

setSpatialDataDir("~/Data/Spatial")

meta <- fw13_loadMeta()
fw13_WENU <- fw13_createTimeseriesObject(nwsID = 451702, meta = meta)

dateRange <- MazamaCoreUtils::dateRange(20170901, 20171001, timezone = "UTC")

fw13_data <- 
  fw13_WENU$data %>%
  dplyr::filter(datetime >= dateRange[1] & datetime <= dateRange[2])

plot(fw13_data[,c('datetime','temperature')], ylim = c(0,100), type = 's')

# Required for wrcc_createTimseriesObject??
###loadSpatialData("NaturalEarthAdm1")

wrcc_WENU <- wrcc_createTimeseriesObject(
  "waWENU", 
  startdate = 20200901,
  enddate = 20201001,
)

wrcc_data <- wrcc_WENU$data

# To plot things on the same x-axis we have to lie about the time
lubridate::year(wrcc_data$datetime) <- 2017

points(wrcc_data[,c('datetime','temperature')], col = 'red', type = 's')

# Looks like WRCC data is good and using deg C instead of deg F


