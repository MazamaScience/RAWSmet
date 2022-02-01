# What is up with the cefa version?

library(RAWSmet)
library(MazamaSpatialUtils)

setSpatialDataDir("~/Data/Spatial")

meta <- cefa_loadMeta()
cefa_WENU <- cefa_createRawsObject(nwsID = 451702, meta = meta)

dateRange <- MazamaCoreUtils::dateRange(20170901, 20171001, timezone = "UTC")

cefa_data <- 
  cefa_WENU$data %>%
  dplyr::filter(datetime >= dateRange[1] & datetime <= dateRange[2])

plot(cefa_data[,c('datetime','temperature')], ylim = c(0,100), type = 's')

# Required for wrcc_createTimseriesObject??
###loadSpatialData("NaturalEarthAdm1")

wrcc_WENU <- wrcc_createRawsObject(
  "waWENU", 
  startdate = 20200901,
  enddate = 20201001,
)

wrcc_data <- wrcc_WENU$data

# To plot things on the same x-axis we have to lie about the time
lubridate::year(wrcc_data$datetime) <- 2017

points(wrcc_data[,c('datetime','temperature')], col = 'red', type = 's')

# Looks like WRCC data is good and using deg C instead of deg F


