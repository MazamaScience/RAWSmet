# Example timeseries plots

# Load required MazamaSpatialUtils
library(MazamaSpatialUtils)

# Set directory of previously installed spatial data
setSpatialDataDir("~/Data/Spatial")

# Load RAWSmet
library(RAWSmet)

# Set directory where RAWS data will be stored for reuse
setRawsDataDir("~/Data/RAWS")

# Download/reload metadata from the "FW13" site
meta <- fw13_loadMeta(verbose = TRUE)

# See what stations are available
meta_leaflet(meta)

# Use a nwsID to download historical data
bend_1 <- fw13_load("352621", meta)

# "raws" objects are a list with 'meta' and 'data'

# Review the time range
range(bend_1$data$datetime)

# Use "recipe style" pipeline to extract data for use with the *openair* package
data_2017 <-
  bend_1 %>%
  raws_filterDate(20170901, 20171101, timezone = "America/Los_Angeles") %>%
  raws_getData(forOpenair = TRUE)

# *openair* timeseries plot (time axis in UTC)
openair::timePlot(
  data_2017,
  pollutant = c("temperature", "humidity"),
  avg.time = "hour",
  main = "Temperature and Humidity in Bend, OR, September 2017",
  key = TRUE,
  name.pol = c("temperature (°C)", "humidity (%)"),
  ylab = ""
)

