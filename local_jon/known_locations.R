# Example timeseries plots

library(MazamaSpatialUtils)
MazamaSpatialUtils::setSpatialDataDir("~/Data/Spatial")

library(MazamaLocationUtils)
MazamaLocationUtils::mazama_initialize()

library(RAWSmet)

meta <- fw13_createMetadata()

known_locations <- MazamaLocationUtils::table_initializeExisting(meta)

dplyr::glimpse(known_locations)

deviceDeploymentID <- 
  sprintf(
    "%s_%s",
    known_locations$locationID,
    known_locations$nwsID
  )
