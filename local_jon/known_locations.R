# Example timeseries plots

library(MazamaSpatialUtils)
MazamaSpatialUtils::setSpatialDataDir("~/Data/Spatial")

library(MazamaLocationUtils)
MazamaLocationUtils::mazama_initialize()

library(RAWSmet)

meta <- cefa_loadMeta()

known_locations <-
  meta %>%
  MazamaLocationUtils::table_initializeExisting(distanceThreshold = 1000)

dplyr::glimpse(known_locations)

deviceDeploymentID <-
  sprintf(
    "%s_%s",
    known_locations$locationID,
    known_locations$nwsID
  )
