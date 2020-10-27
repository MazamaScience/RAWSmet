library(MazamaSpatialUtils)
MazamaSpatialUtils::setSpatialDataDir("~/Data/Spatial")

library(MazamaLocationUtils)
MazamaLocationUtils::mazama_initialize()

library(RAWSmet)
RAWSmet::setRawsDataDir("~/Data/RAWS")
rawsDataDir <- RAWSmet::getRawsDataDir()

meta <- fw13_createMeta()

known_locations <- MazamaLocationUtils::table_initializeExisting(meta)

known_locations$deviceDeploymentID <- 
  sprintf(
    "%s_%s",
    known_locations$locationID,
    known_locations$nwsID
  )

save(known_locations, file = paste0(rawsDataDir, '/known_locations.rda'))
