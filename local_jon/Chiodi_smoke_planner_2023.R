# Creating json output files for the Smoke Planner

################################################################################
# Notes from phone call with Andy Chiodi on 2023-04-24
#
# We just want to get raw data as .csv files. Andy or Joel will then process
# those files.
#
################################################################################

# Load required MazamaSpatialUtils
library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")

library(dplyr)
library(MazamaTimeSeries)

# Load RAWSmet
library(RAWSmet)
setRawsDataDir("~/Data/RAWS")

# ----- Get metadata -----------------------------------------------------------

# Download/reload metadata from the CEFA site ("fw13" data)
meta <- cefa_loadMeta(verbose = TRUE)

# Save CEFA metadata
if ( !file.exists("~/Data/RAWS/cefa_meta.csv") ) {
  readr::write_csv(meta, "~/Data/RAWS/cefa_meta.csv")
}

# ----- Get data ---------------------------------------------------------------

wa_meta <-
  meta %>%
  dplyr::filter(stateCode %in% c("WA"))

# See what stations are available
meta_leaflet(wa_meta)

i <- 0
for ( id in wa_meta$deviceID ) {

  i <- i + 1

  message(sprintf("Working on %s (%d/%d) ...", id, i, nrow(wa_meta)))

  filePath <- file.path("~/Data/RAWS", sprintf("cefa_%s_data.csv", id))

  result <- try({

    raws <- cefa_load(id, wa_meta)

    readr::write_csv(raws$data, file = filePath)

  }, silent = verbose)

  if ( "try-error" %in% class(result) ) {

    err_msg <- geterrmessage()
    message(sprintf("Skipping %s: %s", id, err_msg))

  }


}

