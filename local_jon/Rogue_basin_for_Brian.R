# From Brian Potter:
#
# When we start building the SW OR website, we'll need to pull specified RAWS, 
# extract specific measures, and plot them either as time series or as maps. We 
# haven't had the discussion yet about what that format will be, but that's 
# where I see this package as being relevant. Otherwise right now, I'd only be 
# using it to poke at the LNU Complex and what it shows for the big growth days.
#
# You may have heard me talk about the marine layer project/onshore flow/westside 
# fire. That's  SW OR. We got funding to purchase and deploy 3 new RAWS in the 
# Rogue drainage. They will complement the existing RAWS so that we can monitor 
# and understand events where onshore flow brings marine air into the drainage. 
# That situation is a big challenge on fires, so knowing when a fire will be 
# above or below the marine layer will be really helpful. Our next step is 
# building an interface for all the RAWS in that drainage, possibly with some UW 
# WRF data to overlay or otherwise combine.


# ----- Spatial Data -----------------------------------------------------------

library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")

# For assigning states
loadSpatialData("NaturalEarthAdm1")

# State boundary
MazamaSpatialUtils::loadSpatialData("USCensusStates")

OR <- subset(USCensusStates, stateCode == "OR")

# USGS Watershed boundaries
MazamaSpatialUtils::loadSpatialData("WBDHU8")
OR_HU8 <- subset(WBDHU8, stateCode == "OR")
Rogue_parts <- subset(OR_HU8, stringr::str_detect(OR_HU8$HUCName, "Rogue|Applegate|Illinois"))

# Combine them
Rogue <- MazamaSpatialUtils::dissolve(Rogue_parts, field = "stateCode", sum_fields = "area")

# Create a map
plot(OR)
plot(Rogue, col = 'blue', add = TRUE)


# ----- FW13 Data --------------------------------------------------------------

library(RAWSmet)
setRawsDataDir("~/Data/RAWS")

fw13_meta <- 
  fw13_loadMeta() %>%
  dplyr::filter(stateCode == "OR")

# Have a look:
raws_leaflet(fw13_meta)

# * 2) Filter by boundaries -----

# Let's filter with lon/lat boundaries
Rogue_fw13_meta <-
  fw13_meta %>%
  dplyr::filter(
    longitude > -125 & longitude < -122 & latitude > 42 & latitude < 43
  )

# Have a look:
raws_leaflet(Rogue_fw13_meta)

# Looks good!

# * 1) Filter by basin names -----

# What RAWS sites existin within the Rogue watershed?
fw13_meta$HUCName <- 
  MazamaSpatialUtils::getHUCName(
    fw13_meta$longitude, 
    fw13_meta$latitude,
    dataset = "WBDHU8"
  )

Rogue_fw13_meta <-
  fw13_meta %>%
  dplyr::filter(stringr::str_detect(HUCName, "Rogue|Applegate|Illinois"))

# Have a look:
raws_leaflet(Rogue_fw13_meta)

# NOTE:  Unfortunately, this misses "FLYNN PRARIE" and "RED MOUND" which are not
# NOTE:  "technically" within the watershed boundaries but are probably sites of
# NOTE:  interest. But it's not terrible to have added HUCName as "spatial 
# NOTE:  metadata".


# ----- TBD --------------------------------------------------------------------

wrcc_meta <- 
  wrcc_loadMeta(stateCode = "OR")

# Have a look:
raws_leaflet(wrcc_meta)

