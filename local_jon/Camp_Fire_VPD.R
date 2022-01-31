#
# From Andy Chiodi:
#
# Did you watch Cliff's presentation?  The handful of RAWS slides he showed in
# relation to 2018 Camp Fire were pretty close to what I was going to suggest:
# basically, given a location and date/time (e.g. fire ignition):
#
# 1. What are the closest RAWS
# 2. What do key variables (wind, temp, rh, VPD, fuel-moisture if available)
#    look like leading up to that datetime and during the fire?
# 3. What can we say about how anomalous those conditions are?

# From Brian Potter:
#
# Here's what I use for VPD: VPD=(1-RH/100)*6.11 exp(17.27*T/(T+237.15)) where
# RH is 1-100 and T is in Celsius.
#
# The equation comes from the Clausius-Clapeyron equation, basic thermodynamics,
# and knowledge of the saturation vapor pressure at 0C.
#
# See: https://en.wikipedia.org/wiki/Clausius–Clapeyron_relation#Meteorology_and_climatology
#
# Excellent scientific paper on the importance of VPD:
#   http://ocp.ldeo.columbia.edu/res/div/ocp/WestCLIM/PDFS/Seager_etal_VPD.pdf

library(RAWSmet)
setRawsDataDir("~/Data/RAWS")

# ----- Configurable parameters ------------------------------------------------

# Camp Fire: 39°48′37″N 121°26′14″W

xlim <- c(-123, -120)
ylim <- c(38.5, 41.5)

# ----- FW13 Meta --------------------------------------------------------------

fw13_meta <-
  fw13_loadMeta() %>%
  dplyr::filter(longitude >= xlim[1] & longitude < xlim[2] &
                  latitude >= ylim[1] & latitude < ylim[2])

# Have a look:
meta_leaflet(fw13_meta)

# Looks good!

# ----- Load FW13 data ---------------------------------------------------------

fw13_list <- fw13_loadMultiple(
  nwsIDs = fw13_meta$nwsID,
  meta = fw13_meta,
  newDownload = FALSE
)

fw13_CampFireList <-
  fw13_list %>%
  rawsList_filterDate("2018-10-01", "2018-11-15", timezone = "America/Los_Angeles") %>%
  rawsList_removeEmpty()

# > length(fw13_CampFireList)
# [1] 20

fw13_CampFireDF <-
  fw13_CampFireList %>%
  rawsList_toRawsDF()

# ----- WRCC Meta --------------------------------------------------------------

wrcc_meta <-
  wrcc_loadMeta(stateCode = "CA") %>%
  dplyr::filter(longitude >= xlim[1] & longitude < xlim[2] &
                  latitude >= ylim[1] & latitude < ylim[2])

# Have a look:
meta_leaflet(wrcc_meta)

# Looks good!


# ----- Load WRCC data ---------------------------------------------------------

wrcc_list <- wrcc_loadMultiple(
  wrccIDs = wrcc_meta$wrccID,
  meta = wrcc_meta,
  year = 2018,
  newDownload = FALSE
)

wrcc_CampFireList <-
  wrcc_list %>%
  rawsList_filterDate("2018-10-01", "2018-11-15", timezone = "America/Los_Angeles") %>%
  rawsList_removeEmpty()

# > length(fw13_CampFireList)
# [1] 20

wrcc_CampFireDF <-
  wrcc_CampFireList %>%
  rawsList_toRawsDF()

# ----- TODO -------------------------------------------------------------------

# Create timeseries plots showing the evolution of Andy's preferred parameters

library(ggplot2)

ggplot(wrcc_CampFireDF) +
  geom_line(aes(x = datetime, y = VPD, color = wrccID))

s <- MazamaCoreUtils::parseDatetime(20181101, timezone = "America/Los_Angeles")
e <- MazamaCoreUtils::parseDatetime(20181108, timezone = "America/Los_Angeles")

# Local time
###lubridate::tz(wrcc_CampFireDF$datetime) <- "America/Los_Angeles"

# To get the local time axis see:
#   https://github.com/MazamaScience/AirMonitorPlots/blob/master/R/custom_datetimeScale.R

# HACK:  In the mean time, just subtract 8 hours from UTC and call it LST
wrcc_CampFireDF$datetime <- wrcc_CampFireDF$datetime + lubridate::dhours(8)

wrcc_CampFireDF %>%
  dplyr::filter(datetime >= s & datetime < e) %>%
  ggplot() +
  geom_point(
    aes(x = datetime, y = VPD),
    color = 'black',
    alpha = 0.1,
    shape = 'square'
  ) +
  ###scale_x_datetime(timezone = "UTC") +
  theme_bw() +
  xlab("LST") +
  ggtitle("Vapor Pressure Deficit")

