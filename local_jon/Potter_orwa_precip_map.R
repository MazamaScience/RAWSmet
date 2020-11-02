# map of RAWS precip from August 1 to Sept. 7, 2020 for WA & OR
#
# NOTE:  Current year implies accessing data from WRCC

library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")

library(RAWSmet)
setRawsDataDir("~/Data/RAWS")

# Load state level metadata
wa_meta <- wrcc_loadMeta("WA")
or_meta <- wrcc_loadMeta("OR")

# Load all stations in Oregon
orList <- 
  # Load data
  wrcc_loadMultiple(
    wrccIDs = or_meta$wrccID,
    meta = or_meta,
    year = 2020,
    newDownload = TRUE,
    password = MY_PASSWORD
  ) %>%
  # Filter to the desired time range
  rawsList_filterDate(
    startdate = 20200801,
    enddate = 20200907,
    timezone = "America/Los_Angeles"
  ) %>%
  # Remove any stations with no data
  rawsList_removeEmpty()

# Create a tidy dataframe
orDF <-
  orList %>%
  lapply(raws_toRawsDF) %>%
  dplyr::bind_rows()

# How big is it?
# > pryr::object_size(orDF)
# 17.2 MB

# Find the cumulative precip at each site
plotDF <-
  orDF %>%
  dplyr::group_by(wrccID) %>%
  dplyr::mutate(
    total_precip = cumsum(precipitation)
  ) %>%
  dplyr::slice_max(datetime, with_ties = FALSE) %>%
  dplyr::ungroup()
  
# ----- Map it -----------------------------------------------------------------

library(ggmap)
library(ggplot2)

# 5 bins (one per ',')
breaks = c(-Inf, 5, 10, 20, 50, Inf)

# 5 colors
RColorBrewer::brewer.pal(5, "BrBG")

# 5 color positions [0-1]
values <- c(.005, .01, .02, .05, 1)


gg <- 
  ggmap::qmplot(
    longitude,
    latitude,
    data = plotDF, 
    geom = "blank", 
    zoom = 7, 
    maptype = "terrain-background",
    ###maptype = "toner-lite"
  ) +
  geom_point(aes(color = total_precip), alpha = 1, size = 8) +
  scale_color_stepsn(
    name = "Precip (mm)",
    breaks = breaks,
    colors = colors,
    values = values,
  ) +
  ggtitle(
    label = "Total Precip Aug 01-Sep 07, 2020"
  )
  
print(gg)

