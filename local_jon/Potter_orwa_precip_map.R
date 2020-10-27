# map of RAWS precip from August 1 to Sept. 7, 2020 for WA & OR
#
# Current year implies accessing data from WRCC

library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")

library(RAWSmet)
setRawsDataDir("~/Data/RAWS")

# Load state level metadata
wa_meta <- wrcc_loadMeta("WA")
or_meta <- wrcc_loadMeta("OR")

# Subset to five stations each
wa_sample <- dplyr::slice_sample(wa_meta, n = 5)
or_sample <- dplyr::slice_sample(or_meta, n = 5)

# Get the IDs
wa_IDs <- wa_sample$wrccID
or_IDs <- or_sample$wrccID

# NOTE:  Set the variable MY_PASSWORD to your WRCC password before running this.

# Get/save annual data files for Washington
waList <- 
  # Load data
  wrcc_loadMultiple(
    wrccIDs = wa_IDs,
    meta = wa_sample,
    year = 2020,
    password = MY_PASSWORD
  ) %>%
  # Filter to the desired time range
  rawsList_filterDate(
    startdate = 20200801,
    enddate = 20200907,
    timezone = "America/Los_Angeles"
  )

# Sanity check

if ( FALSE ) {
  
  View(waList[[1]]$data) 
  library(ggplot2)
  waList[[1]]$data %>% ggplot() + geom_line(aes(x = datetime, y = temperature))
  waList[[1]]$data %>% ggplot() + geom_line(aes(x = datetime, y = precipitation))
  
}


# Get/save annual data files for Oregon
orList <- 
  # Load data
  wrcc_loadMultiple(
    wrccIDs = or_IDs,
    meta = or_sample,
    year = 2020,
    password = MY_PASSWORD
  ) %>%
  # Filter to the desired time range
  rawsList_filterDate(
    startdate = 20200801,
    enddate = 20200907,
    timezone = "America/Los_Angeles"
  )

# Total wrecipitation
# TODO:  Find a tidy/purrr/somehow better way of doing this on a List

cumPrecip_1 <- cumsum(waList[[1]]$data$precipitation)
# Quick and dirty with base plot
plot(waList[[1]]$data$datetime, cumPrecip_1, type = 's')

max(cumPrecip_1)



