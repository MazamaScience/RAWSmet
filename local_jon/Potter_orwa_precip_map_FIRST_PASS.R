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
  
  raws_data <-
    waList[[1]] %>%
    raws_extractData(forOpenair = TRUE) %>%
    dplyr::mutate(total_precip = cumsum(precipitation))
  
  title <- 
    sprintf(
      "Droughtiness in %s, %s, August 2020",
      waList[[1]]$meta$siteName,
      waList[[1]]$meta$stateCode
    )
  
  openair::timePlot(
    raws_data,
    pollutant = c("total_precip", "temperature", "humidity"),
    avg.time = "hour",
    main = title, 
    key = TRUE,
    name.pol = c("total precip (mm)", "temperature (°C)", "humidity (%)"), 
    ylab = ""
  )
  
  openair::timePlot(
    raws_data,
    pollutant = c("humidity"),
    windflow = list(col = "grey", lwd = 1, scale = 0.1),
    avg.time = "hour",
    main = title, 
    key = TRUE,
    name.pol = c("humidity (%)"), 
    ylab = ""
  )
  
  
  
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

if ( FALSE ) {
  
  View(orList[[1]]$data) 
  library(ggplot2)
  orList[[1]]$data %>% ggplot() + geom_line(aes(x = datetime, y = temperature))
  orList[[1]]$data %>% ggplot() + geom_line(aes(x = datetime, y = precipitation))
  
  raws_data <-
    orList[[1]] %>%
    raws_extractData(forOpenair = TRUE) %>%
    dplyr::mutate(total_precip = cumsum(precipitation))
  
  title <- 
    sprintf(
      "Droughtiness in %s, %s, August 2020",
      orList[[1]]$meta$siteName,
      orList[[1]]$meta$stateCode
    )
  
  openair::timePlot(
    raws_data,
    pollutant = c("total_precip", "temperature", "humidity"),
    y.relation = "free",
    avg.time = "hour",
    main = title, 
    key = TRUE,
    name.pol = c("total precip (mm)", "temperature (°C)", "humidity (%)"), 
    ylab = ""
  )
  
  openair::timePlot(
    raws_data,
    pollutant = c("humidity"),
    windflow = list(col = "grey", lwd = 1, scale = 0.1),
    avg.time = "hour",
    main = title, 
    key = TRUE,
    name.pol = c("humidity (%)"), 
    ylab = ""
  )
  
  
}


# ===== Example wind shift on Sep 07, 2020 =====================================

if ( FALSE ) {
 
  OREB <- wrcc_loadYear("orOREB", or_meta, 2020, password = "fire2010")

  title <- 
    sprintf(
      "Hot-Dry-Windy in %s, %s, September 2020",
      OREB$meta$siteName,
      OREB$meta$stateCode
    )
  
  raws_data <- 
    OREB %>% 
    raws_filterDate(20200905, 20200910) %>% 
    raws_extractData(forOpenair = TRUE)
  
  openair::timePlot(
    raws_data,
    pollutant = c("temperature", "humidity"),
    y.relation = "free",
    windflow = list(col = "grey", lwd = 1, scale = 0.1),
    avg.time = "hour",
    main = title, 
    key = TRUE,
    name.pol = c("temperature (deg C)", "humidity (%)"), 
    ylab = ""
  )

}

# ===== Download all of ORWA ===================================================

if ( FALSE ) {
  
  orList <- 
    # Load data
    wrcc_loadMultiple(
      wrccIDs = or_meta$wrccID,
      meta = or_meta,
      year = 2020,
      password = MY_PASSWORD
    )


  # TODO:  This dies if any station has no data for the period
  # orList <- 
  #   wrcc_loadMultiple(or_meta$wrccID, or_meta, 2020) %>%
  #   # Filter to the desired time range
  #   rawsList_filterDate(
  #     startdate = 20200801,
  #     enddate = 20200907,
  #     timezone = "America/Los_Angeles"
  #   )
  

    
  or_wrccID_2020 <- names(orList) # 136/205 stations have data in 2020
  
  getTotalPrecip <- function(obj) {
    # NOTE:  Handle warnings like this:
    # NOTE:  Warning message:
    # NOTE:    In max(cumsum(obj$data$precipitation), na.rm = TRUE) :
    # NOTE:    no non-missing arguments to max; returning -Inf
    if ( all(is.na(obj$data$precipitation)) ) {
      totalPrecip <- NA
    } else {
      totalPrecip <- max( cumsum(obj$data$precipitation), na.rm = TRUE )
    }
    totalPrecip <- ifelse(totalPrecip < 0, 0, totalPrecip)
    return(totalPrecip)
  }
  
  totalPrecipList <- lapply(orList, getTotalPrecip)
  
  or_meta_valid <- 
    or_meta %>% 
    dplyr::filter(wrccID %in% or_wrccID_2020)
  
  # NOTE:  totalPrecipList should be in the same order as or_meta_valid but 
  # NOTE:  we will make sure with:
  order <- match(names(totalPrecipList), or_meta_valid$wrccID)
  
  or_meta_valid$totalPrecip <- as.numeric(totalPrecipList)[order]
  
  # Example stolen from 
  #   https://github.com/MazamaScience/AirFireHysplit/blob/master/R/fireDF_map.R
  
  library(ggmap)
  library(ggplot2)
  
  plotDF <- or_meta_valid

  # 6 Categories
  plotDF$precipBin <- 
    .bincode(
      plotDF$totalPrecip,
      breaks = c(0, 50, 100, 200, 500, 1000, Inf),
      include.lowest = TRUE
    )
  
  RdBu <- RColorBrewer::brewer.pal(11, "RdBu")[c(1:3,9:11)]
  cols <- c(
    "1" = RdBu[1],
    "2" = RdBu[2],
    "3" = RdBu[3],
    "4" = RdBu[4],
    "5" = RdBu[5],
    "6" = RdBu[6]
  )
  
  gg <- ggmap::qmplot(
    longitude,
    latitude,
    data = plotDF, 
    geom = "blank", 
    zoom = 7, 
    maptype = "terrain-background"
  ) 
  
  gg <- gg + 
    ggplot2::geom_point(ggplot2::aes(color = as.factor(.data$precipBin)), alpha = 1, size = 5) +
    ggplot2::scale_color_manual(
      values = cols,
      labels = c("<50mm", "50-100", "100-200", "200-500", "500-1000", ">1000mm")
    )
    # ggplot2::scale_colour_gradientn(
    #   name = "Rainfall (mm)",
    #   colors = RdBu,
    #   values = c(0, .02, .05, .1, .2, 1))
  
 print(gg)
 
}
