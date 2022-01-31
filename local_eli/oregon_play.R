fw13Meta <- fw13_loadMeta()

# Get all stations with valid data on the west side of the cascades
meta <- fw13Meta %>%
  dplyr::filter(stateCode == "OR") %>%
  dplyr::filter(longitude < -122.33)

allData_westernOregon <- data.frame()

for ( i in 1:nrow(meta) ) {
  res <- try({
    stationTSObject <- fw13_load(nwsID = meta[1,]$nwsID, meta = fw13Meta)
    
    # Get data for each September
    stationData <- stationTSObject %>%
      raws_getData(forOpenair = TRUE) %>%
      dplyr::filter(lubridate::month(datetime) == 9)
    
    allData_westernOregon <- rbind(allData_westernOregon, stationData)
  }, silent = FALSE)
}


orOGOO <- 
  wrcc_load(wrccID = "orOGOO", year = 2020) %>%
  raws_filterDate(
    startdate = MazamaCoreUtils::parseDatetime(20200901, timezone = "America/Los_Angeles"),
    enddate = MazamaCoreUtils::parseDatetime(20200924, timezone = "America/Los_Angeles")
  )

orOGOOData <- orOGOO %>% raws_getData(forOpenair = TRUE)

fw13_data <-
  fw13_load(nwsID = "352545") %>%
  raws_getData(forOpenair = TRUE) %>%
  dplyr::filter(lubridate::month(datetime) == 9)