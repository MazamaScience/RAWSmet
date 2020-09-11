waMeta <- wrcc_loadMeta(stateCode = "WA")
orMeta <- wrcc_loadMeta(stateCode = "OR")

unknownStationsFirst <- c()
unknownStationsSecond <- c()
unknownStationsThird <- c()


for ( i in 1:nrow(orMeta) ) {
  wrccID <- orMeta[i,]$wrccID
  print(sprintf("%s, %d", wrccID, i))
  res = try ({
    fileString <- wrcc_downloadData(wrccID = wrccID)
    monitorData <- wrcc_identifyMonitorType(fileString)
    
    if ( monitorData$monitorType == "UNKNOWN" ) {
      lines <- readr::read_lines(fileString)
      lines <- stringr::str_replace(lines, '^ *', '')
      header <- lines[2:4]
      
      if ( !header[1] %in% unknownStationsFirst ||
           !header[2] %in% unknownStationsSecond ||
           !header[3] %in% unknownStationsThird) {
        unknownStationsFirst <- append(unknownStationsFirst, header[1])
        unknownStationsSecond <- append(unknownStationsSecond, header[2])
        unknownStationsThird <- append(unknownStationsThird, header[3])
      }
    }
  }, silent = TRUE)
}

unknownStations <- data.frame("first" = unknownStationsFirst, "second" = unknownStationsSecond, "third" = unknownStationsThird)

for ( i in 1:nrow(orMeta) ) {
  wrccID <- orMeta[i,]$wrccID
    res <- try ({
    fileString <- wrcc_downloadData(wrccID = wrccID)
    monitorData <- wrcc_identifyMonitorType(fileString)
    
    if ( monitorData$monitorType == "WRCC_TYPE27" ) {
      print(wrccID)
    }
  })
}



fs <- wrcc_downloadData(wrccID = "waWENU")
lines <- readr::read_lines(fs)
lines <- stringr::str_replace(lines, '^ *', '')
header <- lines[2:4]

line1Split <- stringr::str_split(header[1], '\t')[[1]] %>% stringr::str_replace_all(':', '') %>% stringr::str_replace_all(' ', '')
line2Split <- stringr::str_split(header[2], '\t')[[1]] %>% stringr::str_replace_all(':', '') %>% stringr::str_replace_all(' ', '')
line3Split <- stringr::str_split(header[3], '\t')[[1]] %>% stringr::str_replace_all(':', '') %>% stringr::str_replace_all(' ', '')

datetimeVariations <- c("Date/TimeYYMMDDhhmm")
precipVariations <- c("Precip")
windSpeedVariations <- c("WindSpeed")
windDirecVariations <- c("WindDirec")
avAirTempVariations <- c("AvAirTemp")
fuelTempVariations <- c("FuelTemp")
relHumidtyVariations <- c("RelHumidty")
batteryVoltageVariations <- c("BatteryVoltage")
avFuelMoistrVariations <- c("AvFuelMoistr")
dirMxGustVariations <- c("DirMxGust")
mxGustSpeedVariations <- c("MxGustSpeed")
solarRadVariations <- c("SolarRad.")