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

for ( i in 1:nrow(unknownStationsOR) ) {
  wrccID <- unknownStationsOR[i,]$wrccID
  print(wrccID)
  fileString <- wrcc_downloadData(wrccID = wrccID)
  monitorData <- wrcc_identifyMonitorType(fileString)
  
  if ( monitorData$monitorType == "UNKNOWN" ) {
    unknownStationsOR <- append(unknownStationsOR, wrccID)
  }
}
