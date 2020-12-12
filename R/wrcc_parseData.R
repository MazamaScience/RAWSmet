#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#' @importFrom dplyr lag
#'
#' @title Parse RAWS data string
#'
#' @param fileString character string containing RAWS data
#' @description Raw character data from WRCC are parsed into a tibble.
#' The incoming \code{fileString} can be read in directly from WRCC using 
#' \code{wrcc_downloadData()} or from a local file using \code{readr::read_file()}.
#'
#' The type of monitor represented by this fileString is inferred from the 
#' column names using \code{wrcc_identifyMonitorType()} and appropriate column 
#' types are assigned. The character data are then processed, read into a tibble 
#' and augmented in the following ways:
#' 
#' \enumerate{
#' \item{Spaces at the beginning and end of each line are removed.}
#' \item{All header lines beginning with ':' are removed.}
#' }
#' 
#' @return Dataframe of RAWS raw station data.
#' @references \href{https://raws.dri.edu/}{RAWS USA Climate Archive}
#' 
#' @examples
#' \dontrun{
#' library(RAWSmet)
#' 
#' fileString <- wrcc_downloadData(wrccID = 'WENU')
#' tbl <- wrcc_parseData(fileString)
#' }

wrcc_parseData <- function(
  fileString = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(fileString)
  
  # ----- Identify format ------------------------------------------------------
  
  if( MazamaCoreUtils::logger.isInitialized() )
    logger.debug(" ----- wrcc_parseData() ----- ")
  
  # Identify monitor type
  monitorTypeList <- wrcc_identifyMonitorType(fileString)
  
  monitorType <- monitorTypeList$monitorType
  
  if ( monitorType == "UNKNOWN" ) {
    stop("Cannot process data of unknown type.")
  }
  
  rawNames <- monitorTypeList$rawNames
  columnNames <- monitorTypeList$columnNames
  columnTypes <- monitorTypeList$columnTypes
  columnUnits <- monitorTypeList$columnUnits
  
  # ----- Parse fileString -----------------------------------------------------
  
  # Convert the fileString into individual lines
  lines <- readr::read_lines(fileString)
  
  if ( length(lines) <= 4 && MazamaCoreUtils::logger.isInitialized() ) {
    logger.warn("No valid PM2.5 data")
    stop(paste0("No valid PM2.5 data"))
  }
  
  # NOTE:  Here is an example header from WRCC ASCII output:
  # NOTE:
  # NOTE:  [1] "  Enumclaw  Washington "
  # NOTE:  [2] ":       LST	 mm  	 m/s 	 Deg 	Deg C	Deg C	  %  	volts	  %  	 Deg 	 m/s 	 W/m2"
  # NOTE:  [3] ": Date/Time	 Precip	  Wind 	 Wind  	 Av Air	  Fuel 	  Rel  	Battery	Av Fuel	   Dir 	Mx Gust	 Solar "
  # NOTE:  [4] ":YYMMDDhhmm	       	  Speed	 Direc 	  Temp 	  Temp 	Humidty	Voltage	 Moistr	 MxGust	 Speed 	  Rad. "
  
  # Strip spaces from the beginning and end but retain "\t" (This is why we can't use stringr::str_trim)
  lines <- stringr::str_replace(lines, '^ *', '')
  lines <- stringr::str_replace(lines, ' *$', '')
  
  # Get monitorName from first line and then remove that line
  monitorName <- lines[1]
  lines <- lines[-1]
  
  # Remove header lines beginning with ":", leaving only data
  goodLines <- !is.na(lines) & !stringr::str_detect(lines, '^:')
  
  # Read the data into a tibble
  fakeFile <- paste0(lines[goodLines], collapse = '\n')
  
  tbl <- 
    readr::read_tsv(fakeFile, col_names = columnNames, col_types = columnTypes) %>%
    # Convert -9999 to NA
    dplyr::na_if(-9999)
  
  tbl$monitorType <- monitorType
  
  # ----- Convert to metric units ----------------------------------------------
  
  # Precip
  unit <- columnUnits[which(columnNames == "precipitation")]
  if ( length(unit) > 0 && unit != "mm" ) {
    stop(sprintf("Need to handle precipitation units of \"%s\" in wrcc_parseData()"))
  }
  
  # Temperature
  unit <- columnUnits[which(columnNames == "temperature")]
  if ( length(unit) > 0 && unit != "degC" ) {
    stop(sprintf("Need to handle temperature units of \"%s\" in wrcc_parseData()"))
  }
  
  # Fuel Temperature
  unit <- columnUnits[which(columnNames == "fuelTemperature")]
  if ( length(unit) > 0 && unit != "degC" ) {
    stop(sprintf("Need to handle fuelTemperature units of \"%s\" in wrcc_parseData()"))
  }
  
  # Wind Speed
  unit <- columnUnits[which(columnNames == "windSpeed")]
  if ( length(unit) > 0 && unit != "m/s" ) {
    stop(sprintf("Need to handle windSpeed units of \"%s\" in wrcc_parseData()"))
  }
  
  # Max Gust Speed
  unit <- columnUnits[which(columnNames == "maxGustSpeed")]
  if ( length(unit) > 0 && unit != "m/s" ) {
    stop(sprintf("Need to handle maxGustSpeed units of \"%s\" in wrcc_parseData()"))
  }
 
  # ----- Convert precipitation from "cumulative since water year start" -------
  
  precipHourly <- c(NA, diff(tbl$precipitation))
  
  # Handle water year reset.
  # NOTE: On the hour after the water year reset, precipHourly will be negative
  #       To get the amount of precipitation on this hour, we must add this
  #       negative value to the previous hourly measurement.
  # 
  #       For example:
  #       precipitation precipHourly temp actualPrecip
  #                   1           NA   NA           NA
  #                   1            0    1            0
  #                   2            1    2            1
  #                   1           -1    1            1
  temp <- precipHourly + dplyr::lag(tbl$precipitation)
  precipHourly[precipHourly < 0 & !is.na(precipHourly)] <- temp[precipHourly < 0 & !is.na(precipHourly)]

  tbl$precipitation <- precipHourly

  # ----- Return ---------------------------------------------------------------
  
  return(tbl)
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  library(RAWSmet)
  
  wrccID <- "OROB"
  startdate <- 20200101
  enddate <- 20200201
  password <- "MY_PASSWORD"
  
  
  fileString <- wrcc_downloadData(
    wrccID = wrccID,
    startdate = startdate,
    enddate = enddate,
    password = password
  )
  
  
}
