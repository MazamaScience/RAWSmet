#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Identify RAWS station type
#'
#' @param fileString character string containing RAWS data
#' @description Examine the column names of the incoming character vector
#' to identify different types of monitor data provided by RAWS.
#'
#' The return is a list includes everything needed to identify and parse the raw
#' data using \code{readr::read_tsv()}:
#'
#' \itemize{
#' \item{\code{monitorType}}{ -- identification string}
#' \item{\code{rawNames}}{ -- column names from the data (including special characters)}
#' \item{\code{columnNames}}{ -- assigned column names (special characters repaced with '.')}
#' \item{\code{columnTypes}}{ -- column type string for use with \code{readr::read_csv()}}
#' }
#'
#' The \code{monitorType} will be one of:
#' \itemize{
#' \item{"\code{WRCC_TYPE1}"}{ -- ???}
#' \item{"\code{WRCC_TYPE2}"}{ -- ???}
#' \item{"\code{UNKOWN}"}{ -- ???}
#' }
#'
#' @return List including \code{monitorType}, \code{rawNames}, \code{columnNames} and \code{columnTypes}.
#' @references \href{https://raws.dri.edu/}{RAWS USA Climate Archive}
#' @examples
#' \dontrun{
#' fileString <- raws_downloadData(unitID='WENU')
#' monitorTypeList <- raws_identifyMonitorType(fileString)
#' }

wrcc_identifyMonitorType <- function(fileString) {
  
  if ( class(fileString)[1] != "character" && MazamaCoreUtils::logger.isInitialized() ) {
    logger.error('WRCC fileString is of type %s', class(fileString)[1])
    stop(paste0('WRCC fileString is of type %s', class(fileString)[1]))
  }
  
  #     Different header styles     -------------------------------------------
  
  # Type 1
  type1_header <- vector('character',3)
  type1_header[1] <- ":       LST	 mm  	 m/s 	 Deg 	Deg C	Deg C	  %  	volts	  %  	 Deg 	 m/s 	 W/m2"
  type1_header[2] <- ": Date/Time	 Precip	  Wind 	 Wind  	 Av Air	  Fuel 	  Rel  	Battery	Av Fuel	   Dir 	Mx Gust	 Solar "
  type1_header[3] <- ":YYMMDDhhmm	       	  Speed	 Direc 	  Temp 	  Temp 	Humidty	Voltage	 Moistr	 MxGust	 Speed 	  Rad. "
  type1_rawNames <- c('LST_datestamp', 'Percip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', 'AvFuelMoistr', 'DirMxGust', 'MxGustSpeed', 'SolarRad')
  type1_names <- type1_rawNames
  type1_types <- 'cddddddddddd'
  
  # Type 2
  type2_header <- vector('character',3)
  type2_header[1] <- ":       LST	 mm  	 m/s 	 Deg 	Deg C	  %  	volts	 Deg 	 m/s 	 W/m2"
  type2_header[2] <- ": Date/Time	 Precip	  Wind 	 Wind  	 Av Air	  Rel  	Battery	   Dir 	Mx Gust	 Solar "
  type2_header[3] <- ":YYMMDDhhmm	       	  Speed	 Direc 	  Temp 	Humidty	Voltage	 MxGust	 Speed 	  Rad. "
  type2_rawNames <- c('LST_datestamp', 'Percip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'RelHumidty',
                      'BatteryVoltage', 'DirMxGust', 'MxGustSpeed', 'SolarRad')
  type2_names <- type2_rawNames
  type2_types <- 'cddddddddd'
  
  #      Extract  header lines from the incoming fileString     ---------------
  
  # NOTE:  Here are some example headers from WRCC ASCII output:
  # NOTE:
  # NOTE:  [1] "  Enumclaw  Washington "
  # NOTE:  [2] ":       LST	 mm  	 m/s 	 Deg 	Deg C	Deg C	  %  	volts	  %  	 Deg 	 m/s 	 W/m2"
  # NOTE:  [3] ": Date/Time	 Precip	  Wind 	 Wind  	 Av Air	  Fuel 	  Rel  	Battery	Av Fuel	   Dir 	Mx Gust	 Solar "
  # NOTE:  [4] ":YYMMDDhhmm	       	  Speed	 Direc 	  Temp 	  Temp 	Humidty	Voltage	 Moistr	 MxGust	 Speed 	  Rad. "
  # NOTE:
  # NOTE:  [1] " Sullivan  Indiana "
  # NOTE:  [2] ":       LST	 mm  	 m/s 	 Deg 	Deg C	  %  	volts	 Deg 	 m/s 	 W/m2"
  # NOTE:  [3] ": Date/Time	 Precip	  Wind 	 Wind  	 Av Air	  Rel  	Battery	   Dir 	Mx Gust	 Solar "
  # NOTE:  [4] ":YYMMDDhhmm	       	  Speed	 Direc 	  Temp 	Humidty	Voltage	 MxGust	 Speed 	  Rad. "
  
  lines <- readr::read_lines(fileString)
  
  # Strip spaces from the beginning and end but retain "\t" (This is why we can't use stringr::str_trim)
  lines <- stringr::str_replace(lines, '^ *', '')

  # Extract the header
  header <- lines[2:4]
  
  #     Assign the monitor type     -------------------------------------------
  
  # Default to "UNKONWN" type of monitor
  monitorType <- "UNKNOWN"
  rawNames <- vector('character')
  columnNames <- vector('character')
  columnTypes <- vector('character')
  
  # Test the header against known headers to determine the type
  if ( all(header == type1_header) ) {
    monitorType <- "WRCC_TYPE1"
    rawNames <- type1_rawNames
    columnNames <- type1_names
    columnTypes <- type1_types
  } else if ( all(header == type2_header) ) {
    monitorType <- "WRCC_TYPE2"
    rawNames <- type2_rawNames
    columnNames <- type2_names
    columnTypes <- type2_types
  }
  
  monitorTypeList <- list(monitorType=monitorType,
                          rawNames=rawNames,
                          columnNames=columnNames,
                          columnTypes=columnTypes)
  
  return(monitorTypeList)
  
}
