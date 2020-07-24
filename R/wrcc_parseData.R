#' @keywords WRCC
#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Parse WRCC data string
#'
#' @param fileString character string containing WRCC RAWS data
#' @description Raw character data from WRCC are parsed into a tibble.
#' The incoming \code{fileString}
#' can be read in directly from WRCC using \code{wrcc_downloadData()} or from a local
#' file using \code{readr::read_file()}.
#'
#' The type of monitor represented by this fileString is inferred from the column names
#' using \code{wrcc_identifyMonitorType()} and appropriate column types are assigned.
#' The character data are then processed, read into a tibble and augmented in the following ways:
#' \enumerate{
#' \item{Spaces at the beginning and end of each line are moved.}
#' \item{All header lines beginning with ':' are removed.}
#' }
#' @return Dataframe of WRCC raw monitor data.
#' @references \href{https://raws.dri.edu/}{RAWS USA Climate Archive}
#' @examples
#' \dontrun{
#' fileString <- wrcc_downloadData(20150701, 20150930, unitID = 'WENU')
#' tbl <- wrcc_parseData(fileString)
#' }

wrcc_parseData <- function(fileString) {
  
  logger.debug(" ----- wrcc_parseData() ----- ")
  
  # Identify monitor type
  monitorTypeList <- wrcc_identifyMonitorType(fileString)
  
  monitorType <- monitorTypeList$monitorType
  rawNames <- monitorTypeList$rawNames
  columnNames <- monitorTypeList$columnNames
  columnTypes <- monitorTypeList$columnTypes
  
  # Convert the fileString into individual lines
  lines <- readr::read_lines(fileString)
  
  if ( length(lines) <= 4 ) {
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
  lines <- stringr::str_replace(lines,'^ *','')
  lines <- stringr::str_replace(lines,' *$','')
  
  # Get monitorName from first line and then remove that line
  monitorName <- lines[1]
  lines <- lines[-1]
  
  # Remove header lines beginning with ":", leaving only data
  goodLines <- !is.na(lines) & !stringr::str_detect(lines,'^:')
  
  # Read the data into a tibble
  fakeFile <- paste0(lines[goodLines], collapse='\n')
  columnPositions <- readr::fwf_empty(fakeFile, col_names = columnNames)
  tbl <- readr::read_tsv(fakeFile, col_names = columnNames, col_types = columnTypes)
  
  return(tbl)
  
}
