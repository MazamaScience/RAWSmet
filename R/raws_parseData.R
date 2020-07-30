#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Parse RAWS data string
#'
#' @param fileString character string containing RAWS data
#' @description Raw character data from WRCC are parsed into a tibble.
#' The incoming \code{fileString}
#' can be read in directly from WRCC using \code{raws_downloadData()} or from a local
#' file using \code{readr::read_file()}.
#'
#' The type of monitor represented by this fileString is inferred from the column names
#' using \code{raws_identifyMonitorType()} and appropriate column types are assigned.
#' The character data are then processed, read into a tibble and augmented in the following ways:
#' \enumerate{
#' \item{Spaces at the beginning and end of each line are moved.}
#' \item{All header lines beginning with ':' are removed.}
#' }
#' @return Dataframe of RAWS raw station data.
#' @references \href{https://raws.dri.edu/}{RAWS USA Climate Archive}
#' @examples
#' \dontrun{
#' fileString <- raws_downloadData(unitID = 'WENU')
#' tbl <- raws_parseData(fileString)
#' }

raws_parseData <- function(fileString) {

  if( MazamaCoreUtils::logger.isInitialized() )
    logger.debug(" ----- wrcc_parseData() ----- ")

  # Identify monitor type
  monitorTypeList <- raws_identifyMonitorType(fileString)
  
  monitorType <- monitorTypeList$monitorType
  rawNames <- monitorTypeList$rawNames
  columnNames <- monitorTypeList$columnNames
  columnTypes <- monitorTypeList$columnTypes
  
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
  tbl <- readr::read_tsv(fakeFile, col_names = columnNames, col_types = columnTypes)
  
  # Convert -9999 to NA
  tbl[] <- tbl %>% lapply(gsub, pattern = -9999, replacement = NA)

  return(tbl)
  
}
