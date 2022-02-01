#' @export
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
#' \item{"\code{WRCC_TYPE3}"}{ -- ???}
#' \item{"\code{UNKOWN}"}{ -- ???}
#' }
#'
#' @return List including \code{monitorType}, \code{rawNames}, \code{columnNames} and \code{columnTypes}.
#' @references \href{https://raws.dri.edu/}{RAWS USA Climate Archive}
#' @examples
#' \dontrun{
#' library(RAWSmet)
#'
#' fileString <- wrcc_downloadData(wrccID = 'WENU')
#' monitorTypeList <- wrcc_identifyMonitorType(fileString)
#' str(monitorTypeList)
#' }

wrcc_identifyMonitorType <- function(
  fileString = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(fileString)

  if ( class(fileString)[1] != "character" )
    stop(paste0('WRCC fileString is of type %s', class(fileString)[1]))

  # ----- Extract  header lines from the incoming fileString -------------------

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

  # Get each column from the header and trim whitespaces and the extra ':'
  line1Split <- stringr::str_split(header[1], '\t')[[1]] %>% stringr::str_replace_all(':', '') %>% stringr::str_replace_all(' ', '')
  line2Split <- stringr::str_split(header[2], '\t')[[1]] %>% stringr::str_replace_all(':', '') %>% stringr::str_replace_all(' ', '')
  line3Split <- stringr::str_split(header[3], '\t')[[1]] %>% stringr::str_replace_all(':', '') %>% stringr::str_replace_all(' ', '')

  # ----- Harmonize unit name variations ---------------------------------------

  # NOTE:  Some WRCC stations have different names for many of the standard columns.
  # NOTE:  The following vectors are the different variations of these names.

  mmVariations <- c("mm")
  inVariations <- c("in")
  m_sVariations <- c("m/s")
  mphVariations <- c("mph")
  degVariations <- c("Deg")
  degCVariations <- c("DegC", "Deg C")
  degFVariations <- c("DegF", "Deg F")

  # Units come from the first line
  rawUnits <- line1Split

  columnUnits <- rawUnits
  # Replace any column names differing from the standard with its standardized name
  for ( var in mmVariations )
    columnUnits <- columnUnits %>% stringr::str_replace(var, "mm")
  for ( var in inVariations )
    columnUnits <- columnUnits %>% stringr::str_replace(var, "in")
  for ( var in m_sVariations )
    columnUnits <- columnUnits %>% stringr::str_replace(var, "m/s")
  for ( var in mphVariations )
    columnUnits <- columnUnits %>% stringr::str_replace(var, "mph")
  for ( var in degVariations )
    columnUnits <- columnUnits %>% stringr::str_replace(var, "deg")
  for ( var in degCVariations )
    columnUnits <- columnUnits %>% stringr::str_replace(var, "degC")
  for ( var in degFVariations )
    columnUnits <- columnUnits %>% stringr::str_replace(var, "degF")

  # ----- Harmonize column name variations -------------------------------------

  # NOTE:  Some WRCC stations have different names for many of the standard columns.
  # NOTE:  The following vectors are the different variations of these names.

  datetimeVariations <- c("Date/TimeYYMMDDhhmm")
  precipVariations <- c("Precip")
  windSpeedVariations <- c("WindSpeed")
  windDirecVariations <- c("WindDirec")
  avAirTempVariations <- c("AvAirTemp")
  fuelTempVariations <- c("FuelTemp")
  relHumidtyVariations <- c("RelHumidty")
  batteryVoltageVariations <- c("BatteryVoltage")
  avFuelMoistrVariations <- c("AvFuelMoistr", "AvFuelMoistur", "FuelMoistur", "FuelMoistr", "AvAvFuelMoistr")
  dirMxGustVariations <- c("DirMxGust")
  mxGustSpeedVariations <- c("MxGustSpeed")
  solarRadVariations <- c("SolarRad.")

  # Concatenate the second and third line
  rawNames <- paste0(line2Split, line3Split)

  columnNames <- rawNames
  # Replace any column names differing from the standard with its standardized name
  for ( var in datetimeVariations )
    columnNames <- columnNames %>% stringr::str_replace(var, "datetime")
  for ( var in precipVariations )
    columnNames <- columnNames %>% stringr::str_replace(var, "precipitation")
  for ( var in windSpeedVariations )
    columnNames <- columnNames %>% stringr::str_replace(var, "windSpeed")
  for ( var in windDirecVariations )
    columnNames <- columnNames %>% stringr::str_replace(var, "windDirection")
  for ( var in avAirTempVariations )
    columnNames <- columnNames %>% stringr::str_replace(var, "temperature")
  for ( var in fuelTempVariations )
    columnNames <- columnNames %>% stringr::str_replace(var, "fuelTemperature")
  for ( var in relHumidtyVariations )
    columnNames <- columnNames %>% stringr::str_replace(var, "humidity")
  for ( var in batteryVoltageVariations )
    columnNames <- columnNames %>% stringr::str_replace(var, "batteryVoltage")
  for ( var in avFuelMoistrVariations )
    columnNames <- columnNames %>% stringr::str_replace(var, "fuelMoisture")
  for ( var in dirMxGustVariations )
    columnNames <- columnNames %>% stringr::str_replace(var, "maxGustDirection")
  for ( var in mxGustSpeedVariations )
    columnNames <- columnNames %>% stringr::str_replace(var, "maxGustSpeed")
  for ( var in solarRadVariations )
    columnNames <- columnNames %>% stringr::str_replace(var, "solarRadiation")

  columnTypes <- 'c'
  columnTypes <- stringr::str_pad(columnTypes, length(rawNames), 'right', 'd')

  # NOTE: Some headers have multiple columns named ''. Further down the line this
  #       confused readr::read_tsv so we must rename them to something unique.
  columnNames[columnNames == ''] <- paste0("_", seq(length(columnNames[columnNames == ''])))

  # Default to "UNKONWN" type of monitor
  # monitorType <- "UNKNOWN"

  # ----- Return ---------------------------------------------------------------

  monitorTypeList <- list(
    monitorType = "WRCC_REGULARIZED",
    rawNames = rawNames,
    columnNames = columnNames,
    columnTypes = columnTypes,
    columnUnits = columnUnits
  )

}
