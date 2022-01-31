#' @export
#' 
#' @name rawsDF_isRawsDF
#' @title Test for correct structure for a \emph{rawsDF} object
#' 
#' @param rawsDF \emph{rawsDF} object
#' 
#' @return \code{TRUE} if \code{rawsDF} has the correct structure, 
#' \code{FALSE} otherwise.
#' 
#' @description The \code{rawsDF} is checked for the presence of core data columns
#' 
#' Core columns include:
#' 
#' \itemize{
#'   \item{\code{datetime} -- datetime of the observation}
#'   \item{\code{temperature} -- temperature (C)}
#'   \item{\code{humidity} -- humidity (\%)}
#'   \item{\code{windSpeed} -- wind speed (m/s)}
#'   \item{\code{windDirection} -- wind direction (degrees)}
#'   \item{\code{maxGustSpeed} -- speed of max gust (m/s)}
#'   \item{\code{maxGustDirection} -- direction of max gust (degrees)}
#'   \item{\code{precipitation} -- precipitation (mm/h)}
#'   \item{\code{solarRadiation} -- solar radiation (W/m^2)}
#'   \item{\code{fuelMoisture} -- fuel moisture}
#'   \item{\code{fuelTemperature} -- fuel temperature (C)}
#'   \item{\code{monitorType} -- FW13 or WRCC depending on data source}
#'   \item{\code{nwsID} -- NWS station identifier (for FW13 data)}
#'   \item{\code{wrccID} -- WRCC station identifier (for WRCC data)}
#'   \item{\code{locationName} -- English language station name}
#'   \item{\code{longitude} -- decimal degrees E}
#'   \item{\code{latitude} -- decimal degrees N}
#'   \item{\code{timezone} -- timezone of the station}
#'   \item{\code{elevation} -- elevation of station in m}
#' }
#' 
#' @examples 
#' \donttest{
#' library(RAWSmet)
#' 
#' rawsDF <- example_fw13_Saddle_Mountain %>% raws_toRawsDF()
#' 
#' rawsDF_isRawsDF(rawsDF)
#' }
#' 
rawsDF_isRawsDF <- function(
  rawsDF = NULL
) {
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(rawsDF)
  
  if( !is.data.frame(rawsDF) )
    return(FALSE)
  
  requiredNames <- c('datetime', 'temperature', 'humidity', 'windSpeed', 
                     'windDirection', 'maxGustSpeed', 'maxGustDirection', 
                     'precipitation', 'solarRadiation', 'fuelMoisture', 
                     'fuelTemperature', 'monitorType', 'nwsID', 'wrccID', 
                     'locationName', 'longitude', 'latitude', 'timezone', 'elevation')
  
  if ( !all(requiredNames %in% names(rawsDF)) )
    return(FALSE)
  
  # Nothing failed so return TRUE
  return(TRUE)
  
}