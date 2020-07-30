#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#' @importFrom MazamaSpatialUtils loadSpatialData getCountryCode getStateCode getTimezone
#' 
#' @title Obtain metadata for each station in a state.
#'
#' @param stateCode Two character state code (will be downcased).
#'
#' @return Dataframe of station metadata.
#'
#' @description Obtains station metadata from a WRCC webservice and displays
#' it in a human-readable dataframe. The metadata includes station IDs, station
#' names, longitude, latitude, elevation, country codes, state codes, and 
#' timezones.
#' 
#' Passing a station ID in addition to the state code to \code{raws_getStationMetadata()}
#' will get a list of metadata for that specific station.
#'
#' @examples
#' \dontrun{
#' library(RAWSmet)
#'
#' meta <- raws_createMetadata(stateCode = 'WA')
#' dplyr::glimpse(meta)
#' }
#'
#' @rdname raws_createMetadata
#' @references \href{https://raws.dri.edu/}{RAWS USA Climate Archive}

raws_createMetadata <- function(
  stateCode = NULL
) {

  # ----- Validate parameters --------------------------------------------------
  
  stopIfNull(stateCode)
  
  # TODO: Check if stateCode is in list of state codes.
  
  # ----- Get station IDs ------------------------------------------------------
  
  stateCode <- tolower(stateCode)
  
  url <- paste0("https://raws.dri.edu/", tolower(stateCode), "lst.html")
  
  monitorIDs <- 
    MazamaCoreUtils::html_getLinkUrls(url) %>%                        # get links 
    stringr::str_subset(paste0("rawMAIN.pl\\?", stateCode)) %>%       # only keep those with "rawMAIN.pl?[stateCode]"
    stringr::str_match(paste0(".+MAIN.pl\\?", stateCode, "(.+)")) %>% # pull out everything after "MAIN.pl?[stateCode]"
    magrittr::extract(, 2)                                            # keep the second column of the matrix
  
  # ----- Get station metadata ------------------------------------------------------
  
  metadataList <- lapply(monitorIDs, raws_getStationMetadata, stateCode = stateCode)
  metadata <- dplyr::bind_rows(metadataList)
  
  return(metadata)
}

#' @rdname raws_createMetadata
#' @param stationID Four character station ID (will be upcased)
#' @export
raws_getStationMetadata <- function(
  stationID = NULL,
  stateCode = NULL
) {
  
  # ----- Setup ---------------------------------------------------------------
  
  loadSpatialData("USCensusStates")
  
  # ----- Validate parameters --------------------------------------------------
  
  stopIfNull(stationID)
  stopIfNull(stateCode)
  
  # TODO: Check if stateCode is in list of state codes.
  
  # ----- Get the data ---------------------------------------------------------
  
  metaUrl <- paste0("https://raws.dri.edu/cgi-bin/wea_info.pl?", tolower(stateCode), toupper(stationID))
  
  tables <- MazamaCoreUtils::html_getTables(metaUrl)
  
  if( length(tables) > 1) {
    metaTable <- tables[[2]]
  } else {
    metaTable <- tables[[1]]
  }
  
  siteName <- metaTable$X2[metaTable$X1 == "Location"]
  latitudeDMS <- metaTable$X2[metaTable$X1 == "Latitude"]
  longitudeDMS <- metaTable$X2[metaTable$X1 == "Longitude"]
  elevation <- metaTable$X2[metaTable$X1 == "Elevation"]
  
  # ----- Organize the data ----------------------------------------------------
  
  # Covert degrees/minutes/seconds coordinates to decimal degrees
  latitudeSplit <- stringr::str_split(latitudeDMS, " ", simplify = TRUE)
  longitudeSplit <- stringr::str_split(longitudeDMS, " ", simplify = TRUE)
  
  latitudeDegrees <- as.integer(stringr::str_sub(latitudeSplit[1], end = -2))
  latitudeMinutes <- as.integer(stringr::str_sub(latitudeSplit[2], end = -2))
  latitudeSeconds <- as.integer(stringr::str_sub(latitudeSplit[3], end = -6))
  
  longitudeDegrees <- as.integer(stringr::str_sub(longitudeSplit[1], end = -2))
  longitudeMinutes <- as.integer(stringr::str_sub(longitudeSplit[2], end = -2))
  longitudeSeconds <- as.integer(stringr::str_sub(longitudeSplit[3], end = -6))
  
  latitude <- latitudeDegrees + latitudeMinutes/60 + latitudeSeconds/3600
  # NOTE: The longitude is in degrees west. Negate to get it in degrees east.
  longitude <- -(longitudeDegrees + longitudeMinutes/60 + longitudeSeconds/3600)
  
  # Convert elevation in feet to meters
  elevation <- as.integer(stringr::str_sub(elevation, end = -5)) * 0.3048
  
  # Remove extra whitespace in siteName
  siteName <- gsub("\\s+", " ", siteName)
  
  # ----- Get extra information ------------------------------------------------
  
  # Get state code
  stateCode <- MazamaSpatialUtils::getStateCode(
    lon = longitude,
    lat = latitude,
    dataset = 'USCensusStates',
    useBuffering = TRUE
  )
  
  # Get country code
  countryCode <- MazamaSpatialUtils::getCountryCode(
    lon = longitude,
    lat = latitude,
    useBuffering = TRUE
  )
  
  # Get timezone
  timezone <- MazamaSpatialUtils::getTimezone(
    lon = longitude,
    lat = latitude,
    useBuffering = TRUE
  )
  
  # ----- Return ---------------------------------------------------------------
  
  ret <- list("stationID" = stationID, "siteName" = siteName, "longitude" = longitude, "latitude" = latitude, 
              "elevation" = elevation, "countryCode" = countryCode, "stateCode" = stateCode, "timezone" = timezone)
  
  return(ret)
}
