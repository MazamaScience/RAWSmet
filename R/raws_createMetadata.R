#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#' @importFrom MazamaSpatialUtils loadSpatialData getCountryCode getStateCode getTimezone
#' 
#' @title Obtain metadata for each station in a state.
#'
#' @param stateCode Two character state code (will be downcased).
#' @param baseUrl Base URL for data queries.
#' @param verbose Logical flag controlling detailed progress statements.
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
  stateCode = NULL,
  baseUrl = "https://raws.dri.edu/",
  verbose = FALSE
) {

  # ----- Validate parameters --------------------------------------------------
  
  stopIfNull(stateCode)
  
  # TODO: Check if stateCode is in list of state codes.
  
  # ----- Get station IDs ------------------------------------------------------
  
  stateCode <- tolower(stateCode)
  
  # NOTE: California is separated into north and south. We have to get these
  #       two lists separately. 
  if ( stateCode == 'ca' ) {
    
    northUrl <- paste0(baseUrl, "ncalst.html")
    southUrl <- paste0(baseUrl, "scalst.html")
    
    stationIDs <- 
      MazamaCoreUtils::html_getLinkUrls(northUrl) %>%        # get links 
      append(MazamaCoreUtils::html_getLinkUrls(southUrl)) %>%
      stringr::str_subset("rawMAIN.pl\\?") %>%              # only keep those with "rawMAIN.pl?"
      stringr::str_match(".+MAIN.pl\\?(.+)") %>%            # pull out everything after "MAIN.pl?"
      magrittr::extract(, 2)                                # keep the second column of the matrix
    
  } else {
    
    url <- paste0(baseUrl, tolower(stateCode), "lst.html")
    
    stationIDs <- 
      MazamaCoreUtils::html_getLinkUrls(url) %>%  # get links 
      stringr::str_subset("rawMAIN.pl\\?") %>%    # only keep those with "rawMAIN.pl?"
      stringr::str_match(".+MAIN.pl\\?(.+)") %>%  # pull out everything after "MAIN.pl?"
      magrittr::extract(, 2)                      # keep the second column of the matrix   
    
  }
  
  # Filter for stations not in the requested state
  stationIDs <- stringr::str_subset(stationIDs, paste0("^", stateCode) )
  
  # ----- Loop over stationIDs -------------------------------------------------
  
  recordList <- list()
  i <- 0
  for ( stationID in stationIDs ) {
   
    i <- i + 1
    if ( verbose )
      message(sprintf("Working on %03d/%03d: %s...", i, length(stationIDs), stationID))
      
    # * Get the data -----
    
    metaUrl <- paste0("https://raws.dri.edu/cgi-bin/wea_info.pl?", stationID)
    
    tables <- MazamaCoreUtils::html_getTables(metaUrl)
    
    # NOTE: If a station has photos, the first table on the site will contain the photos.
    if ( length(tables) > 1) {
      metaTable <- tables[[2]]
    } else {
      metaTable <- tables[[1]]
    }
    
    siteName <- metaTable$X2[metaTable$X1 == "Location"]
    latitudeDMS <- metaTable$X2[metaTable$X1 == "Latitude"]
    longitudeDMS <- metaTable$X2[metaTable$X1 == "Longitude"]
    elevation <- metaTable$X2[metaTable$X1 == "Elevation"]
    
    # * Convert to internal standard -----
    
    # Scrape the coordinate data
    # NOTE: Coordinate data comes in like "48° 44' 35&quot"
    latitudeSplit <- stringr::str_split(latitudeDMS, " ", simplify = TRUE)
    longitudeSplit <- stringr::str_split(longitudeDMS, " ", simplify = TRUE)
    
    latitudeDegrees <- as.integer(stringr::str_sub(latitudeSplit[1], end = -2))
    latitudeMinutes <- as.integer(stringr::str_sub(latitudeSplit[2], end = -2))
    latitudeSeconds <- as.integer(stringr::str_sub(latitudeSplit[3], end = -6))
    
    longitudeDegrees <- as.integer(stringr::str_sub(longitudeSplit[1], end = -2))
    longitudeMinutes <- as.integer(stringr::str_sub(longitudeSplit[2], end = -2))
    longitudeSeconds <- as.integer(stringr::str_sub(longitudeSplit[3], end = -6))
    
    # Covert degrees/minutes/seconds coordinates to decimal degrees
    latitude <- latitudeDegrees + latitudeMinutes/60 + latitudeSeconds/3600
    # NOTE: The longitude is in degrees west. Negate to get it in degrees east.
    longitude <- -(longitudeDegrees + longitudeMinutes/60 + longitudeSeconds/3600)
    
    # Convert elevation in feet to meters
    elevation <- as.integer(stringr::str_sub(elevation, end = -5)) * 0.3048
    
    # Remove extra whitespace in siteName
    siteName <- gsub("\\s+", " ", siteName)
    
    recordList[[stationID]] <-
      dplyr::tibble(
        "countryCode" = "US", 
        "stateCode" = toupper(stateCode), 
        "stationID" = stationID, 
        "siteName" = siteName, 
        "longitude" = longitude, 
        "latitude" = latitude, 
        "elevation" = elevation      
      )
    
  }
  
  # ----- Assemble dataframe ---------------------------------------------------
  
  meta <- dplyr::bind_rows(recordList)
  
  # Now add the timezeon
  meta$timezone <- MazamaSpatialUtils::getTimezone(meta$longitude, meta$latitude)

  return(meta)
  
}
