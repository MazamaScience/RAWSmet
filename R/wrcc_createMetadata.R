#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#' @importFrom MazamaSpatialUtils loadSpatialData getCountryCode getStateCode getTimezone
#' 
#' @title Obtain metadata for each station in a state.
#'
#' @param stateCode Two character state code (will be downcased).
#' @param stationIDs Vector of stationIDs to be used instead of \code{stateCode}.
#' @param baseUrl Base URL for data queries.
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @return Dataframe of station metadata.
#'
#' @description Assembles individual station metadata from a WRCC webservice 
#' into a standardized dataframe. Metdata columns include:
#' 
#' \itemize{
#' \item{\code{countryCode} -- ISO 3166-1 alpha-2 country code}
#' \item{\code{stateCode} -- ISO 3166-2 alpha-2 state code}
#' \item{\code{stationID} -- WRCC RAWS station identifier}
#' \item{\code{siteName} -- human readable site name}
#' \item{\code{longitude} -- decimal degrees East}
#' \item{\code{latitude} -- decimal degrees North}
#' \item{\code{elevation} -- in meters}
#' \item{\code{timezone} -- Olson timezone}
#' }
#' 
#' Because of the large number of web requests required to assemble this 
#' metadata, it is recommended that the file be saved and reused.
#' 
#' @examples
#' \dontrun{
#' library(RAWSmet)
#'
#' wa_meta <- wrcc_createMetadata(stateCode = 'WA', verbose = TRUE)
#' dplyr::glimpse(wa_meta)
#' }
#'
#' @references \href{https://raws.dri.edu/}{RAWS USA Climate Archive}

wrcc_createMetadata <- function(
  stateCode = NULL,
  stationIDs = NULL,
  baseUrl = "https://raws.dri.edu/",
  verbose = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( is.null(stateCode) && is.null(stationIDs) ) 
    stop("Either 'stateCode' or 'stationIDs' must be specified.")
  
  # TODO: Check if stateCode is in list of state codes.
  
  # ----- Get station IDs ------------------------------------------------------
  
  if ( is.null(stationIDs) ) {
    
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
    
  } #END of stationIDs from stateCode
  
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
    nessID <- metaTable$X4[metaTable$X3 == "NESS ID" & !is.na(metaTable$X3)]
    nwsID <- metaTable$X4[metaTable$X3 == "NWS ID" & !is.na(metaTable$X3)]
    agency <- metaTable$X4[metaTable$X3 == "Agency" & !is.na(metaTable$X3)]
    
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
    elevation <- round(as.integer(stringr::str_sub(elevation, end = -5)) * 0.3048)
    
    # Remove extra whitespace in siteName
    siteName <- gsub("\\s+", " ", siteName)
    
    # Get stateCode from stationID
    stateCode <- toupper(stringr::str_sub(stationID, 1, 2))
    
    # Check if identifies are valid
    # NOTE: identifiers will not be a character if the table is missing.
    # NOTE: identifiers will be empty strings ("") if the table exists but the
    #       value is empty.
    if ( !is.character(nessID) || nessID == "")
      nessID = NA
    if ( !is.character(nwsID) || nwsID == "")
      nwsID = NA
    if ( !is.character(agency) || agency == "")
      agency = NA
    
    recordList[[stationID]] <-
      dplyr::tibble(
        "countryCode" = "US", 
        "stateCode" = stateCode, 
        "stationID" = stationID, 
        "siteName" = siteName, 
        "longitude" = longitude, 
        "latitude" = latitude, 
        "elevation" = elevation,
        "nessID" = nessID,
        "nwsID" = nwsID,
        "agency" = agency
      )
    
  }
  
  # ----- Assemble dataframe ---------------------------------------------------
  
  meta <- dplyr::bind_rows(recordList)
  
  # Make sure the timezone dataset is loaded
  if ( !exists("SimpleTimezones") )
    utils::data("SimpleTimezones", package = "MazamaSpatialUtils")
  
  # Now add the timezone
  meta$timezone <- MazamaSpatialUtils::getTimezone(meta$longitude, meta$latitude)
  
  return(meta)
  
}
