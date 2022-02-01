#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error html_getLinks
#' @importFrom MazamaSpatialUtils getTimezone US_stateCodeToName
#'
#' @title Obtain metadata for each station in a state.
#'
#' @param stateCode Two character state code (will be downcased).
#' @param wrccIDs Vector of wrccIDs to be used instead of \code{stateCode}.
#' @param baseUrl Base URL for data queries.
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @return Dataframe of station metadata.
#'
#' @description Assembles individual station metadata from a WRCC webservice
#' into a standardized dataframe. Metdata columns include:
#'
#' \itemize{
#'   \item{\code{deviceDeploymentID} -- unique identifier}
#'   \item{\code{deviceID} -- device identifier}
#'   \item{\code{locationID} -- location identifier}
#'   \item{\code{locationName} -- English language name}
#'   \item{\code{longitude} -- decimal degrees E}
#'   \item{\code{latitude} -- decimal degrees N}
#'   \item{\code{elevation} -- elevation of station in m}
#'   \item{\code{countryCode} -- ISO 3166-1 alpha-2}
#'   \item{\code{stateCode} -- ISO 3166-2 alpha-2}
#'   \item{\code{timezone} -- Olson time zone}
#'   \item{\code{nwsID} -- NWS station identifier}
#'   \item{\code{wrccID} -- WRCC station identifier (in WRCC data)}
#'   \item{\code{nessID} -- NESS station identifier (in WRCC data)}
#'   \item{\code{agencyName} -- responsible agency (in WRCC data)}
#' }
#'
#' Because of the large number of web requests required to assemble this
#' metadata, it is recommended that the file be saved and reused by calling
#' \code{wrcc_loadMeta()}.
#'
#' @seealso \code{wrcc_loadMeta}
#' @seealso \code{setRawsDataDir}
#'
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(RAWSmet)
#'
#' wa_meta <- wrcc_createMeta(stateCode = 'WA', verbose = TRUE)
#'
#' dplyr::glimpse(wa_meta, width = 75)
#'
#' }, silent = FALSE)
#' }
#'
#' @references \href{https://raws.dri.edu/}{RAWS USA Climate Archive}

wrcc_createMeta <- function(
  stateCode = NULL,
  wrccIDs = NULL,
  baseUrl = "https://raws.dri.edu/",
  verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(stateCode) && is.null(wrccIDs) )
    stop("either 'stateCode' or 'wrccIDs' must be specified")

  if ( ! is.null(stateCode) && ! toupper(stateCode) %in% MazamaSpatialUtils::US_stateCodes$stateCode )
    stop("parameter 'stateCode' is not a valid state code")

  if ( !is.null(stateCode) )
    stateCode <- tolower(stateCode)

  # ----- Get station IDs ------------------------------------------------------

  if ( is.null(wrccIDs) ) {

    # NOTE: California, Idaho, and Montana are split into multiple lists so we
    #       must concatenate these lists.
    #       Also, some states are combined into the same list so we must have
    #       an extra check to get the correct list for these states.
    if ( stateCode == 'ca' ) {

      # Get the north, central, and south urls
      northUrl <- paste0(baseUrl, "ncalst.html")
      centralUrl <- paste0(baseUrl, "ccalst.html")
      southUrl <- paste0(baseUrl, "scalst.html")

      # Concatenate the links
      stationLinks <-
        MazamaCoreUtils::html_getLinks(northUrl) %>%
        rbind(MazamaCoreUtils::html_getLinks(centralUrl)) %>%
        rbind(MazamaCoreUtils::html_getLinks(southUrl))

    } else if ( stateCode == 'id' ) {

      # Get the north and south urls
      northUrl <- paste0(baseUrl, "nidwmtlst.html")
      southUrl <- paste0(baseUrl, "sidlst.html")

      # Concatenate the links
      stationLinks <-
        MazamaCoreUtils::html_getLinks(northUrl) %>%
        rbind(MazamaCoreUtils::html_getLinks(southUrl))

    } else if ( stateCode == 'mt' ) {

      # Get the west and east urls
      westUrl <- paste0(baseUrl, "nidwmtlst.html")
      eastUrl <- paste0(baseUrl, "emtlst.html")

      # Concatenate the links
      stationLinks <-
        MazamaCoreUtils::html_getLinks(westUrl) %>%
        rbind(MazamaCoreUtils::html_getLinks(eastUrl))

    } else {

      combinedStates <- c("me_nh_vt", "ct_ma_ri", "nj_pa", "ky_tn", "mi_wi", "va_wv", "de_md", "ga_sc", "al_ms")

      if ( any(stringr::str_detect(combinedStates, stateCode)) ) {

        # If the state is in one of these combines lists
        combinedPath = stringr::str_subset(combinedStates, stateCode)
        url <- paste0(baseUrl, combinedPath, "lst.html")

      } else {

        # If the state does not match any of these cases
        url <- paste0(baseUrl, stateCode, "lst.html")

      }

      # Get the links
      stationLinks <- MazamaCoreUtils::html_getLinks(url)

    } # END partial-/multi-state combinations

    # Get the name of the state
    stateName <- MazamaSpatialUtils::US_stateCodeToName(stateCode)

    # Get the station IDs for the given state
    wrccIDs <-
      stationLinks %>%
      dplyr::filter(stringr::str_detect(.data$linkName, stateName)) %>% # Get stations in the requested state
      dplyr::pull(.data$linkUrl) %>%                                    # Get only the link URLs
      stringr::str_subset("rawMAIN.pl\\?") %>%                          # only keep those with "rawMAIN.pl?"
      stringr::str_replace("/cgi-bin/rawMAIN.pl\\?", "")                # keep everything after "MAIN.pl?"

    # Stop if there are no stations in the given state
    if ( rlang::is_empty(wrccIDs) )
      stop(sprintf("Could not find any stations in the state with state code '%s'", stateCode))

  } # END of wrccIDs from stateCode

  # ----- Loop over wrccIDs -------------------------------------------------

  recordList <- list()
  i <- 0
  for ( wrccID in wrccIDs ) {

    i <- i + 1
    if ( verbose )
      message(sprintf("Working on %03d/%03d: %s...", i, length(wrccIDs), wrccID))

    # * Get the data -----

    metaUrl <- paste0("https://raws.dri.edu/cgi-bin/wea_info.pl?", wrccID)

    tables <- MazamaCoreUtils::html_getTables(metaUrl)

    # NOTE: If a station has photos, the first table on the site will contain the photos.
    if ( length(tables) > 1) {
      metaTable <- tables[[2]]
    } else {
      metaTable <- tables[[1]]
    }

    locationName <- metaTable$X2[metaTable$X1 == "Location"]
    latitudeDMS <- metaTable$X2[metaTable$X1 == "Latitude"]
    longitudeDMS <- metaTable$X2[metaTable$X1 == "Longitude"]
    elevation <- metaTable$X2[metaTable$X1 == "Elevation"]

    if ( "X3" %in% names(metaTable) ) {
      nessID <- metaTable$X4[metaTable$X3 == "NESS ID" & !is.na(metaTable$X3)]
      nwsID <- metaTable$X4[metaTable$X3 == "NWS ID" & !is.na(metaTable$X3)]
      agencyName <- metaTable$X4[metaTable$X3 == "Agency" & !is.na(metaTable$X3)]
    } else {
      nessID <- as.character(NA)
      nwsID <- as.character(NA)
      agencyName <- as.character(NA)
    }

    # * Convert to internal standard -----

    # Scrape the coordinate data
    # NOTE: Coordinate data comes in like "48\u00b0 44' 35&quot"

    latitudeSplit <- stringr::str_match(latitudeDMS, "(\\d+).* (\\d+).* (\\d+)")

    latitude <-
      as.numeric(latitudeSplit[2]) +
      as.numeric(latitudeSplit[3]) / 60 +
      as.numeric(latitudeSplit[4]) / 3600

    longitudeSplit <- stringr::str_match(longitudeDMS, "(\\d+).* (\\d+).* (\\d+)")

    # NOTE: The longitude is in degrees west. Negate to get it in degrees east.
    longitude <- -1.0 * (
      as.numeric(longitudeSplit[2]) +
        as.numeric(longitudeSplit[3]) / 60 +
        as.numeric(longitudeSplit[4]) / 3600
    )

    # Convert elevation in feet to meters
    elevationSplit <- stringr::str_match(elevation, "(\\d+)")
    elevation <- round(as.integer(stringr::str_sub(elevationSplit[2])) * 0.3048)

    # Remove extra whitespace in locationName
    locationName <- stringr::str_trim(locationName)

    # Remove state name from locationName
    stateCode <- stringr::str_sub(wrccID, 0, 2)
    stateName <- MazamaSpatialUtils::US_stateCodeToName(stateCode)

    locationName <- stringr::str_trim(stringr::str_sub(locationName, 1, nchar(locationName) - nchar(stateName)))

    # * Create deviceDeploymentID -----

    # Use nwsID as deviceID if possible (this matches cefa_createMeta())
    deviceID <- nwsID
    if ( is.na(deviceID) || deviceID == "" )
      deviceID <- wrccID

    locationID <- MazamaLocationUtils::location_createID(longitude, latitude)
    deviceDeploymentID <- paste0(locationID, "_", deviceID)

    # * Assemble tibble -----

    recordList[[wrccID]] <-
      dplyr::tibble(
        "deviceDeploymentID" = deviceDeploymentID,
        "deviceID" = deviceID,
        "locationID" = locationID,
        "locationName" = locationName,
        "longitude" = longitude,
        "latitude" = latitude,
        "elevation" = elevation,
        "countryCode" = "US",
        "stateCode" = toupper(stateCode),
        "timezone" = as.character(NA),
        "nwsID" = nwsID,
        "wrccID" = wrccID,
        "nessID" = nessID,
        "agencyName" = agencyName
      )

  }

  # ----- Assemble dataframe ---------------------------------------------------

  meta <- dplyr::bind_rows(recordList)

  # Make sure the timezone dataset is loaded
  if ( !exists("SimpleTimezones") )
    utils::data("SimpleTimezones", package = "MazamaSpatialUtils")

  # Now add the timezone
  meta$timezone <- MazamaSpatialUtils::getTimezone(meta$longitude, meta$latitude)

  # ----- Return ---------------------------------------------------------------

  return(meta)

}
