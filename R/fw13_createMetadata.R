#' @export
#' @importFrom MazamaSpatialUtils loadSpatialData getStateCode getTimezone
#' @importFrom readxl read_xlsx
#' @importFrom rlang .data
#' 
#' @title Obtain metadata for each station.
#'
#' @param stationIDs Vector of stationIDs to be used instead of \code{stateCode}.
#' @param metadataUrl Base URL for station metadata.
#'
#' @return Dataframe of station metadata.
#'
#' @description Assembles individual station metadata from a CEFA webservice 
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
#' @examples
#' \dontrun{
#' library(RAWSmet)
#'
#' meta <- fw13_createMetadata()
#' dplyr::glimpse(meta)
#' }
#'
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}

fw13_createMetadata <- function(
  stationIDs = NULL,
  metadataUrl = "https://cefa.dri.edu/raws/RAWSfw13list.xlsx"
) {
  
  # ----- Setup ----------------------------------------------------------------
  
  dataDir <- MazamaSpatialUtils::getSpatialDataDir()
  loadSpatialData("NaturalEarthAdm1")
  loadSpatialData("SimpleTimezones")
  
  # ----- Download station metadata --------------------------------------------
  
  filePath <- file.path(dataDir, "RAWSfw13list.xlsx")
  utils::download.file("https://cefa.dri.edu/raws/RAWSfw13list.xlsx", destfile = filePath)
  meta <- readxl::read_xlsx(filePath)
  
  if ( !is.null(stationIDs) )
    meta <- meta %>% dplyr::filter(.data$StationID %in% stationIDs)
  
  # ----- Add extra metadata ---------------------------------------------------
  
  # Get state codes from lat/lon coordinates
  meta$stateCode <- MazamaSpatialUtils::getStateCode(meta$LonDegrees, meta$LatDegrees)
  
  # Get timezones
  meta$timezone <- MazamaSpatialUtils::getTimezone(meta$LonDegrees, meta$LatDegrees)
  
  # ----- Assemble tibble ------------------------------------------------------
  
  meta <-
    dplyr::tibble(
      countryCode = "US",
      stateCode = meta$stateCode,
      fw13ID = meta$StationID,
      siteName = meta$Name,
      longitude = meta$LonDegrees,
      latitude = meta$LatDegrees,
      elevation = meta$Elevation,
      timezone = meta$timezone
  )
  
  # ----- Return ---------------------------------------------------------------
  
  return(meta)
  
}
