#' @export
#' @importFrom readxl read_xlsx
#' @importFrom rlang .data
#' @importFrom dplyr all_of
#'
#' @title Obtain metadata for every station.
#'
#' @param metadataUrl URL for station metadata.
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @return Dataframe of station metadata.
#'
#' @description Assembles individual station metadata from a CEFA webservice
#' into a standardized dataframe. Metdata columns include:
#'
#' \itemize{
#' \item{\code{nwsID} -- NWS RAWS station identifier}
#' \item{\code{longitude} -- decimal degrees East}
#' \item{\code{latitude} -- decimal degrees North}
#' \item{\code{elevation} -- in meters}
#' \item{\code{siteName} -- human readable site name}
#' \item{\code{countryCode} -- ISO 3166-1 alpha-2 country code}
#' \item{\code{stateCode} -- ISO 3166-2 alpha-2 state code}
#' \item{\code{timezone} -- Olson timezone}
#' }
#'
#' @examples
#' \dontrun{
#' library(RAWSmet)
#'
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial")
#'
#' meta <- fw13_createMeta(verbose = TRUE)
#' dplyr::glimpse(meta)
#' }
#'
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}

fw13_createMeta <- function(
  metadataUrl = "https://cefa.dri.edu/raws/RAWSfw13list.xlsx",
  verbose = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  if ( !isNamespaceLoaded("MazamaSpatialUtils") ) {
    suppressPackageStartupMessages({
      attachNamespace("MazamaSpatialUtils")
    })
  }

  MazamaSpatialUtils::loadSpatialData("NaturalEarthAdm1.rda")

  # ----- Download station metadata --------------------------------------------


  filePath <- file.path(tempdir(), "RAWSfw13list.xlsx")
  utils::download.file("https://cefa.dri.edu/raws/RAWSfw13list.xlsx", destfile = filePath)

  col_names <- c("nwsID", "latitude", "longitude", "elevation", "siteName")
  col_types <- c("text", "numeric", "numeric", "numeric", "text")

  desiredColumns <- c(
    "nwsID",
    "longitude",
    "latitude",
    "elevation",
    "siteName",
    "countryCode"
  )

  meta <-
    readxl::read_xlsx(
      path = filePath,
      col_names = col_names,
      col_types = col_types,
      skip = 1
    ) %>%
    dplyr::mutate(
      nwsID = stringr::str_pad(.data$nwsID, 6, pad = "0"),
      countryCode = "US"
    ) %>%
    dplyr::select(all_of(desiredColumns))

  # > dplyr::glimpse(meta)
  # Rows: 1,791
  # Columns: 6
  # $ nwsID       <chr> "021503", "500726", "020401", "500742", "032101", "010702"…
  # $ longitude   <dbl> -114.32750, -141.47000, -109.11167, -146.22000, -92.81917,…
  # $ latitude    <dbl> 34.13083, 62.83000, 33.84278, 65.02000, 35.57278, 34.34389…
  # $ elevation   <dbl> 360, 1800, 8188, 1100, 1600, 989, 804, 850, 1855, 1850, 48…
  # $ siteName    <chr> "AHAKAHV PRESERVE", "ALCAN HWY MI 1244", "ALPINE", "ANGEL …
  # $ countryCode <chr> "US", "US", "US", "US", "US", "US", "US", "US", "US", "US"…

  # ----- Add spatial metadata -------------------------------------------------

  if ( verbose )
    message("Assigning 'stateCode'...")

  # Get state codes from lat/lon coordinates
  meta$stateCode <- MazamaSpatialUtils::getStateCode(
    meta$longitude,
    meta$latitude,
    countryCodes = c("US"),
    useBuffering = TRUE
  )

  if ( verbose )
    message("Assigning 'timezone'...")

  # Get timezones
  meta$timezone <- MazamaSpatialUtils::getTimezone(
    meta$longitude,
    meta$latitude,
    countryCodes = c("US"),
    useBuffering = TRUE
  )

  # ----- Add empty columns ----------------------------------------------------

  # NOTE: These columns exist in WRCC metadata so we add these to keep
  #       the columns of RAWS metadata consistent
  meta$nessID <- as.character(NA)
  meta$wrccID <- as.character(NA)
  meta$agency <- as.character(NA)


  # ----- Reorder columns ----------------------------------------------------

  meta <-
    dplyr::select(
      .data = meta,
      nwsID = .data$nwsID,
      wrccID = .data$wrccID,
      nessID = .data$nessID,
      siteName = .data$siteName,
      longitude = .data$longitude,
      latitude = .data$latitude,
      timezone = .data$timezone,
      elevation = .data$elevation,
      countryCode = .data$countryCode,
      stateCode = .data$stateCode,
      agency = .data$agency
    )

  # ----- Clean up and Return --------------------------------------------------

  unlink(filePath, force = TRUE)

  return(meta)

}
