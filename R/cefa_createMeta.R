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
#'   \item{\code{nwsID} -- NWS station identifier (for FW13 data)}
#'   \item{\code{wrccID} -- WRCC station identifier (for WRCC data)}
#'   \item{\code{nessID} -- NESS station identifier (in WRCC data)}
#'   \item{\code{agencyName} -- responsible agency (in WRCC data)}
#' }
#'
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(RAWSmet)
#'
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial")
#'
#' meta <- cefa_createMeta(verbose = TRUE)
#' dplyr::glimpse(meta, width = 75)
#'
#' }, silent = FALSE)
#' }
#'
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}

cefa_createMeta <- function(
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

  col_names <- c("nwsID", "latitude", "longitude", "elevation", "locationName")
  col_types <- c("text", "numeric", "numeric", "numeric", "text")

  desiredColumns <- c(
    "deviceDeploymentID",
    "deviceID",
    "locationID",
    "locationName",
    "longitude",
    "latitude",
    "elevation",
    "countryCode",
    "stateCode",
    "timezone",
    "nwsID",
    "wrccID",
    "nessID",
    "agencyName"
  )

  meta <-

    # Read in contents of xlsx file
    readxl::read_xlsx(
      path = filePath,
      col_names = col_names,
      col_types = col_types,
      skip = 1
    ) %>%

    # Add locationID and deviceID
    dplyr::mutate(
      locationID = MazamaLocationUtils::location_createID(.data$longitude, .data$latitude),
      deviceID = stringr::str_pad(.data$nwsID, 6, pad = "0")
    ) %>%

    # Add all other required columns
    dplyr::mutate(
      deviceDeploymentID = paste0(.data$locationID, "_", .data$deviceID),
      countryCode = "US",
      stateCode = as.character(NA),
      timezone = as.character(NA),
      nwsID = .data$deviceID,
      wrccID = as.character(NA),
      nessID = as.character(NA),
      agencyName = as.character(NA)
    ) %>%

    # Reorder columns
    dplyr::select(all_of(desiredColumns))

  # > dplyr::glimpse(meta, width = 75)
  # Rows: 1,791
  # Columns: 13
  # $ deviceDeploymentID <chr> "f607cead84c92505_021503", "68bd6497ac6c9cd9_5
  # $ deviceID           <chr> "021503", "500726", "020401", "500742", "03210
  # $ locationID         <chr> "f607cead84c92505", "68bd6497ac6c9cd9", "31ccb
  # $ locationName       <chr> "AHAKAHV PRESERVE", "ALCAN HWY MI 1244", "ALPI
  # $ longitude          <dbl> -114.32750, -141.47000, -109.11167, -146.22000
  # $ latitude           <dbl> 34.13083, 62.83000, 33.84278, 65.02000, 35.572
  # $ elevation          <dbl> 360, 1800, 8188, 1100, 1600, 989, 804, 850, 18
  # $ countryCode        <chr> "US", "US", "US", "US", "US", "US", "US", "US"
  # $ stateCode          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
  # $ timezone           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
  # $ nwsID              <chr> "021503", "500726", "020401", "500742", "03210
  # $ wrccID             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
  # $ nessID             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
  # $ agencyName         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA

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

  # ----- Clean up and Return --------------------------------------------------

  unlink(filePath, force = TRUE)

  return(meta)

}
