#' @export
#' @importFrom rlang .data
#' @importFrom dplyr filter
#'
#' @title Load FW13 RAWS timeseries object from a local directory
#'
#' @param nwsID NWS RAWS station identifier.
#' @param meta Tibble of FW13 station metadata.
#' @param newDownload Logical flag specifying whether or not to download and override existing data.
#' @param baseUrl Base URL for data queries.
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @return Timeseries object with 'meta' and 'data'.
#'
#' @description Loads FW13 station metadata and data from the \code{rawsDataDir}.
#' If the data is not in this directory, this will download and save the data.
#'
#' @note The `newDownload` parameter has three possible settings:
#' \itemize{
#' \item{\code{NA} -- Download data if it is not found in \code{rawsDataDir}}
#' \item{\code{TRUE} -- Always download data, overwriting existing data in \code{rawsDataDir}.
#' This is useful for updating data files with more recent data.}
#' \item{\code{FALSE} -- Never download data. This is useful when working with
#' \link{wrcc_loadMultiple} and archival data to avoid continually requesting
#' data for stations which have no data over a particular time period.}
#' }
#'
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(RAWSmet)
#' setRawsDataDir("~/Data/RAWS/")
#'
#' # For creation of metadata
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("NaturalEarthAdm1.rda")
#'
#' meta <- fw13_loadMeta()
#'
#' rawsObject <- fw13_load(nwsID = 451702, meta = meta)
#' head(rawsObject$data)
#'
#' }, silent = FALSE)
#' }
#'
#' @seealso \code{fw13_createRawsObject}
#' @seealso \code{setRawsDataDir}
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}

fw13_load <- function(
  nwsID = NULL,
  meta = NULL,
  newDownload = NA,
  baseUrl = "https://cefa.dri.edu/raws/fw13/",
  verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(nwsID)

  currentYear <- lubridate::now(tzone = "UTC") %>% lubridate::year()

  dataDir <- getRawsDataDir()

  # ----- Check for local data -------------------------------------------------

  # NOTE:  We save the file with the currentYear. This guarantees that updated
  # NOTE:  files will be obtained at least every year.

  fileName = sprintf("fw13_%s_%d.rda", nwsID, currentYear)
  filePath = file.path(dataDir, fileName)

  if ( file.exists(filePath) ) {

    # Anything other than newDownload == TRUE means don't re-download
    if ( is.na(newDownload) || newDownload == FALSE ) {

      if ( verbose ) {
        message(sprintf("Loading data from %s", filePath))
      }

      # If local data exists, load and return it.
      rawsObject <- get(load(filePath))

    } else {

      if ( verbose ) {
        message(paste("Downloading and saving data to", filePath))
      }

      # Download new data to overwrite existing file
      rawsObject <- fw13_createRawsObject(nwsID = nwsID, meta = meta, baseUrl = baseUrl)

      # Save this data and overwrite existing file
      save(rawsObject, file = filePath)

    }

  } else {

    if ( is.na(newDownload) || newDownload == TRUE ) {

      if ( verbose ) {
        message("Could not find local data.")
        message(paste("Downloading and saving data to", filePath))
      }

      # If local data does not exist, download and return it.
      rawsObject <- fw13_createRawsObject(nwsID = nwsID, meta = meta, baseUrl = baseUrl)

      # Save this object so it may be loaded in the future
      save(rawsObject, file = filePath)

    } else {

      stop(sprintf("Skipping nwsID: %s", nwsID))

    }

  }

  # ----- Return ---------------------------------------------------------------

  return(rawsObject)

}
