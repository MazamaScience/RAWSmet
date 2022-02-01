#' @export
#' @importFrom rlang .data
#' @importFrom dplyr filter
#'
#' @title Load multiple FW13 RAWS timeseries objects from a local directory
#'
#' @param nwsIDs Vector of NWS RAWS station identifiers.
#' @param meta Tibble of FW13 station metadata.
#' @param newDownload Logical flag stating whether or not to download and override existing data.
#' @param baseUrl Base URL for data queries.
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @return List of timeseries objects each containing 'meta' and 'data'.
#'
#' @description Loads FW13 station metadata and data from the \code{rawsDataDir}. If the
#' data is not in this directory, this will download and save the data.
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
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("NaturalEarthAdm1")
#'
#' library(RAWSmet)
#' setRawsDataDir("~/Data/RAWS/")
#'
#' stationMeta <- cefa_loadMeta()
#'
#' nwsIDs <- c("021503", "500726", "020401")
#' stationData <- cefa_loadMultiple(nwsIDs = nwsIDs, meta = stationMeta)
#'
#' dplyr::glimpse(stationData)
#'
#' }, silent = FALSE)
#' }
#'
#' @seealso \code{cefa_createRawsObject}
#' @seealso \code{cefa_load}
#' @seealso \code{setRawsDataDir}
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}

cefa_loadMultiple <- function(
  nwsIDs = NULL,
  meta = NULL,
  newDownload = NA,
  baseUrl = "https://cefa.dri.edu/raws/fw13/",
  verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(nwsIDs)

  dataDir <- getRawsDataDir()

  # ----- Load data for each station -------------------------------------------

  stationList <- list()
  counter <- 1

  for ( nwsID in nwsIDs ) {

    if ( verbose )
      message(sprintf("Loading data for %s (%d/%d)", nwsID, counter, length(nwsIDs)))

    res <- try({

      # Load or download data for each station
      stationTimeseriesObject <-
        cefa_load(
          nwsID = nwsID,
          meta = meta,
          newDownload = newDownload,
          baseUrl = baseUrl,
          verbose = verbose)

      # Add this station's raws_timeseries object to the list
      stationList[[nwsID]] <- stationTimeseriesObject

    }, silent = verbose)

    counter <- counter + 1

  }

  class(stationList) <- c("rawsList", class(stationList))

  # ----- Return ---------------------------------------------------------------

  return(stationList)

}
