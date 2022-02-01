#' @export
#' @importFrom rlang .data
#' @importFrom dplyr filter
#'
#' @title Load multiple WRCC RAWS timeseries objects from a local directory
#'
#' @param wrccIDs Vector of RAWS station identifiers (will be upcased).
#' @param meta Tibble of WRCC station metadata.
#' @param year Year to access station data for.
#' @param newDownload Logical flag stating whether or not to download and override existing data.
#' @param password Password required for access to archival data.
#' @param baseUrl Base URL for data queries.
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @return List of timeseries objects each containing 'meta' and 'data'.
#'
#' @description Loads WRCC station metadata and data from the \code{rawsDataDir}. If the
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
#'
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("NaturalEarthAdm1")
#'
#' library(RAWSmet)
#' setRawsDataDir("~/Data/RAWS")
#'
#' wa_meta <- wrcc_loadMeta(stateCode = "WA")
#'
#' wrccIDs <- c("waWENU", "waWASH", "waWALD")
#' stationData <-
#'   wrcc_loadMultiple(
#'     wrccIDs = wrccIDs,
#'     meta = wa_meta,
#'     year = 2020,
#'     newDownload = NA,
#'     password = MY_PASSWORD
#'   )
#'
#' dplyr::glimpse(stationData)
#'
#' }, silent = FALSE)
#' }
#'
#' @seealso \code{wrcc_createRawsObject}
#' @seealso \code{wrcc_load}
#' @seealso \code{setRawsDataDir}
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}

wrcc_loadMultiple <- function(
  wrccIDs = NULL,
  meta = NULL,
  year = NULL,
  newDownload = NA,
  password = NULL,
  baseUrl = "https://wrcc.dri.edu/cgi-bin/wea_list2.pl",
  verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(wrccIDs)
  MazamaCoreUtils::stopIfNull(year)

  if ( !is.numeric(year) ) {
    stop("Parameter 'year' must be numeric.")
  }

  dataDir <- getRawsDataDir()

  # ----- Load data for each station -------------------------------------------

  stationList <- list()
  counter <- 1

  for ( wrccID in wrccIDs ) {

    if ( verbose )
      message(sprintf("Loading data for %s (%d/%d)", wrccID, counter, length(wrccIDs)))

    result <- try({

      # Load or download data for each station
      stationTimeseriesObject <-
        wrcc_loadYear(
          wrccID = wrccID,
          meta = meta,
          year = year,
          newDownload = newDownload,
          password = password,
          baseUrl = baseUrl,
          verbose = verbose
        )

    }, silent = verbose)

    if ( "try-error" %in% class(result) ) {

      err_msg <- geterrmessage()
      # TODO:  Trap and potentially rephrase error messages
      message(sprintf(
        "Skipping %s: %s",
        wrccID,
        err_msg
      ))

    } else {

      # Add this station's raws_timeseries object to the list
      stationList[[wrccID]] <- stationTimeseriesObject

    }

    counter <- counter + 1

  }

  class(stationList) <- c("rawsList", class(stationList))

  # ----- Return ---------------------------------------------------------------

  return(stationList)

}
