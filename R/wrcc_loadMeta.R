#' @export
#'
#' @title Load WRCC RAWS station metadata from a local directory
#'
#' @param stateCode Two character state code (will be downcased).
#' @param newDownload Logical flag stating whether or not to download and override existing data.
#' @param baseUrl Base URL for data queries.
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @return Dataframe containing station metadata.
#'
#' @description Loads WRCC station metadata for a given state from the
#' \code{rawsDataDir}. If the data is not in this directory, this will download
#' and save the data.
#'
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' # For creation of metadata
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("NaturalEarthAdm1.rda")
#'
#' library(RAWSmet)
#' setRawsDataDir("~/Data/RAWS")
#'
#' wa_meta <- wrcc_loadMeta(stateCode = "WA")
#'
#' dplyr::glimpse(wa_meta, width = 80)
#'
#' }, silent = FALSE)
#' }
#'
#' @seealso \code{wrcc_createMeta}
#' @seealso \code{setRawsDataDir}
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}

wrcc_loadMeta <- function(
  stateCode = NULL,
  newDownload = FALSE,
  baseUrl = "https://raws.dri.edu/",
  verbose = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  MazamaCoreUtils::stopIfNull(stateCode)

  dataDir <- getRawsDataDir()

  # ----- Check for local data -------------------------------------------------

  fileName = sprintf("wrcc_meta_%s.rda", stateCode)
  filePath = file.path(dataDir, fileName)

  if ( file.exists(filePath) && newDownload == FALSE ) {

    if( verbose ) {
      message(sprintf("Loading data from %s", filePath))
    }

    # If local data exists, load and return it.
    metadata <- get(load(filePath))

  } else {

    if ( verbose ) {
      if ( !newDownload )
        message("Could not find local data.")
      message(paste("Downloading and saving data to", filePath))
    }

    # If local data does not exist, download and return it.
    metadata <- wrcc_createMeta(stateCode = stateCode, baseUrl = baseUrl, verbose = verbose)

    # Save this object so it may be loaded in the future
    save(metadata, file = filePath)

  }

  # ----- Return ---------------------------------------------------------------

  return(metadata)

}
