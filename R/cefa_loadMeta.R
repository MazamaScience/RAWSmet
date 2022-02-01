#' @export
#'
#' @title Load FW13 RAWS station metadata from a local directory
#'
#' @param metadataUrl URL for station metadata.
#' @param newDownload Logical flag stating whether or not to download and override existing data.
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @return Dataframe containing station metadata.
#'
#' @description Loads all FW13 station metadata from the \code{rawsDataDir}. If
#' the data is not in this directory, this will download and save the data.
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
#' cefa_meta <- cefa_loadMeta()
#'
#' dplyr::glimpse(cefa_meta, width = 75)
#'
#' }, silent = FALSE)
#' }
#'
#' @seealso \code{cefa_createMeta}
#' @seealso \code{setRawsDataDir}
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}

cefa_loadMeta <- function(
  metadataUrl = "https://cefa.dri.edu/raws/RAWSfw13list.xlsx",
  newDownload = FALSE,
  verbose = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  dataDir <- getRawsDataDir()

  # ----- Check for local data -------------------------------------------------

  fileName = "cefa_metadata.rda"
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
    metadata <- cefa_createMeta(metadataUrl = metadataUrl, verbose = verbose)

    # Save this object so it may be loaded in the future
    save(metadata, file = filePath)

  }

  # ----- Return ---------------------------------------------------------------

  return(metadata)

}
