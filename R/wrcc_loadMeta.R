#' @export
#' 
#' @title Load WRCC RAWS station metadata from a local directory
#'
#' @param stateCode Two character state code (will be downcased).
#' @param forceDownload Logical flag stating whether or not to download and override existing data.
#' @param baseUrl Base URL for data queries.
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @return Dataframe containing station metadata.
#'
#' @description Loads WRCC station metadata for a given state from the \code{rawsDataDir}. If the
#' data is not in this directory, this will download and save the data. 
#' 
#' @examples
#' \dontrun{
#' library(RAWSmet)
#' 
#' setRawsDataDir("~/Data/RAWS/")
#'
#' stationMeta <- wrcc_loadMeta(stateCode = "WA")
#' 
#' dplyr::glimpse(stationMeta)
#' }
#'
#' @seealso \code{wrcc_createMetadata}
#' @seealso \code{setRawsDataDir}
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}

wrcc_loadMeta <- function(
  stateCode = NULL,
  forceDownload = FALSE,
  baseUrl = "https://raws.dri.edu/",
  verbose = TRUE
) {
  
  # ----- Setup ----------------------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(stateCode)
  
  dataDir <- getRawsDataDir()
  
  # ----- Check for local data -------------------------------------------------
  
  fileName = sprintf("wrccMetadata_%s.rda", stateCode)
  filePath = file.path(dataDir, fileName)
  
  if ( file.exists(filePath) && forceDownload == FALSE ) {
    
    if( verbose ) {
      message(sprintf("Loading data from %s", filePath))
    }
    
    # If local data exists, load and return it.
    metadata <- get(load(filePath))
    
  } else {
    
    if ( verbose ) {
      if ( !forceDownload )
        message("Could not find local data.")
      message(paste("Downloading and saving data to", filePath, "."))
    }
    
    # If local data does not exist, download and return it.
    metadata <- wrcc_createMetadata(stateCode = stateCode, baseUrl = baseUrl, verbose = verbose)
    
    # Save this object so it may be loaded in the future
    save(metadata, file = filePath)
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(metadata)
  
}
