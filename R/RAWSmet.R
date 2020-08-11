#' @docType package
#' @name RAWSmet
#' @title Utilities for working with RAWS data.
#' @description This package provides many functions for downloading, parsing,
#' and working with Remote Automatic Weather Station (RAWS) data.
NULL

# ----- Internal Package State -------------------------------------------------

rawsEnv <- new.env(parent = emptyenv())
rawsEnv$dataDir <- NULL

#' @docType data
#' @keywords environment
#' @name rawsDataDir
#' @title Directory for RAWS data
#' @format Absolute path string.
#' @description This package maintains an internal directory location which users can set
#' using \code{setRawsDataDir()}. All package functions use this directory whenever data
#' are created or loaded.
#' 
#' The default setting when the package is loaded is \code{NULL}.
#' @seealso getRawsDataDir
#' @seealso setRawsDataDir
NULL

#' @keywords environment
#' @export
#' @title Get package data directory
#' @description Returns the package data directory where RAWS data is located.
#' @return Absolute path string.
#' @seealso rawsDataDir
#' @seealso setRawsDataDir
getRawsDataDir <- function() {
  if (is.null(rawsEnv$dataDir) ) {
    stop('No data directory found. Please set a data directory with setRawsDataDir("YOUR_DATA_DIR").',call.=FALSE)
  } else {
    return(rawsEnv$dataDir)    
  }
}

#' @keywords environment
#' @export
#' @title Set package data directory
#' @param dataDir directory where RAWS data are created
#' @description Sets the package data directory where RAWS data is located.
#' If the directory does not exist, it will be created.
#' @return Silently returns previous value of data directory.
#' @seealso rawsDataDir
#' @seealso getRawsDataDir
setRawsDataDir <- function(dataDir) {
  old <- rawsEnv$dataDir
  dataDir <- path.expand(dataDir)
  tryCatch({
    if (!file.exists(dataDir)) dir.create(dataDir)
    rawsEnv$dataDir <- dataDir
  }, warning = function(warn) {
    warning("Invalid path name.")
  }, error   = function(err) {
    stop(paste0("Error in setSpatialDataDir(",dataDir,")."))
  })
  return(invisible(old))
}

#' @keywords environment
#' @keywords internal
#' @export
#' @title Remove package data directory
#' @description Resets the package data dir to NULL. Used for internal testing. 
#' @return Silently returns previous value of data directory.
#' @seealso rawsDataDir
#' @seealso getRawsDataDir
#' @seealso setRawsDataDir
removeRawsDataDir <- function() {
  old <- rawsEnv$dataDir
  rawsEnv$dataDir <- NULL
}

