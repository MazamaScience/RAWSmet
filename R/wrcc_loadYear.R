#' @export
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' 
#' @title Load WRCC RAWS timeseries object from a local directory
#'
#' @param wrccID RAWS station identifier (will be upcased)
#' @param meta Tibble of WRCC station metadata.
#' @param year Year to access station data for.
#' @param newDownload Logical flag specifying whether or not to download and override existing data.
#' @param password Password required for access to archival data.
#' @param baseUrl Base URL for data queries.
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @return Timeseries object with 'meta' and 'data'.
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
#' 
#' @examples
#' \dontrun{
#' library(RAWSmet)
#' 
#' setRawsDataDir("~/Data/RAWS/")
#'
#' stationMeta <- wrcc_loadMeta(stateCode = "WA")
#' rawsObject <- wrcc_loadYear(
#'   wrccID = "waWENU", 
#'   meta = stationMeta, 
#'   year = 2020,
#'   password = MY_PASSWORD
#' )
#' 
#' dplyr::glimpse(rawsObject)
#' }
#'
#' @seealso \code{wrcc_createTimeseriesObject}
#' @seealso \code{setRawsDataDir}
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}

wrcc_loadYear <- function(
  wrccID = NULL,
  meta = NULL,
  year = NULL,
  newDownload = NA,
  password = NULL,
  baseUrl = "https://wrcc.dri.edu/cgi-bin/wea_list2.pl",
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(wrccID)
  MazamaCoreUtils::stopIfNull(year)
  
  if ( !is.numeric(year) ) {
    stop("Parameter 'year' must be numeric.")
  }
  
  dataDir <- getRawsDataDir()
  
  # ----- Check for local data -------------------------------------------------
  
  fileName = sprintf("wrcc_%s_%d.rda", wrccID, year)
  filePath = file.path(dataDir, fileName)
  
  if ( file.exists(filePath) ) {
    
    # Anything other than newDownload == TRUE means don't re-download
    if ( is.na(newDownload) || newDownload == FALSE ) {
      
      if ( verbose ) {
        message(sprintf("Loading data from %s", filePath))
      }
      
      # If local data exists, load and return it.
      rawsObject <- get(load(filePath))
      
    }
    
  } else {
    
    # Anything other than newDownload == FALSE means download new data
    if ( is.na(newDownload) || newDownload == TRUE ) {
      
      if ( verbose ) {
        if ( !newDownload )
          message("Could not find local data.")
        message(paste("Downloading and saving data to", filePath))
      }
      
      # NOTE:  Extend start and end UTC dates by one day to capture full days in
      # NOTE:  every timezone.
      
      # NOTE:  Surprising behavior from lubridate at the year boundary:
      # NOTE:  
      # NOTE:  > x <- ymd_hms("2020-01-01 00:00:00", tz = "UTC")
      # NOTE:  > y <- ymd_hms("2020-01-01 00:00:01", tz = "UTC")
      # NOTE:  > lubridate::ceiling_date(x, "year")
      # NOTE:  [1] "2020-01-01 UTC"
      # NOTE:  > lubridate::ceiling_date(y, "year")
      # NOTE:  [1] "2021-01-01 UTC"
      
      startdate <- 
        lubridate::ymd(paste0(year, "0601"), tz = "UTC") %>% 
        lubridate::floor_date(unit = "year") - lubridate::ddays(1)
      
      enddate <- 
        lubridate::ymd(paste0(year, "0601"), tz = "UTC") %>% 
        lubridate::ceiling_date(unit = "year") + lubridate::ddays(1)
      
      # If local data does not exist, download and return it.
      rawsObject <- wrcc_createTimeseriesObject(
        wrccID = wrccID, 
        meta = meta, 
        startdate = strftime(startdate, "%Y%m%d%H", tz = "UTC"),
        enddate = strftime(enddate, "%Y%m%d%H", tz = "UTC"),
        password = password,
        baseUrl = baseUrl
      )
      
      # NOTE:  Do not trim data to the year. Leave an extra day on each end.

      # Save this object so it may be loaded in the future
      save(rawsObject, file = filePath)
      
    }
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(rawsObject)
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  library(RAWSmet)
  
  setRawsDataDir("~/Data/RAWS/")
  
  meta <- wrcc_loadMeta(stateCode = "OR")
  
  wrccID = "orOWAG"
  ###meta = NULL
  year = 2020
  newDownload = TRUE
  password = NULL
  baseUrl = "https://wrcc.dri.edu/cgi-bin/wea_list2.pl"
  verbose = TRUE
  
  
  raws <- wrcc_loadYear(
    wrccID = wrccID,
    meta = meta,
    year = year,
    newDownload = newDownload,
    password = MY_PASSWORD,
    baseUrl = "https://wrcc.dri.edu/cgi-bin/wea_list2.pl",
    verbose = TRUE
  )
  
}
