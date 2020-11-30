#' @export
#' @importFrom utils object.size
#' @importFrom rlang .data
#' 
#' @title Convert a raws_timeseries object to a rawsDF object
#'
#' @param rawsObject \emph{raws_timeseries} object to convert.
#' @param sizeMax Maximum allowable size (in MB) for the resulting dataframe.
#'
#' @return Tidy dataframe containing data and metadata.
#'
#' @description Converts a \emph{raws_timeseries} object to a single, tidy
#' dataframe containing all varaibles from \code{rawsObject$data} along with the 
#' following values from \code{rawsObject$meta}:
#' 
#' \enumerate{
#'  \item{nwsID - the nwsID of the station}
#'  \item{wrccID - the wrccID of the station}
#'  \item{siteName - the name of the station}
#'  \item{longitude - longitude coordinate of the station}
#'  \item{latitude - latitude coordinate of the station}
#'  \item{elevation - elevation of the station}
#'  \item{timezone - timezone of the station}
#' }
#' 
#' This version of the RAWS data is known as a \emph{rawsDF} object.
#' 
#' Replicating this set of variables for every record greatly inflates the size
#' of the data but also makes it much more useful when working with the 
#' \pkg{dplyr} and \code{ggplot2} packages.
#' 
#' Multiple \emph{rawsDF} objects can be combined with \code{dplyr::bind_rows()} 
#' and used to create multi-station plots.
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
#' rawsDF <- raws_toRawsDF(rawsObject)
#' 
#' dplyr::glimpse(rawsDF)
#' }
#'
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}

raws_toRawsDF <- function(
  rawsObject = NULL,
  sizeMax = 100
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(rawsObject)
  MazamaCoreUtils::stopIfNull(sizeMax)
  
  if ( !raws_isRaws(rawsObject) )
    stop("Parameter 'rawsObject' is not a valid raws_timeseries object.")
  
  if ( raws_isEmpty(rawsObject) )
    stop("Parameter 'rawsObject' is empty.")
  
  if ( !is.numeric(sizeMax) ) 
    stop("Parameter 'sizeMax' must be a numeric value.")
  
  # ----- Calculate size of added metadata -------------------------------------

  # NOTE:  Use only metadata useful in plotting to avoid unnecessary bloat
  
  # > names(rawsObject$meta)
  # [1] "nwsID"       "wrccID"      "nessID"      "siteName"    "longitude"  
  # [6] "latitude"    "timezone"    "elevation"   "countryCode" "stateCode"  
  # [11] "agency"  
  
  metaColumns <- c(
    "nwsID",
    "wrccID",
    ###"nessID",
    "siteName",
    "longitude",
    "latitude",
    "timezone",
    "elevation"
    ###"countryCode",
    ###"stateCode",
    ###"agency"
  )
  
  meta <- rawsObject$meta[1, metaColumns]

  tidySize <- 
    utils::object.size(meta) * nrow(rawsObject$data) +
    utils::object.size(rawsObject$data)
  
  if ( as.numeric(tidySize) > (sizeMax * 1e6) ) {
    stop(sprintf(
      "Resulting rawsDF will be %.1f MB.  Adjust 'sizeMax' or consider filtering by datetime.",
      (tidySize/1e6)
    ))
  }
  
  # ----- Create rawsDF --------------------------------------------------------
  
  rawsDF <-
    rawsObject$data %>%
    dplyr::mutate(
      nwsID = meta$nwsID,
      wrccID = meta$wrccID,
      ###nessID = meta$nessID,
      siteName = meta$siteName,
      longitude = meta$longitude,
      latitude = meta$latitude,
      timezone = meta$timezone,
      elevation = meta$elevation
      ###countryCode = meta$countryCode,
      ###stateCode = meta$stateCode,
      ###agency = meta$agency
    )
  
  # ----- Return ---------------------------------------------------------------
  
  return(rawsDF)
  
}

