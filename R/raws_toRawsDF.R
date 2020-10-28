#' @export
#' @importFrom utils object.size
#' 
#' @title Convert a raws_timeseries object to a rawsDF object
#'
#' @param rawsObject \emph{raws_timeseries} object to convert.
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @return rawsDF object
#'
#' @description Converts a \emph{raws_timeseries} object to a single dataframe
#' containing the following values:
#' 
#' \enumerate{
#'  \item{nwsID - the nwsID of the station}
#'  \item{wrccID - the wrccID of the station}
#'  \item{siteName - the name of the station}
#'  \item{longitude - longitude coordinate of the station}
#'  \item{latitude - latitude coordinate of the station}
#'  \item{datetime - the datestamp of the observation}
#'  \item{parameter - the name of the parameter this record represents}
#'  \item{value - the value of the parameter this record represents}
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
#' rawsDF <- raws_toRawsDF(rawsObject)
#' 
#' dplyr::glimpse(rawsDF)
#' }
#'
#' @seealso \code{wrcc_loadYear}
#' @seealso \code{fw13_load}
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}

raws_toRawsDF <- function(
  rawsObject = NULL,
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !raws_isRaws(rawsObject) )
    stop("Parameter 'rawsObject' is not a valid raws_timeseries object.")
  
  if ( raws_isEmpty(rawsObject) )
    stop("Parameter 'rawsObject' is empty.")
  
  # ----- Check if converted object will be too large --------------------------
  
  rawsObject_size <- object.size(rawsObject)
  
  if ( rawsObject_size * 5 > 1e+8 )
    stop("Resulting rawsDF will be too large (> 100MB)")
  
  # ----- Convert to rawsDF ----------------------------------------------------
  
  rawsDF <- data.frame(
    "nwsID" = character(),
    "wrccID" = character(),
    "siteName" = character(),
    "longitude" = numeric(),
    "latitude" = numeric(),
    "datetime" = as.Date(character()),
    "parameter" = character(),
    "value" = numeric()
  )
  
  # Iterate through each parameter besides the first (datetime) and the last (monitorType)
  for ( parameter in names(rawsObject$data[3:length(rawsObject$data)-1]) ) {
    
    newDF <- data.frame(
      "nwsID" = rawsObject$meta$nwsID,
      "wrccID" = rawsObject$meta$wrccID,
      "siteName" = rawsObject$meta$siteName,
      "longitude" = rawsObject$meta$longitude,
      "latitude" = rawsObject$meta$latitude,
      "datetime" = rawsObject$data$datetime,
      "parameter" = parameter,
      "value" = rawsObject$data[[parameter]]
    ) 
    
    rawsDF <- rbind(rawsDF, newDF)
    
  }
  
  
  return(rawsObject)
  
}

