#' @export
#' @importFrom rlang .data
#' @importFrom dplyr all_of
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Obtain RAWS data and create a timeseries object.
#'
#' @param fw13ID Station identifier found in 'meta'.
#' @param meta Tibble of RAWS metadata containing \code{fw13ID}.
#' @param baseUrl Base URL for data queries.
#'
#' @return Timeseries object with 'meta' and 'data'.
#'
#' @description Obtains station data from a webservice and converts
#' it into a quality controlled, metadata enhanced "raw" tibble
#' ready for use with all \code{raw_~} functions.
#'
#' Steps involved include:
#'
#' \enumerate{
#'  \item{download data text}
#'  \item{parse data text}
#'  \item{standardized names}
#' }
#'
#' @examples
#' \dontrun{
#' library(RAWSmet)
#'
#' meta500726 <- fw13_createTimeseriesObject(fw13ID = '500726')
#' 
#' 
#' }
#'
#' @seealso \code{\link{fw13_createMetadata}}
#' @seealso \code{\link{fw13_createRawDataframe}}
#'
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}

fw13_createTimeseriesObject <- function(
  fw13ID = NULL,
  meta = NULL,
  baseUrl = "https://cefa.dri.edu/raws/fw13/"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  stopIfNull(fw13ID)
  
  suppressWarnings({
    fw13ID <- as.numeric(fw13ID)
  })
  
  if ( is.na(fw13ID) )
    stop("FW13 ID must be numeric or able to be coereced to numeric.")
  
  # ----- Create 'meta' --------------------------------------------------------
  
  if ( is.null(meta) ) {
    
    meta <- fw13_createMetadata(stationIDs = fw13ID)
    
  } else {
    
    meta <- dplyr::filter(meta, fw13ID == !!fw13ID)
    
  }
  
  # ----- Create 'data' --------------------------------------------------------
  
  # * Download/parse -----
  
  tbl <- fw13_createRawDataframe(
    stationID = fw13ID,
    baseUrl = baseUrl
  )
  
  # * Harmonize ----
  
  # Define the set of standard columns that will always be returned
  standardColumns <- c("LST_datestamp", "recordType", "stationID", "observationDate", "observationTime",
                       "observationType", "weatherCode", "dryBulbTemp", "atmosMoisture",
                       "windDirection", "avWindSpeed", "fuelMoisture", "maxTemp", "minTemp",
                       "maxRelHumidity", "minRelHumidity", "percipDuration", "percipAmount",
                       "wetFlag", "herbaceousGreenness", "shrubGreenness", "moistureType",
                       "measurementType", "seasonCode", "soilarRadiation", "maxGustDirection",
                       "maxGustSpeed", "snowFlag")
  
  data <-
    tbl %>%
    dplyr::select(all_of(standardColumns))
    
  
  # * Convert datetime to UTC ----
  
  UTC_offset <-
    MazamaSpatialUtils::SimpleTimezones@data %>%
    dplyr::filter(.data$timezone == meta$timezone) %>%
    dplyr::pull("UTC_offset")
  
  # NOTE:  The 'datetime' column is "local standard time all-year-round" for
  # NOTE:  which no timezone exists. So we have to convert it first to UTC
  # NOTE:  and then shift it by the UTC offset.
  
  UTC_time <-
    MazamaCoreUtils::parseDatetime(data$LST_datestamp, timezone = "UTC") +
    lubridate::dhours(UTC_offset)
  
  data$datetime <- UTC_time
  
  # ----- Return ---------------------------------------------------------------
  
  # Combine meta and data dataframes into a list
  raws <- list(meta = meta, data = data)
  class(raws) <- c("raws_timeseries", class(raws))
  
  return(raws)
  
}
