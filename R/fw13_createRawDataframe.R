#' @export
#' @importFrom rlang .data
#' @import MazamaCoreUtils 
#'
#' @title Obtain RAWS FW13 data and parse into a tibble
#'
#' @param nwsID Station identifier.
#' @param baseUrl Base URL for data queries.
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @return Raw tibble of RAWS data.
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
#' }
#'
#' @examples
#' \dontrun{
#' library(RAWSmet)
#'
#' tbl <- fw13_createRawDataframe(nwsID = 500742)
#' dplyr::glimpse(tbl)
#' }
#'
#' @seealso \code{\link{fw13_downloadData}}
#'
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}

fw13_createRawDataframe <- function(
  nwsID = NULL,
  baseUrl = "https://cefa.dri.edu/raws/fw13/",
  verbose = FALSE
) {
  
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(nwsID)
  
  # Guarantee it is zero padded six characters
  nwsID <- stringr::str_pad(nwsID, 6, pad = "0")
  
  # ----- Download/parse data --------------------------------------------------
  
  # Read in FW13 data
  fileString <- fw13_downloadData(nwsID, baseUrl)
  
  returnEmptyTibble <- FALSE
  
  if ( fileString == "" ) {
    
    if ( MazamaCoreUtils::logger.isInitialized() ) {
      logger.warn("RAWS data service failed for nwsID: '%s'", nwsID)
    }
    
    # NOTE:  HACK solution to return an empty tibble
    returnEmptyTibble <- TRUE
    fileString <- "W13500742200507131500R  72 36261  4   72 48100 34 1   20     213 713200 10 \n"

  }
  
  # Read fwf raw data into a tibble
  
  # Set these manually. The website that stores this information times out often.
  # This information originally came from https://fam.nwcg.gov/fam-web/weatherfirecd/13.htm
  
  widths = c(3, 6, 8, 4, 1, 1, 3, 3, 3, 3, 2, 3, 3, 3, 3, 2, 5, 1, 2, 2, 1, 1 , 1, 4, 3, 3, 1)
  col_types <- "cccc ccnn nnnnn nnnn cnnc ccnn nc" %>% stringr::str_replace_all(" ","")
  col_names <- c(
    "recordType", "nwsID", "observationDate", "observationTime",
    "observationType", "weatherCode", "dryBulbTemp", "atmosMoisture",
    "windDirection", "avWindSpeed", "fuelMoisture", "maxTemp", "minTemp",
    "maxRelHumidity", "minRelHumidity", "precipDuration", "precipAmount",
    "wetFlag", "herbaceousGreenness", "shrubGreenness", "moistureType",
    "measurementType", "seasonCode", "solarRadiation", "maxGustDirection",
    "maxGustSpeed", "snowFlag"
  )
  
  col_positions <- readr::fwf_widths(
    widths = widths,
    col_names = col_names
  )
  
  # Read in raw data
  df <- 
    readr::read_fwf(
      file = fileString, 
      col_positions = col_positions, 
      col_types = col_types,
      progress = verbose
    )
    
  # NOTE:  HACK solution to return an empty tibble
  if ( exists("returnEmptyTibble") && returnEmptyTibble )
    df <- df %>% dplyr::filter(nwsID == "Rumplestiltskin")
  
  # ----- Return ---------------------------------------------------------------
  
  return(df)
  
}
