#' @export
#' @importFrom rlang .data
#' @import MazamaCoreUtils
#'
#' @title Parse RAWS FW13 characterString
#'
#' @param fileString Character string containing RAWS FW13 data.
#'
#' @return Raw tibble of RAWS data.
#'
#' @description
#' Parse raw character data from CEFA into a tibble. The incoming \code{fileString}
#' can be read in directly from CEFA using \code{cefa_downloadData()} or from a
#' local file using \code{readr::read_file()}.
#'
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(RAWSmet)
#'
#' tbl <-
#'   cefa_downloadData(nwsID = 500742) %>%
#'   cefa_parseData()
#'
#' dplyr::glimpse(tbl)
#'
#' }, silent = FALSE)
#' }
#'
#' @seealso \code{\link{cefa_downloadData}}
#'
#' @references \href{https://cefa.dri.edu/raws/}{Program for Climate, Ecosystem and Fire Applications}
#' @references \href{https://fam.nwcg.gov/fam-web/weatherfirecd/13.htm}{FW13 Data Format}

cefa_parseData <- function(
  fileString = NULL
) {


  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(fileString)

  # ----- Parse data -----------------------------------------------------------

  returnEmptyTibble <- FALSE

  if ( fileString == "" ) {

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
  tbl <-
    readr::read_fwf(
      file = fileString,
      col_positions = col_positions,
      col_types = col_types,
      progress = FALSE
    )

  # NOTE:  HACK solution to return an empty tibble
  if ( exists("returnEmptyTibble") && returnEmptyTibble )
    tbl <- tbl %>% dplyr::filter(.data$nwsID == "DONT FIND ME")

  # ----- Return ---------------------------------------------------------------

  return(tbl)

}
