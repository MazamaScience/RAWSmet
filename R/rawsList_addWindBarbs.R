#' @export
#'
#' @importFrom rlang .data
#'
#' @title Add wind barbs to a map for multiple RAWS stations

#' @param rawsList a list of raws_timeseries objects
#' @param observationTime Time of observation in question. Only accepts POSIXct objects.
#' @param skipMissing Logical flag to skip stations with data missing at the requested time.
#' @param type Which type of wind to plot. Currently only accepts 'winds' or 'gusts'
#' @param circleSize size of the circle
#' @param circleFill circle fill color
#' @param lineCol line color (currently not supported)
#' @param extraBarbLength add length to barbs
#' @param barbSize size of the barb
#' @param ... additional arguments to be passed to \code{lines}

#' @description Add a multi-sided polygon to a plot.

#' @references https://commons.wikimedia.org/wiki/Wind_speed

#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(maps)
#' library(RAWSmet)
#'
#' setRawsDataDir("~/Data/RAWS")
#'
#' maps::map('state', "washington")
#'
#' # Noon on 09-01-2017
#' observationTime <- MazamaCoreUtils::parseDatetime(2017090112, timezone = "America/Los_Angeles")
#'
#' rawsList_addWindBarbs(rawsList = example_cefaList, observationTime = observationTime)
#'
#' }, silent = FALSE)
#' }
#'
rawsList_addWindBarbs <- function(
  rawsList = NULL,
  observationTime = NULL,
  skipMissing = FALSE,
  type = "winds",
  circleSize = 1,
  circleFill = 'transparent',
  lineCol = 1,
  extraBarbLength = 0,
  barbSize = 1,
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(rawsList)
  MazamaCoreUtils::stopIfNull(observationTime)

  purrr::map(rawsList, ~ {
    if( !raws_isRaws(.x) )
      stop("One or more elements of 'rawsList' is not a valid raws_timeseries object.")
  })

  if ( !lubridate::is.POSIXct(observationTime) )
    stop("Parameter 'observationTime' must be a valid POSIXct object.")

  # ----- Draw wind barbs ------------------------------------------------------

  for( stationID in names(rawsList)) {

    station <- rawsList[[stationID]]

    # Check if the data contains observations at the requested time
    if( !observationTime %in% station$data$datetime) {
      if ( skipMissing )
        message(sprintf("No observation found for station '%s' at 'observationTime'. Skipping", stationID))
      else
        stop(sprintf("No observation found for station '%s'at 'observationTime'.", deparse(substitute(.x))))
    }

    observationData <- station$data %>% dplyr::filter(.data$datetime == observationTime)

    lon <- station$meta$longitude
    lat <- station$meta$latitude

    # Convert wind speed from m/s to knots
    if ( type == "gusts") {
      speedKnots <- observationData[1,]$maxGustSpeed * 1.944
      windDirection <- observationData[1,]$maxGustDirection
    } else {
      speedKnots <- observationData[1,]$windSpeed * 1.944
      windDirection <- observationData[1,]$windDirection
    }

    addWindBarbs(
      x = lon,
      y = lat,
      speed = speedKnots,
      dir = windDirection,
      circleSize = circleSize,
      circleFill = circleFill,
      lineCol = lineCol,
      extraBarbLength = extraBarbLength,
      barbSize = barbSize
    )
  }

}
