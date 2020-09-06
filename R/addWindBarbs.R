#' @export
#' @title Add wind barbs to a map
#' @param x vector of longitudes
#' @param y vector of latitudes
#' @param speed vector of wind speeds in knots
#' @param dir wind directions in degrees clockwise from north
#' @param circleSize size of the circle 
#' @param circleFill circle fill color
#' @param lineCol line color (currently not supported)
#' @param extraBarbLength add length to barbs
#' @param barbSize size of the barb 
#' @param ... additional arguments to be passed to \code{lines}
#' @description Add a multi-sided polygon to a plot.
#' @references https://commons.wikimedia.org/wiki/Wind_speed
#' @examples
#' \donttest{
#' library(maps)
#' library(RAWSmet)
#' 
#' maps::map('state', "washington")
#' 
#' januaryData <- example_fw13SaddleMountain %>% raws_filterDate(20170101, 20170101)
#' juneData <- example_fw13SaddleMountain %>% raws_filterDate(20170601, 20170601)
#' 
#' lat <- c(januaryData$meta$latitude, juneData$meta$latitude) 
#' lon <- c(januaryData$meta$longitude, juneData$meta$longitude) 
#' speed <- c(januaryData$data[1,]$windSpeed, juneData$data[1,]$windSpeed)
#' dir <- c(januaryData$data[1,]$windDirection, juneData$data[1,]$windDirection)
#' 
#' addWindBarbs(lon, lat, speed, dir, lwd = 2)
#'}
#'
addWindBarbs <- function(x,
                         y,
                         speed,
                         dir,
                         circleSize = 1,
                         circleFill = 'transparent',
                         lineCol = 1,
                         extraBarbLength = 0,
                         barbSize = 1,
                         ...) {
  
  # Make sure all vector lengths match
  lengths <- c(length(x), 
               length(y), 
               length(speed), 
               length(dir),
               length(circleFill),
               length(circleSize),
               length(lineCol))
  
  vectorLength <- max(lengths)
  
  # TODO:  check to make sure lengths are all multiples
  
  x <- rep_len(x, length.out = vectorLength)
  y <- rep_len(y, length.out = vectorLength)
  speed <- rep_len(speed, length.out = vectorLength)
  dir <- rep_len(dir, length.out = vectorLength)
  circleFill <- rep_len(circleFill, length.out = vectorLength)
  circleSize <- rep_len(circleSize, length.out = vectorLength)
  lineCol <- rep_len(lineCol, length.out = vectorLength)
  
  for (i in 1:vectorLength) {
    addWindBarb(x[i], y[i], speed[i], dir[i],
                circleSize[i], circleFill[i], lineCol[i],
                extraBarbLength, barbSize, ...)
  }
  
}