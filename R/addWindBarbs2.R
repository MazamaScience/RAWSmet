#' @export
#' @title Add wind barbs to a timeseries plot
#' @param x vector of x-axis positions
#' @param y vector of y-axis positions
#' @param speed vector of wind speeds in knots
#' @param dir wind directions in degrees clockwise from north
#' @param extraBarbLength add length to barbs
#' @param barbSize size of the barb 
#' @param barbLocation starting location for barbs. Currently accepts 'point' or 'zero'
#' @param ... additional arguments to be passed to \code{lines}
#' @description Add a multi-sided polygon to a plot.
#' @references https://commons.wikimedia.org/wiki/Wind_speed
#' @examples
#' \donttest{
#' library(RAWSmet)
#' 
#' data <- 
#'   example_fw13SaddleMountain %>%
#'   raws_filterDate("2017-09-01", "2017-10-01") %>%
#'   raws_extractData()
#'
#' plot(data$datetime, data$windSpeed)
#' addWindBarbs2(data$datetime, data$windSpeed, data$windSpeed, data$windDirection)
#'}
#'
addWindBarbs2 <- function(x,
                         y,
                         speed,
                         dir,
                         extraBarbLength = 0,
                         barbSize = 1,
                         barbLocation = "zero",
                         ...) {
  
  # Make sure all vector lengths match
  lengths <- c(length(x), 
               length(y), 
               length(speed), 
               length(dir))
  
  vectorLength <- max(lengths)
  
  # TODO:  check to make sure lengths are all multiples
  
  x <- rep_len(x, length.out = vectorLength)
  y <- rep_len(y, length.out = vectorLength)
  speed <- rep_len(speed, length.out = vectorLength)
  dir <- rep_len(dir, length.out = vectorLength)
  
  for (i in 1:vectorLength) {
    addWindBarb2(x[i], y[i], speed[i], dir[i],
                extraBarbLength, barbSize, barbLocation, ...)
  }
  
}