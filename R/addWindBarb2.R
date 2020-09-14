#' @keywords internal
#' @import graphics
#' @title Add wind barb to a timeseries plot
#' @param x x-axis position
#' @param y y-axis position
#' @param speed wind speed in knots 
#' @param dir wind direction in degrees clockwise from north
#' @param extraBarbLength add length to barbs
#' @param barbSize size of the barb 
#' @param barbColor color of the barbs
#' @param barbLocation starting location for the barb. Currently accepts 'point' or 'zero'
#' @param ... additional arguments to be passed to \code{lines}
#' @description Add a wind barb to the plot. Used internally in \link{addWindBarbs2}
#' @references \url{https://commons.wikimedia.org/wiki/Wind_speed}

addWindBarb2 <- function(
  x, 
  y,
  speed,
  dir,
  extraBarbLength = 0,
  barbSize = 1,
  barbColor = "blue",
  barbLocation = "zero",
  ...
) {
  
  # Wind direction is measured in degrees clockwise from north
  # We want to convert into counter-clockwise from east
  dir <- (360 - dir + 90) %% 360
  
  # Get dir in radians
  
  rad <- dir * pi / 180
  
  # Get x and y scale factors
  pin <- par("pin")
  usr <- par("usr")
  xpi <- (usr[2] - usr[1]) / pin[1]
  ypi <- (usr[4] - usr[3]) / pin[2]
  
  if ( speed > 0 ) {
    # The baseline barb length will be 1/4 inch
    lx <- xpi / 4 * barbSize
    ly <- ypi / 4 * barbSize
    if (speed < 5) {
      # under 5 knots, barb length is shorter with lower speeds
      lx <- lx/5*speed
      ly <- ly/5*speed
    }
    
    # Convert POSIXct x axis to numeric
    x <- as.numeric(x)
    
    # If the barb starting location is zero, set y to 0
    # If not y will be left as is
    if ( barbLocation == "zero" ) {
      y <- 0
    }
    
    # Get ending points for barb
    xe <- x + (lx + extraBarbLength*lx) * cos(rad)
    ye <- y + (ly + extraBarbLength*ly) * sin(rad)
    
    # Convert numeric end position back to POSIXct
    xe <- as.POSIXct.numeric(xe, origin = "1970-01-01")

    # Draw arrows
    arrows(x0 = x, y0 = y, x1 = xe, y1 = ye, col = barbColor, ...)
  }
  

}