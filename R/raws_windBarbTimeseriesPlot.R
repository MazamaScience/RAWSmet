#' @export
#' 
#' @title Create a visualization of wind speed and direction for RAWS data.
#'
#' @param rawsObject \emph{raws_timeseries} object for which to create the plot for
#' @param extraBarbLength add length to barbss
#' @param barbSize size of the barbs
#' @param barbColor color of the barbs
#' @param pointColor color of the plotted points
#' @param barbLocation starting location for barbs. Currently accepts 'point' or 'zero'
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param xlim vector containing minimum and maximum x-axis values
#' @param ylim vector containing minimum and maximum y-axis values
#' @param ... additional arguments to be passed to \code{addWindBarbs2}
#'
#' @return plot of wind speed and direction
#'
#' @description Creates a visualization of wind speed and direction
#'
#' @examples
#' \donttest{
#' library(RAWSmet)
#' 
#' rawsObject <- 
#'  example_cefa_Saddle_Mountain %>%
#'  raws_filterDate("20170901", "20170902")
#'   
#' raws_windBarbTimeseriesPlot(rawsObject, barbSize = 5, barbLocation = "point", barbColor = "blue")
#' }
#'
raws_windBarbTimeseriesPlot <- function(
  rawsObject = NULL,
  extraBarbLength = 0,
  barbSize = 1,
  barbColor = "blue",
  pointColor = "black",
  barbLocation = "zero",
  xlab = "Time",
  ylab = "Wind Speed",
  xlim = NA,
  ylim = NA,
  ...
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( ! raws_isRaws(rawsObject) )
    stop("Parameter 'rawsObject' is not a valid raws_timeseries object.")
  
  if ( raws_isEmpty(rawsObject) )
    stop("Parameter 'rawsObject' must not be empty")
  
  # ----- Extract data and metadata --------------------------------------------

  meta <- 
    raws_getMeta(rawsObject)
  
  data <- 
    raws_getData(rawsObject)
  
  
  # ----- Calculate plot axis limits -------------------------------------------

  if ( is.na(xlim) ) {
    xmin = as.numeric(min(data$datetime)) - (barbSize + extraBarbLength) * 60 * 30
    xmax = as.numeric(max(data$datetime)) + (barbSize + extraBarbLength) * 60 * 30
    
    xlim = c(as.POSIXct.numeric(xmin, origin = "1970-01-01"), as.POSIXct.numeric(xmax, origin = "1970-01-01"))
  }

    if ( is.na(ylim) ) {
    ymin = min(data$windSpeed) -  max(data$windSpeed) / 2
    ymax = max(data$windSpeed) +  max(data$windSpeed) / 2
    
    ylim = c(ymin, ymax)
  }
  
  
  # ----- Create plot ----------------------------------------------------------
  
  if ( is.na(meta$wrccID) )
    stationID <- meta$nwsID
  else
    stationID <- meta$wrccID
  
  title <- sprintf("Observed Wind at %s, %s %s \n From %s LST to %s LST", 
                   stationID, 
                   meta$locationName, 
                   meta$stateCode, 
                   min(data$datetime), 
                   max(data$datetime))

  plot(x = data$datetime, 
       y = data$windSpeed, 
       col = pointColor, 
       xlab = xlab, 
       ylab = ylab, 
       xlim = xlim, 
       ylim = ylim, 
       main = title)
  
  addWindBarbs2(x = data$datetime,
                y = data$windSpeed,
                speed = data$windSpeed,
                dir = data$windDirection,
                extraBarbLength = extraBarbLength,
                barbSize = barbSize,
                barbColor = barbColor,
                barbLocation = barbLocation,
                ...)
}
