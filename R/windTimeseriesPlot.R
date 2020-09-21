#' @export
#' @importFrom MazamaCoreUtils parseDatetime
#' @importFrom ggplot2 ggplot geom_point geom_line ylim labs scale_color_manual theme guide_legend element_text aes
#' @importFrom metR geom_arrow
#' @importFrom grid arrow unit
#' @importFrom rlang .data
#' 
#' @title Create a visualization of wind and gust speeds for a given station
#'
#' @param rawsObject \emph{raws_timeseries} object for which to create the plot for
#' @param startDate Desired start datetime (ISO 8601).
#' @param endDate Desired end datetime (ISO 8601).
#'
#' @return ggplot2 plot of wind and gust speeds
#'
#' @description Creates a visualization of wind and gust speeds and wind directions.
#'
#' @examples
#' \donttest{
#' library(RAWSmet)
#' library(grid)
#' library(ggplot2)
#' 
#' plot <- windTimeseriesPlot(example_fw13SaddleMountain, 20170801, 20170802)
#' plot
#' }
#'
windTimeseriesPlot <- function(
  rawsObject = NULL,
  startDate = NULL,
  endDate = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( ! raws_isRaws(rawsObject) )
    stop("Parameter 'rawsObject' is not a valid raws_timeseries object.")
  
  if ( raws_isEmpty(rawsObject) )
    stop("Parameter 'rawsObject' must not be empty")
  
  if ( is.null(startDate) )
    startDate <- min(rawsObject$data$datetime)
  
  if ( is.null(endDate) )
    endDate <- max(rawsObject$data$datetime)
  
  # ----- Extract data and metadata --------------------------------------------
  
  meta <- 
    raws_extractMeta(rawsObject)
  
  data <- 
    raws_filterDate(rawsObject, startDate, endDate, timezone = meta$timezone) %>% 
    raws_extractData()
  
  # ----- Create plot ----------------------------------------------------------
  
  # Wind direction is measured in degrees clockwise from north
  # We want to convert into counter-clockwise from east
  data$windDirection <- (360 - data$windDirection + 90) %% 360
  
  if ( is.na(meta$wrccID) )
    stationID <- meta$nwsID
  else
    stationID <- meta$wrccID
  
  title <- sprintf("Observed Wind at %s, %s %s \n From %s LST to %s LST", 
                  stationID, 
                  meta$siteName, 
                  meta$stateCode, 
                  MazamaCoreUtils::parseDatetime(startDate, timezone = meta$timezone), 
                  MazamaCoreUtils::parseDatetime(endDate, timezone = meta$timezone))
  
  gg <- ggplot2::ggplot(data, ggplot2::aes(x = .data$datetime, y = .data$windSpeed)) +
    ggplot2::geom_point(ggplot2::aes(y = .data$windSpeed, color = "Winds")) +
    ggplot2::geom_line(ggplot2::aes(y = .data$maxGustSpeed, color = "Gusts")) +
    metR::geom_arrow(ggplot2::aes(mag = 1, angle =.data$windDirection), arrow.ends = "last", show.legend = F) +
    ggplot2::ylim(min = -1, max = max(data$maxGustSpeed)) +
    ggplot2::labs(title = title, x = "", y = "Speed in m/s", color = "") +
    ggplot2::scale_color_manual(values = c("blue", "red"),
                       breaks = c("Winds", "Gusts"),
                       guide = ggplot2::guide_legend(override.aes = list(
                         linetype = c("blank", "solid"),
                         shape = c(16, NA)))) +
    ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(hjust = 0.5))
  
  return(gg)
  
}
