#' @export
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes_ geom_point geom_line geom_area labs facet_wrap
#' @importFrom ggplot2 ylim
#' @importFrom tidyr gather
#'
#' @title Faceted plot of a \emph{raws_timeseries} tibble
#'
#' @param data \code{data} dataframe of a \emph{raws_timeseries} object.
#' @param pattern Pattern used to match groups of parameters.
#' @param parameters Custom vector of parameters to view.
#' @param nrow Number of rows in the faceted plot.
#' @param ncol Number of columns in the faceted plot.
#' @param autoRange Logical specifying whether to scale the y axis separately
#' for each plot or to use a common y axis.
#' @param ylim Vector of (lo,hi) y-axis limits.
#' @param style Style of plot: ("point", "line", "area")
#'
#' @description A plotting function that uses ggplot2 to display a suite of
#' \emph{raws_timeseries} plots all at once.
#'
#' @note Specification of \code{ylim} will override the choice of
#' \code{autoRange}.
#'
#' @examples
#' library(RAWSmet)
#'
#' data <-
#'   example_cefa_Saddle_Mountain %>%
#'   raws_filterDate(20170801, 20170901) %>%
#'   raws_getData()
#'
#' timeseriesMultiplot(
#'   data,
#'   pattern = c("humidity|temperature"),
#'   nrow = 2
#' )
#'
timeseriesMultiplot <- function(
  data = NULL,
  pattern = NULL,
  parameters = NULL,
  nrow = NULL,
  ncol = NULL,
  autoRange = TRUE,
  ylim = NULL,
  style = "line"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(data)

  # Check for existence of "datetime"
  if ( !"datetime" %in% names(data) || (!"POSIXct" %in% class(data$datetime)) )
    stop("Parameter 'data' must have a column named 'datetime' of class 'POSIXct'.")

  # Check for existence of parameters
  if ( !is.null(parameters) ) {
    unknownParameters <- setdiff(parameters, names(data))
    if ( length(unknownParameters) > 0 ) {
      parameterString <- paste0(unknownParameters, collapse = ", ")
      err_msg <- paste0("Parameters not found in data:\n\t",
                        parameterString)
      stop(err_msg)
    }
  }

  # Remove columns with all NA values as they mess up the creation of axes
  containsDataMask <- lapply(data, function(x) { any(!is.na(x)) }) %>% unlist()
  noDataNames <- names(containsDataMask[!containsDataMask])
  if ( length(noDataNames) > 0 ) {
    msg <- sprintf("Dropping columns with all missing data: %s",
                   paste0(noDataNames, collapse = ", "))
    message(msg)
    data <- data[,containsDataMask]
  }

  if ( is.null(nrow) && is.null(ncol) )
    ncol <- 1

  # ----- Determine parameters to plot -----------------------------------------

  if ( is.null(parameters) ) {

    parameters <- sort(names(data))

    # Subset if requested
    if ( !is.null(pattern) ) {
      parameters <- stringr::str_subset(parameters, pattern)
    }

  }

  # Otherwise just use the incoming parameters

  # Make sure 'datetime' is included, but only once
  parameters <- unique(c("datetime", parameters))

  tidyData <-
    data[,parameters] %>%
    tidyr::gather("parameter", "value", -.data$datetime)

  # ----- Create plot ----------------------------------------------------------

  # Y axis
  if ( autoRange && is.null(ylim) ) {
    scales <- "free_y"
  } else {
    scales <- "fixed"
  }

  # Facets
  facets <- factor(tidyData$parameter, levels = parameters)

  # NOTE:  Using ggplot in a package requires special attention:
  # NOTE:    https://ggplot2.tidyverse.org/reference/aes_.html
  # NOTE:    https://bookdown.org/rdpeng/RProgDA/non-standard-evaluation.html

  gg <-
    ggplot(tidyData, aes_(x = ~datetime, y = ~value))

  if ( style == "point" ) {
    gg <- gg + geom_point()
  } else if ( style == "line" ) {
    gg <- gg + geom_line()
  } else {
    gg <- gg + geom_area()
  }

  gg <- gg +
    facet_wrap(facets, nrow = nrow, ncol = ncol, scales = scales )

  if ( !is.null(ylim) ) {
    gg <- gg + ylim(ylim)
  }

  # ----- Return ---------------------------------------------------------------

  return(gg)

}
