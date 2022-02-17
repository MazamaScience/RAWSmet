

# ===== INTERNAL FUNCTIONS =====================================================

#' @export
#' @importFrom rlang .data
#' @importFrom dplyr across everything
#'
#' @title Create summary time series for an \emph{sts} time series object
#'
#' @param sts \emph{sts} object.
#' @param timezone Olson timezone used to interpret dates.
#' @param unit Unit used to summarize by (\emph{e.g.} "day").
#' @param FUN Function used to summarize time series.
#' @param ... Additional arguments to be passed to \code{FUN}
#' (_e.g._ \code{na.rm = TRUE}).
#' @param minCount Minimum number of valid data records required to calculate
#' summaries. Time periods with fewer valid records will be assigned \code{NA}.
#'
#' @return An \emph{sts} time series object containing daily (or other)
#' statistical summaries.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @description
#' Columns of numeric data in \code{sts$data} are grouped by \code{unit} and then
#' summarized using \code{FUN}.
#'
#' The most typical use case is creating daily averages where each day begins at
#' midnight. This function interprets times using the \code{sts$data$datetime}
#' \code{tzone} attribute so be sure that is set properly.
#'
#' Day boundaries are calculated using the specified \code{timezone} or, if
#' \code{NULL}, the time zone found in
#' \code{sts$meta$timezone[1]}. Leaving \code{timezone = NULL}, the default,
#' results in "local time" date filtering which is the most common use case.
#'
#' @note
#' Because the returned \emph{sts} object is defined on a daily axis in a
#' specific time zone, it is important that the incoming \code{sts} contain
#' timeseries associated with a single time zone.
#'

sts_summarize <- function(
  sts,
  timezone = NULL,
  unit = c("day", "week", "month", "year"),
  FUN = NULL,
  ...,
  minCount = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(sts)
  unit <- match.arg(unit)
  MazamaCoreUtils::stopIfNull(FUN)
  MazamaCoreUtils::stopIfNull(minCount)

  if ( MazamaTimeSeries::sts_isEmpty(sts) )
    stop("'sts' has no data")

  if ( length(unique(sts$meta$timezone)) > 1 )
    stop("'sts' has muliple timezones")

  # # Use internal function to determine the timezone to use
  # timezone <- .determineTimezone(sts, NULL, timezone, verbose = TRUE)
  timezone <- sts$meta$timezone[1]

  # ----- Summarize by time period ---------------------------------------------

  # See:  https://www.statology.org/aggregate-daily-data-in-r/
  # See:  https://www3.nd.edu/~steve/computing_with_data/24_dplyr/dplyr.html
  # See:  https://dplyr.tidyverse.org/articles/colwise.html

  customFUN <- function(x, ...) {
    if ( sum(!is.na(x)) >= minCount ) {
      return(FUN(x, ...))
    } else {
      return(NA)
    }
  }

  newData <-

    sts$data %>%

    # Create 'timeUnit' in the desired timezone as a grouping variable
    dplyr::mutate(
      timeUnit = lubridate::floor_date(lubridate::with_tz(.data$datetime, tz = timezone), unit)
    ) %>%
    dplyr::select(-.data$datetime) %>%
    dplyr::group_by(.data$timeUnit) %>%

    # Summarize using FUN (will ignore 'timeUnit' column)
    dplyr::summarize(across(everything(), customFUN, ...)) %>%

    # Replace +/-Inf and NaN with NA
    dplyr::mutate(across(everything(), function(x) { x[!is.finite(x)] <- NA; return(x) })) %>%

    # New, daily 'datetime'
    dplyr::rename(datetime = .data$timeUnit)


  # ----- Create the 'sts' object ----------------------------------------------

  sts$data <- newData

  # ----- Return ---------------------------------------------------------------

  return(invisible(sts))

}


