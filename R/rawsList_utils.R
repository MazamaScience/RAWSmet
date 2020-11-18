#' @export
#'
#' @name rawsList_removeEmpty
#' @title Remove \emph{raws_timeseries} objects from a \emph{rawsList}.
#'
#' @param rawsList \emph{rawsList} object
#'
#' @description Removes all empty \emph{raws_timeseries} objects from a
#' \emph{rawsList}. This may be necessary after using \code{rawsList_filterDate()}
#' so that downstream functions are guaranteed to have only stations with data.
#'
#' @return \emph{rawsList} with empty stations removed.
#'
#'
rawsList_removeEmpty <- function(
  rawsList = NULL
) {

  MazamaCoreUtils::stopIfNull(rawsList)

  returnList <-
    rawsList %>%
    purrr::discard(raws_isEmpty)

  return(returnList)

}

