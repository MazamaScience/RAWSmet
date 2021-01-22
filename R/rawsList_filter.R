#' @export
#' @importFrom rlang .data
#'
#' @title General purpose data filtering for a list of raws_timeseries objects
#'
#' @param rawsList List of \emph{raws_timeseries} objects.
#' @param ... Logical predicates defined in terms of the variables in the
#' \code{rawsObject$data}.
#'
#' @return A list of subsets of the elements of the given list of
#' \emph{raws_timeseries} objects.
#'
#' @description A generalized data filter for a list of \emph{raws_timeseries}
#' objects to choose rows/cases where conditions are true.  Multiple conditions are
#' combined with \code{&} or separated by a comma. Only rows where the condition
#' evaluates to TRUE are kept.Rows where the condition evaluates to \code{NA}
#' are dropped.
#'
#' @seealso \link{rawsList_filterDate}
#' @examples
#' \dontrun{
#' library(RAWSmet)
#'
#' rawsList <- example_fw13Multiple
#'
#' daytime <- rawsList_filter(rawsList, solarRadiation > 0)
#' }
#'
rawsList_filter <- function(
  rawsList = NULL,
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(rawsList)

  if ( !is.list(rawsList) )
    stop("Parameter 'rawsList' must be a list.")

  for ( i in seq(length(rawsList)) ) {
    if ( !raws_isRaws(rawsList[[i]]) )
      stop(sprintf("Element %s in 'rawsList' is not a valid 'raws_timeseries' object.", rawsList[[i]]$meta$wrccID))
    if ( raws_isEmpty(rawsList[[i]]) )
      stop(sprintf("Element %s in 'rawsObject' has no data.", rawsList[[i]]$meta$wrccID))
  }

  # ----- Filter data ----------------------------------------------------------

  rawsList <- rawsList %>% purrr::map(function(x) raws_filter(x, ...))


  # ----- Return ---------------------------------------------------------------

  return(rawsList)

}
