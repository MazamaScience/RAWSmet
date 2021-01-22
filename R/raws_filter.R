#' @export
#' @importFrom rlang .data
#' 
#' @title General purpose data filtering for raws_timeseries objects
#' 
#' @param rawsObject \emph{raws_timeseries} object.
#' @param ... Logical predicates defined in terms of the variables in the 
#' \code{rawsObject$data}.
#' 
#' @return A subset of the incoming \code{raws_timeseries}.
#' 
#' @description A generalized data filter for \emph{raws_timeseries} objects to 
#' choose rows/cases where conditions are true.  Multiple conditions are 
#' combined with \code{&} or separated by a comma. Only rows where the condition 
#' evaluates to TRUE are kept.Rows where the condition evaluates to \code{NA}
#' are dropped.
#' 
#' @seealso \link{raws_filterDate}
#' @examples
#' \donttest{
#' library(RAWSmet)
#' 
#' rawsObject <- example_fw13SaddleMountain
#' 
#' daytime <- raws_filter(rawsObject, solarRadiation > 0)
#' head(daytime$data)
#' }
#' 
raws_filter <- function(
  rawsObject = NULL, 
  ...
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(rawsObject)
  
  if ( !raws_isRaws(rawsObject) )
    stop("parameter 'rawsObject' is not a valid 'raws_timeseries' object")
  
  if ( raws_isEmpty(rawsObject) )
    stop("Parameter 'rawsObject' has no data.")
  
  # Remove any duplicate data records
  rawsObject <- raws_distinct(rawsObject)
  
  # ----- Filter data ----------------------------------------------------------
  
  rawsObject$data <- 
    dplyr::filter(rawsObject$data, ...)
  
  # ----- Return ---------------------------------------------------------------
  
  # Remove any duplicate data records
  rawsObject <- raws_distinct(rawsObject)
  
  return(rawsObject)
  
}
