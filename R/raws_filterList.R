#' @export
#' @importFrom rlang .data
#' 
#' @title General purpose data filtering for a list of raws_timeseries objects
#' 
#' @param rawsObjects List of \emph{raws_timeseries} object.
#' @param ... Logical predicates defined in terms of the variables in the 
#' \code{rawsObject$data}.
#' 
#' @description A generalized data filter for a list of \emph{raws_timeseries} 
#' objects to choose rows/cases where conditions are true.  Multiple conditions are 
#' combined with \code{&} or separated by a comma. Only rows where the condition 
#' evaluates to TRUE are kept.Rows where the condition evaluates to \code{NA}
#' are dropped.
#' 
#' @return A list of subsets of the elements of the given list of 
#' \emph{raws_timeseries} objects.
#' 
#' @seealso \link{raws_filterDateList}
#' @examples
#' \donttest{
#' library(RAWSmet)
#' 
#' rawsObjects <- example_fw13Multiple
#' 
#' daytime <- raws_filter(rawsObjects, solarRadiation > 0)
#' }
#' 
raws_filterList <- function(
  rawsObjects = NULL, 
  ...
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(rawsObjects)
  
  if ( !is.list(rawsObjects) )
    stop("Parameter 'rawsObjects' must be a list.")
  
  for ( i in seq(length(rawsObjects)) ) {
    if ( !raws_isRaws(rawsObjects[[i]]) )
      stop(sprintf("Element %s in 'rawsObjects' is not a valid 'raws_timeseries' object.", rawsObjects[[i]]$meta$wrccID))
    if ( raws_isEmpty(rawsObjects[[i]]) )
      stop(sprintf("Element %s in 'rawsObject' has no data.", rawsObjects[[i]]$meta$wrccID))
    
    # Remove any duplicate data records
    rawsObjects[[i]] <- raws_distinct(rawsObjects[[i]])
  }
  
  # ----- Filter data ----------------------------------------------------------
  
  for ( i in seq(length(rawsObjects)) ) {
    
    rawsObjects[[i]] <-
      raws_filter(rawsObjects[[i]], ...)
    
    # Remove any duplicate data records
    rawsObjects[[i]] <- raws_distinct(rawsObjects[[i]])
  }
  
  # ----- Return ---------------------------------------------------------------

  return(rawsObjects)
  
}
