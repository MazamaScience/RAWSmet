#' @export
#' @importFrom rlang .data
#' 
#' @title General purpose data filtering forraws_timeseries objects
#' 
#' @param rawsObject \emph{raws_timeseries} object.
#' @param ... Logical predicates defined in terms of the variables in the 
#' \code{rawsObject$data}.
#' 
#' @description A generalized data filter for \emph{raws_timeseries} objects to 
#' choose rows/cases where conditions are true.  Multiple conditions are 
#' combined with \code{&} or separated by a comma. Only rows where the condition 
#' evaluates to TRUE are kept.Rows where the condition evaluates to \code{NA}
#' are dropped.
#' 
#' @return A subset of the incoming \code{raws_timeseries}.
#' 
#' @seealso \link{raws_filterDate}
#' @examples
#' \dontrun{
#' library(RAWSmet)
#' setRawsDataDir("~/Data/RAWS")
#' 
#' rawsObject <- fw13_load(nwsID = 451702, year = 2005)
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
    dplyr::filter(rawsObject$data,...)
  
  # ----- Return ---------------------------------------------------------------
  
  # Remove any duplicate data records
  rawsObject <- raws_distinct(rawsObject)
  
  return(rawsObject)
  
}