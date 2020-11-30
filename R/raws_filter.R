#' @export
#' @importFrom rlang .data
#' 
#' @title General purpose data filtering for rawsDF objects
#' 
#' @param rawsDF \emph{rawsDF} object.
#' @param ... Logical predicates defined in terms of the variables in the 
#' \code{rawsDF}.
#' 
#' @description A generalized data filter for \emph{rawsDF} objects to 
#' choose rows/cases where conditions are true.  Multiple conditions are 
#' combined with \code{&} or separated by a comma. Only rows where the condition 
#' evaluates to TRUE are kept.Rows where the condition evaluates to \code{NA}
#' are dropped.
#' 
#' @return A subset of the incoming \code{rawsDF}.
#' 
#' @seealso \link{rawsDF_filterDate}
#' @examples
#' \donttest{
#' library(RAWSmet)
#' 
#' rawsObject <- example_fw13SaddleMountain
#' rawsDF <- rawsObject %>% raws_toRawsDF()
#' 
#' daytime <- rawsDF_filter(rawsDF, solarRadiation > 0)
#' head(daytime)
#' }
#' 
raws_filter <- function(
  rawsDF = NULL, 
  ...
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(rawsDF)
  
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
