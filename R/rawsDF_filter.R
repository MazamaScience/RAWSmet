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
rawsDF_filter <- function(
  rawsDF = NULL, 
  ...
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(rawsDF)
  
  if ( !rawsDF_isRawsDF(rawsDF) )
    stop("parameter 'rawsDF' is not a valid 'rawsDF' object")
  
  if ( nrow(rawsDF) == 0 )
    stop("Parameter 'rawsDF' has no data.")
  
  # ----- Filter data ----------------------------------------------------------

  rawsDF <- 
    dplyr::filter(rawsDF, ...)
  
  # ----- Return ---------------------------------------------------------------
  
  return(rawsDF)
  
}
