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


#' @export
#'
#' @name rawsList_isRawsList
#' @title Test for correct structure for a \emph{raws_list} object
#'
#' @param rawsList \emph{rawsList} object
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @description The \code{rawsList} is checked for the 'list' class name and
#' each element of this list is checked for correct \code{raws_timeseries} object
#' structure.
#'
#' @return \code{TRUE} if \code{rawsObject} has the correct structure, 
#' \code{FALSE} otherwise.
#' 
#' @examples 
#' \donttest{
#' library(RAWSmet)
#' 
#' rawsList_isRawsList(example_fw13List)
#' rawsList_isRawsList(example_wrccList)
#' }
#' 
rawsList_isRawsList <- function(
  rawsList = NULL,
  verbose = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(rawsList)
  
  if ( !is.list(rawsList) )
    return(FALSE)
  
  isRaws <- rawsList %>% lapply(raws_isRaws)
  
  if ( FALSE %in% isRaws ) {
    
    if ( verbose ) {
      
      if ( !is.null(names(rawsList)) ) {
        invalidObjects <- names(rawsList)[isRaws == FALSE]
        
        message("The following elements of 'rawsList' are invalid raws_timeseries objects:")
        message(paste(invalidObjects, collapse = ", "))
      }
      
    }
    
    return(FALSE)
  }
  
  # Nothing failed so return TRUE
  return(TRUE)
  
}

