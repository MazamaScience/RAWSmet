#' @export
#' @importFrom rlang .data
#'
#' @title Convert a list of raws_timeseries objects to a list of rawsDF object
#'
#' @param rawsList List of \emph{raws_timeseries} objects.
#'
#' @description Converts a list of \emph{raws_timeseries} objects (\emph{raws_list}) 
#' to a list of single, tidy dataframes each containing all varaibles from their
#' respective \code{raws_timeseries} object's \code{data} dataframe along with
#' the following values from its \code{meta} dataframe:
#' 
#' \enumerate{
#'  \item{nwsID - the nwsID of the station}
#'  \item{wrccID - the wrccID of the station}
#'  \item{siteName - the name of the station}
#'  \item{longitude - longitude coordinate of the station}
#'  \item{latitude - latitude coordinate of the station}
#'  \item{elevation - elevation of the station}
#' }
#' 
#' This version of the RAWS data is known as a \emph{rawsDF} object.
#' 
#' Replicating this set of variables for every record greatly inflates the size
#' of the data but also makes it much more useful when working with the 
#' \pkg{dplyr} and \code{ggplot2} packages.
#' 
#' Multiple \emph{rawsDF} objects can be combined with \code{dplyr::bind_rows()} 
#' and used to create multi-station plots.
#'
#' @return A list of tidy dataframes containing data and metadata for each
#' \emph{raws_timeseries} object in a \emph{raws_list}.
#'
#' @seealso \link{raws_toRawsDF}
#' @examples
#' library(RAWSmet)
#'
#' rawsDF_list <- example_fw13List %>% rawsList_toRawsDF()
#' 
#' dplyr::glimpse(rawsDF_list[[1]])
#'
rawsList_toRawsDF <- function(
  rawsList = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(rawsList)
  
  
  # ----- Filter data ----------------------------------------------------------
  
  rawsList <- rawsList %>% purrr::map(function(x) raws_toRawsDF(x))
  
  
  # ----- Return ---------------------------------------------------------------
  
  return(rawsList)
  
}
