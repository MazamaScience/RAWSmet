#' #' @export
#' #' @importFrom rlang .data
#' #'
#' #' @title Convert a list of raws_timeseries objects to a single rawsDF object
#' #'
#' #' @param rawsList List of \emph{raws_timeseries} objects.
#' #'
#' #' @return A single tidy dataframe containing data and metadata for each
#' #' \emph{raws_timeseries} object in a \emph{raws_list}.
#' #'
#' #' @description Converts a list of \emph{raws_timeseries} objects (\emph{raws_list})
#' #' to a single, tidy dataframe each containing all variables from each \emph{raws_timeseries}
#' #' object's \code{data} dataframe along with the following values from their \code{meta} dataframes:
#' #'
#' #' \enumerate{
#' #'  \item{nwsID - the nwsID of the station}
#' #'  \item{wrccID - the wrccID of the station}
#' #'  \item{locationName - the name of the station}
#' #'  \item{longitude - longitude coordinate of the station}
#' #'  \item{latitude - latitude coordinate of the station}
#' #'  \item{timezone - the timezone the station is in}
#' #'  \item{elevation - elevation of the station}
#' #' }
#' #'
#' #' This version of the RAWS data is known as a \emph{rawsDF} object.
#' #'
#' #' Replicating this set of variables for every record greatly inflates the size
#' #' of the data but also makes it much more useful when working with the
#' #' \pkg{dplyr} and \code{ggplot2} packages.
#' #'
#' #' Multiple \emph{rawsDF} objects can be combined with \code{dplyr::bind_rows()}
#' #' and used to create multi-station plots.
#' #'
#' #' @seealso \link{raws_toRawsDF}
#' #' @examples
#' #' library(RAWSmet)
#' #'
#' #' rawsDF<- example_fw13List %>% rawsList_toRawsDF()
#' #'
#' #' head(rawsDF)
#' #'
#' rawsList_toRawsDF <- function(
#'   rawsList = NULL
#' ) {
#'
#'   # ----- Validate parameters --------------------------------------------------
#'
#'   MazamaCoreUtils::stopIfNull(rawsList)
#'
#'   if ( !rawsList_isRawsList(rawsList) )
#'     stop("Parameter 'rawsList' is not a valid raws_list object.")
#'
#'   # ----- Filter data ----------------------------------------------------------
#'
#'   rawsList <- rawsList %>% purrr::map(function(x) raws_toRawsDF(x))
#'
#'   # ----- Return ---------------------------------------------------------------
#'
#'   tidyDF <- dplyr::bind_rows(rawsList)
#'
#'   return(tidyDF)
#'
#' }
