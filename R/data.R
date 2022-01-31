#' @title Example FW13 station metadata
#' @format A dataframe with 66 rows and 11 columns of data.
#' @description The \code{example_fw13_meta} dataset provides a small subset of
#' FW13 station metadata containing only stations in Washington state.
#'
#' This dataset was generated on 2022-01-31 by running:
#'
#' \preformatted{
#' library(RAWSmet)
#' library(MazamaSpatialUtils)
#' library(dplyr)
#'
#' setSpatialDataDir("~/Data/Spatial")
#'
#' allfw13Meta <- fw13_createMeta()
#'
#' example_fw13_meta <- dplyr::filter(allfw13Meta, stateCode == "WA")
#'
#' save(example_fw13_meta, file = "data/example_fw13_meta.rda")
#' }
#'
"example_fw13_meta"


#' #' @title Example WRCC station metadata
#' #' @format A dataframe with 121 rows and 11 columns of data.
#' #' @description The \code{example_wrcc_meta} dataset provides a small subset of
#' #' WRCC station metadata containing only stations in Washington state.
#' #'
#' #' This dataset was generated on 2020-09-06 by running:
#' #'
#' #' \preformatted{
#' #' library(RAWSmet)
#' #' library(MazamaSpatialUtils)
#' #'
#' #' setSpatialDataDir("~/Data/Spatial")
#' #' loadSpatialData("NaturalEarthAdm1")
#' #'
#' #' example_wrcc_meta <- wrcc_createMeta(stateCode = "WA")
#' #'
#' #' save(example_wrcc_meta, file = "data/example_wrcc_meta.rda")
#' #' }
#' #'
#' "example_wrcc_meta"


#' @title Example FW13 timeseries data
#' @format A \emph{raws_timeseries object} containing \code{meta} and \code{data} dataframes.
#' \code{meta} has 1 row and 11 columns and \code{data} has 8744 rows and 13 columns.
#' @description The \code{example_fw13_Saddle_Mountain} dataset provides a quickly loadable
#' \emph{raws_timeseries} object containing FW13 data for the station in Saddle Mountain,
#' Washington, (nwsID: 452701) in 2017.
#'
#' This dataset was generated on 2022-01-31 by running:
#'
#' \preformatted{
#' library(RAWSmet)
#'
#' fw13Meta <- fw13_createMeta()
#' example_fw13_Saddle_Mountain <-
#'   fw13_createRawsObject(nwsID = "452701", meta = fw13Meta) %>%
#'   raws_filterDate(20170101, 20180101, timezone = "America/Los_Angeles")
#'
#' save(example_fw13_Saddle_Mountain, file = "data/example_fw13_Saddle_Mountain.rda")
#' }
#'
"example_fw13_Saddle_Mountain"


#' #' @title Example WRCC timeseries data
#' #' @format A \emph{raws_timeseries object} containing \code{meta} and \code{data} dataframes.
#' #' \code{meta} has 1 row and 11 columns and \code{data} has 708 rows and 9 columns.
#' #' @description The \code{example_wrcc_Saddle_Mountain} dataset provides a quickly loadable
#' #' \emph{raws_timeseries} object containing WRCC data for the station in Saddle Mountain,
#' #' Washington, (wrccID: waWSAD) between August 8th and September 6th 2020.
#' #'
#' #' This dataset was generated on 2020-09-06 by running:
#' #'
#' #' \preformatted{
#' #' library(RAWSmet)
#' #' setRawsDataDir("~/Data/RAWS")
#' #'
#' #' wrccMeta <- wrcc_loadMeta(stateCode = "WA")
#' #' example_wrcc_Saddle_Mountain <- wrcc_loadYear(wrccID = "waWSAD", meta = wrccMeta, year = 2020, password = MY_PASSWORD)
#' #'
#' #' save(example_wrcc_Saddle_Mountain, file = "data/example_wrcc_Saddle_Mountain.rda")
#' #' }
#' #'
#' "example_wrcc_Saddle_Mountain"

#' #' @title Example list of WRCC timeseries data
#' #' @format A list of two \emph{raws_timeseries} objects each containing \code{meta} and \code{data} dataframes.
#' #' @description The \code{example_wrccList} dataset provides a quickly loadable list of
#' #' \emph{raws_timeseries} objects containing WRCC data for the station in Saddle Mountain, and Wellpinit
#' #' Washington, (wrccIDs: waWSAD, waWWLP) between September 30th and October 1st 2020.
#' #'
#' #' This dataset was generated on 2020-10-1 by running:
#' #'
#' #' \preformatted{
#' #' library(RAWSmet)
#' #'
#' #' setRawsDataDir(~/Data/RAWS/)
#' #'
#' #' wrccIDs <- c("waWSAD", "waWWLP")
#' #' example_wrccList <- wrcc_loadMultiple(wrccIDs = wrccIDs, year = 2020, password = MY_PASSWORD)
#' #'
#' #' save(example_wrccList, file = "data/example_wrccList.rda")
#' #' }
#' #'
#' "example_wrccList"

#' @title Example list of FW13 timeseries data
#' @format A list of two \emph{raws_timeseries} objects each containing \code{meta} and \code{data} dataframes.
#' @description The \code{example_fw13List} dataset provides a quickly loadable list of
#' \emph{raws_timeseries} objects containing FW13 data for the station in Saddle Mountain, and Wellpinit
#' Washington, (nwsIDs: 452701, 452918) from September 2017.
#'
#' This dataset was generated on 2022-01-31 by running:
#'
#' \preformatted{
#' library(RAWSmet)
#'
#' setRawsDataDir("~/Data/RAWS/")
#'
#' nwsIDs <- c("452701", "452918")
#' example_fw13List <-
#'   fw13_loadMultiple(nwsIDs = nwsIDs) %>%
#'   rawsList_filterDate(20170901, 20171001, timezone = "America/Los_Angeles")
#'
#' save(example_fw13List, file = "data/example_fw13List.rda")
#' }
#'
"example_fw13List"
