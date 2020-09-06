#' @title Example FW13 station metadata
#' @format A dataframe with 66 rows and 11 columns of data.
#' @description The \code{example_fw13Meta} dataset provides a small subset of
#' FW13 station metadata containing only stations in Washington state.
#' 
#' This dataset was generated on 2020-09-06 by running:
#' 
#' \preformatted{
#' library(RAWSmet)
#' library(MazamaSpatialUtils)
#' library(dplyr)
#' 
#' setSpatialDataDir("~/Data/Spatial")
#' 
#' allfw13Meta <- fw13_createMetadata()
#' 
#' example_fw13Meta <- dplyr::filter(allfw13Meta, stateCode == "WA")
#' 
#' 
#' save(example_fw13Meta, file = "data/example_fw13Meta.rda")
#' }
#' 
"example_fw13Meta"


#' @title Example WRCC station metadata
#' @format A dataframe with 121 rows and 11 columns of data.
#' @description The \code{example_wrccMeta} dataset provides a small subset of
#' WRCC station metadata containing only stations in Washington state.
#' 
#' This dataset was generated on 2020-09-06 by running:
#' 
#' \preformatted{
#' library(RAWSmet)
#' library(MazamaSpatialUtils)
#' 
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("NaturalEarthAdm1") 
#' 
#' example_wrccMeta <- wrcc_createMetadata(stateCode = "WA")
#' 
#' save(example_wrccMeta, file = "data/example_wrccMeta.rda")
#' }
#' 
"example_wrccMeta"


#' @title Example FW13 timeseries data
#' @format A \emph{raws_timeseries object} containing \code{meta} and \code{data} dataframes. 
#' \code{meta} has 1 row and 11 columns and \code{data} has 8744 rows and 13 columns.
#' @description The \code{example_fw13SaddleMountain} dataset provides a quickly loadable
#' \emph{raws_timeseries} object containing FW13 data for the station in Saddle Mountain,
#' Washington, (nwsID: 452701) in 2017.
#' 
#' This dataset was generated on 2020-09-06 by running:
#' 
#' \preformatted{
#' library(RAWSmet)
#' 
#' fw13Meta <- fw13_createMetadata()
#' example_fw13SaddleMountain <- 
#'   fw13_createTimeseriesObject(nwsID = "452701", meta = fw13Meta) %>%
#'   raws_filterDate(20170101, 20180101, timezone = "America/Los_Angeles")
#'   
#' save(example_fw13SaddleMountain, file = "data/example_fw13SaddleMountain.rda")
#' }
#' 
"example_fw13SaddleMountain"


#' @title Example WRCC timeseries data
#' @format A \emph{raws_timeseries object} containing \code{meta} and \code{data} dataframes. 
#' \code{meta} has 1 row and 11 columns and \code{data} has 708 rows and 9 columns.
#' @description The \code{example_wrccSaddleMountain} dataset provides a quickly loadable
#' \emph{raws_timeseries} object containing WRCC data for the station in Saddle Mountain,
#' Washington, (wrccID: waWSAD) between August 8th and September 6th 2020.
#' 
#' This dataset was generated on 2020-09-06 by running:
#' 
#' \preformatted{
#' library(RAWSmet)
#' 
#' example_wrccSaddleMountain <- wrcc_createTimeseriesObject(wrccID = "waWSAD", startdate = 20200808, enddate = 20200906)
#'   
#' save(example_wrccSaddleMountain, file = "data/example_wrccSaddleMountain.rda")
#' }
#' 
"example_wrccSaddleMountain"