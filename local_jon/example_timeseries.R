# Example timeseries plots

library(RAWSmet)

raws <- fw13_createTimeseriesObject(nwsID = 500726)

start <- MazamaCoreUtils::parseDatetime("20160101", timezone = "America/Los_Angeles")
end <- MazamaCoreUtils::parseDatetime(20170101, timezone = "America/Los_Angeles")

data <- dplyr::filter(raws$data, datetime >= start & datetime < end)

AirSensor::timeseriesTbl_multiPlot(data)
