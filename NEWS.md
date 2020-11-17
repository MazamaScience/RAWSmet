# RAWSmet 0.2.5

* Fixed converstion to metric precipitation in `fw13_createTimeseriesObject()`.

# RAWSmet 0.2.4

* New "RAWSmet Usage" article.
* New "Oregon/Washington 2020 Water Deficit" article.
* Updated "September 2020 Oregon Wildfires" article.
* Replaced parameter `forceDownload` with `newDownload` with a default option 
of `NA`. The new behavior is to **always download** when `newDownload = TRUE`, 
**never download** when `newDownload = FALSE` and **download if not found** when
`newDownload = NA` -- the default.

# RAWSmet 0.2.3

* New `raws_filterDate()` now returns a _raws_ object with no data records when
no valid data existin within the requested time range.
* New `raw_toRawsDF()` function convertions a _raws_ objejct (list with 'meta'
and 'data') into a tidy dataframe for use with **dplyr** and **ggplot2**.
* Fixed bug in `wrcc_loadYear()` to allow it to load data from past years.
* All examples accessing archival WRCC data now include `password = MY_PASSWORD` 
so that users can set the `MY_PASSWORD` to their personal password and then run
the examples.
* Added `rawsList_removeEmpty()`.
* Fixed WRCC ingest so that columns with all missing are of type 'numeric'.

# RAWSmet 0.2.2

* Tweak downloading messages.
* Data access error messages are now displayed by `wrcc_loadMultiple()`.
* `raws_extractData(forOpenair = TRUE)` now adds `date`, `ws` and `wd` variables
based on `datetime`, `windSpeed` and `windDirection`.

# RAWSmet 0.2.1

* Changed default wrcc metadata name created by `wrcc-loadMeta()` to 
`wrcc_meta_<stateCode>.rda`
* Renamed functions with `Metadata` to just `Meta` for consistency.
* WRCC download/parse functions now harmonizing units to guarantee metric
units for all parameters.

# RAWSmet 0.2.0

* Add password support to all `wrcc_~()` data access functions.
* `wrcc_load()` was renamed to `wrcc_loadYear()`.

# RAWSmet 0.1.9

* Many examples have been updated to run with `example_` data so they can be
tested.
* New `windTimeseriesPlot()`
* Completely refactored `wrcc_identifyMonitorType()` to handle **46!!** different
data formats coming from WRCC.
* Columns with all `NA` are dropped by `timeseriesMultiplot()`.

# RAWSmet 0.1.8

* Added example datasets `example_fw13Meta.rda`, `example_wrccMeta.rda`,
`example_fw13SaddleMountain.rda`, `example_wrccSaddleMountain.rda`.

# RAWSmet 0.1.7

* Changed `stationID` to `wrccID` throughout the code base.
* Added `verbose` arguments to all data loading functions. Functions that
create metadata default to `verbose = TRUE`.
* "FW13" and "WRCC" metadata are now harmonized to always return the same set
of columns.
* Removed state names from "WRCC" metadata `siteName` field.
* New `addWindBarbs()` function to create standard wind barbs for use with
base plots.

# RAWSmet 0.1.6

* Improved `wrcc_createMetadata()` to handle combined-state metadata pages at 
WRCC.
* New `timeseriesMultiplot()` function plots the `$data` portion of 
_raws\_timeseries_ objects.

# RAWSmet 0.1.5

* Added utility functions: `raws_distinct()`, `raws_filter()`, 
`raws_filterDate()`, `raws_isEmpty()`, `raws_isRaws()` to aid in building 
"recipe style" pipelines.
* Added `fw13_load()` and `wrcc_load()` functions to create and save timeseries
data in the `rawsDataDir` directory.
* Added `fw13_loadMeta()` and `wrcc_loadMeta()` functions to create and save 
metadata in the `rawsDataDir` directory.
* `wrcc_leaflet()` renamed to `raws_leaflet()`.
* `fw13_createTimeseriesObject()` now converts data to metric units.

# RAWSmet 0.1.4

* Added "Working with openair" article.

# RAWSmet 0.1.3

* Added `README.md`.
* Added "Identifying Unique Locations" article.

# RAWSmet 0.1.2

Reading data in FW13 format from https://cefa.dri.edu/raws/

* `fw13_createMetadata()`
* `fw13_createRawDataframe()`
* `fw13_createTimeseriesObject()`
* `fw13_downloadData()`

# RAWSmet 0.1.1

First generally useful functionality:

* `raws_createMetdata()`
* `raws_createRawDataframe()`
* `raws_createTimeseriesObject()`
* `raws_downloadData()`
* `raws_identifyMonitorType()`
* `raws_leaflet()`
* `raws_parseData()`

# RAWSmet 0.1.0

* Initial Release
