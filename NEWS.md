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
