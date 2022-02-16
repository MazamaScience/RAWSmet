[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/RAWSmet)](https://cran.r-project.org/package=RAWSmet)
[![Downloads](http://cranlogs.r-pkg.org/badges/RAWSmet)](https://cran.r-project.org/package=RAWSmet)
[![Build Status](https://travis-ci.org/MazamaScience/RAWSmet.svg?branch=master)](https://travis-ci.org/MazamaScience/RAWSmet)

# RAWSmet R Package 

```
Utilities for working with Remote Automatic Weather Station (RAWS) data
```

## Background

The USFS Pacific Wildland Fire Sciences Lab [AirFire](https://www.airfire.org) 
team works to model wildland fire emissions and has created the BlueSky Modeling 
Framework. This system  integrates a wide collection of models along a smoke 
modeling pipeline (fire  information > fuel loadings > consumption modeling > 
emissions modeling > time rate of emissions modeling > plume height estimations > 
smoke trajectory and dispersion  modeling). The resulting model output has 
been integrated into many different smoke prediction systems and scientific 
modeling efforts.

The **RAWSmet** R package is being developed for AirFire to help modelers and 
scientists more easily work with weather data from RAWS stations across 
North America.

The package makes it easier to obtain data, perform analyses and generate 
reports. It includes functionality to:

* access metadata and timeseries data from https://cefa.dri.edu/raws
* access metadata and timeseries data from https://raws.dri.edu
* save and reload .RData versions of these in a rawsDataDir
* determine which locations obtained from metadata are too close to be 
considered “unique” locations
* convert between UTC and local timezones
* apply various algorithms to the data: rolling means, aggregation, _etc._
* provide interactive timeseries and maps through RStudio’s Viewer pane
* create a variety of publication ready maps and timeseries plots

## Installation

Users will want to install the **remotes** package to have access to the latest 
version of the package from GitHub.

The following packages should be installed by typing the following at the 
RStudio console:

```
# Note that vignettes require knitr and rmarkdown
install.packages('knitr')
install.packages('rmarkdown')
install.packages('MazamaSpatialUtils')
install.packages('MazamaLocationUtils')
devtools::install_github('MazamaScience/RAWSmet')
```
Any work with spatial data, _e.g._ assigning states, counties and timezones, 
will require installation of required spatial datasets. To get these datasets 
you should type the following at the RStudio console:

```
library(MazamaSpatialUtils)
dir.create('~/Data/Spatial', recursive = TRUE)
setSpatialDataDir('~/Data/Spatial')
installSpatialData()
```

Data generated with package functions can be be saved and reloaded in a
dedicated directory much the same as the `spatialDataDir` used above:

```
library(RAWSmet)
dir.create('~/Data/RAWS', recursive = TRUE)
setRawsDataDir('~/Data/RAWS')
```

----

This R package was created by [Mazama Science](http://mazamascience.com) and is 
being funded by the USFS [AirFire Research Team](https://airfire.org).
