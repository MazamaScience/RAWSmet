#' @export
#'
#' @title Identify RAWS station type
#'
#' @param fileString character string containing RAWS data
#' @description Examine the column names of the incoming character vector
#' to identify different types of monitor data provided by RAWS.
#'
#' The return is a list includes everything needed to identify and parse the raw
#' data using \code{readr::read_tsv()}:
#'
#' \itemize{
#' \item{\code{monitorType}}{ -- identification string}
#' \item{\code{rawNames}}{ -- column names from the data (including special characters)}
#' \item{\code{columnNames}}{ -- assigned column names (special characters repaced with '.')}
#' \item{\code{columnTypes}}{ -- column type string for use with \code{readr::read_csv()}}
#' }
#'
#' The \code{monitorType} will be one of:
#' \itemize{
#' \item{"\code{WRCC_TYPE1}"}{ -- ???}
#' \item{"\code{WRCC_TYPE2}"}{ -- ???}
#' \item{"\code{WRCC_TYPE3}"}{ -- ???}
#' \item{"\code{UNKOWN}"}{ -- ???}
#' }
#'
#' @return List including \code{monitorType}, \code{rawNames}, \code{columnNames} and \code{columnTypes}.
#' @references \href{https://raws.dri.edu/}{RAWS USA Climate Archive}
#' @examples
#' \dontrun{
#' library(RAWSmet)
#' 
#' fileString <- wrcc_downloadData(wrccID = 'WENU')
#' monitorTypeList <- wrcc_identifyMonitorType(fileString)
#' }

wrcc_identifyMonitorType <- function(fileString) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( class(fileString)[1] != "character" )
    stop(paste0('WRCC fileString is of type %s', class(fileString)[1]))
  
  # ----- Different header styles ----------------------------------------------
  
  # Type 1
  type1_header <- vector('character',3)
  type1_header[1] <- ":       LST	 mm  	 m/s 	 Deg 	Deg C	Deg C	  %  	volts	  %  	 Deg 	 m/s 	 W/m2"
  type1_header[2] <- ": Date/Time	 Precip	  Wind 	 Wind  	 Av Air	  Fuel 	  Rel  	Battery	Av Fuel	   Dir 	Mx Gust	 Solar "
  type1_header[3] <- ":YYMMDDhhmm	       	  Speed	 Direc 	  Temp 	  Temp 	Humidty	Voltage	 Moistr	 MxGust	 Speed 	  Rad. "
  type1_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', 'AvFuelMoistr', 'DirMxGust', 'MxGustSpeed', 'SolarRad')
  type1_names <- type1_rawNames
  type1_types <- 'cddddddddddd'
  
  # Type 2
  type2_header <- vector('character',3)
  type2_header[1] <- ":       LST	 mm  	 m/s 	 Deg 	Deg C	  %  	volts	 Deg 	 m/s 	 W/m2"
  type2_header[2] <- ": Date/Time	 Precip	  Wind 	 Wind  	 Av Air	  Rel  	Battery	   Dir 	Mx Gust	 Solar "
  type2_header[3] <- ":YYMMDDhhmm	       	  Speed	 Direc 	  Temp 	Humidty	Voltage	 MxGust	 Speed 	  Rad. "
  type2_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'RelHumidty',
                      'BatteryVoltage', 'DirMxGust', 'MxGustSpeed', 'SolarRad')
  type2_names <- type2_rawNames
  type2_types <- 'cddddddddd'
  
  # Type 3
  type3_header <- vector('character',3)
  type3_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t  %  \t Deg \t m/s \t W/m2"   
  type3_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t  Fuel \t   Dir \tMx Gust\t Solar "
  type3_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\tMoistur\t MxGust\t Speed \t  Rad. "
  type3_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', 'AvFuelMoistr', 'DirMxGust', 'MxGustSpeed', 'SolarRad')
  type3_names <- type3_rawNames
  type3_types <- 'cddddddddddd'
  
  # Type 4
  type4_header <- vector('character',3)
  type4_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t Deg \t m/s \t W/m2"  
  type4_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t   Dir \tMx Gust\t Solar "
  type4_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t MxGust\t Speed \t  Rad. "
  type4_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', 'DirMxGust', 'MxGustSpeed', 'SolarRad')
  type4_names <- type4_rawNames
  type4_types <- 'cdddddddd'
  
  # Type 5
  type5_header <- vector('character',3)
  type5_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t  %  \tcbars\tDeg C\t Deg \t m/s \t W/m2"          
  type5_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\tAv Fuel\t Soil  \t4\" Soil\t   Dir \tMx Gust\t Solar "
  type5_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t Moistr\tMoistr\tAv Temp\t MxGust\t Speed \t  Rad. " 
  type5_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', "AvFuelMoistr", "SoilMoistr", "SoilTemp", 'DirMxGust', 'MxGustSpeed', 'SolarRad')
  type5_names <- type5_rawNames
  type5_types <- 'cddddddddddd'
  
  # Type 6
  type6_header <- vector('character',3)
  type6_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t Deg \t m/s \t  %  \t W/m2"         
  type6_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t   Dir \tMx Gust\tAv Fuel\t Solar "
  type6_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t MxGust\t Speed \t Moistr\t  Rad. "
  type6_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', 'DirMxGust', 'MxGustSpeed', "AvFuelMoistr", 'SolarRad')
  type6_names <- type6_rawNames
  type6_types <- 'cdddddddddd'
  
  # Type 7
  type7_header <- vector('character',3)
  type7_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t  %  \tcbars\tDeg C\t Deg \t m/s "          
  type7_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t  Fuel \t Soil  \t4\" Soil\t   Dir \tMx Gust"
  type7_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\tMoistr\tMoistr\tAv Temp\t MxGust\t Speed "
  type7_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', 'AvFuelMoistr', 'SoilMoistr', 'SoilAvTemp', 'DirMxGust', 'MxGustSpeed')
  type7_names <- type7_rawNames
  type7_types <- 'cdddddddddddd'
  
  # Type 8
  type8_header <- vector('character',3)
  type8_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\tmbar \t  %  \t Deg \t m/s \t W/m2"       
  type8_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t Barom \tAv Fuel\t   Dir \tMx Gust\t Solar "
  type8_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t Press \t Moistr\t MxGust\t Speed \t  Rad. "
  type8_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', 'BaromPress', 'AvFuelMoistr', 'DirMxGust', 'MxGustSpeed', 'SolarRad')
  type8_names <- type8_rawNames
  type8_types <- 'cdddddddddddd'
  
  # Type 9
  type9_header <- vector('character',3)
  type9_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\t  %  \tvolts\tmbar \t Deg \t m/s \t W/m2\t m/s \tDeg C\tmbar \t mi. \t     \tDeg C\tf/100\t Unk \tf/100\t Unk \tf/100\t Unk "        
  type9_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Rel  \tBattery\t Barom \t   Dir \tMx Gust\t Solar \t  Wind \tAve Air\tAltimtr\t Vis.  \t State \tDew Pt \t Cloud \t Misc  \t Cloud \t Misc  \t Cloud \t Misc  "
  type9_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \tHumidty\tVoltage\t Press \t MxGust\t Speed \t  Rad. \t Spd #2\tTmp 217\t       \t       \t of Wx \t Temp  \tLayer 1\t  #1   \tLayer 2\t  #2   \tLayer 3\t  #3   "
  type9_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'RelHumidty',
                      'BatteryVoltage', 'BaromPress', 'DirMxGust', 'MxGustSpeed', 'SolarRad', 'WindSpd2',
                      "AveAirTmp", 'Altimtr', "Vis", 'StateOfWx', 'DewPtTemp', 'CloudLayer1', 'Misc1',
                      'CloudLayer2', 'Misc2', 'CloudLayer3', 'Misc3')
  type9_names <- type9_rawNames
  type9_types <- 'cdddddddddddddddddddddd'
  
  # Type 10
  type10_header <- vector('character',3)
  type10_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t  %  \t Deg \t m/s \t W/m2\tcbars\tDeg C"
  type10_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\tAv Fuel\t   Dir \tMx Gust\t Solar \t Soil  \t4\" Soil"
  type10_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t Moistr\t MxGust\t Speed \t  Rad. \tMoistr\tAv Temp" 
  type10_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp',  'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', 'AvFuelMoistr', 'DirMxGust', 'MxGustSpeed', 'SolarRad', 'SoilMoistr', 'SoilAvTemp')

  type10_names <- type10_rawNames
  type10_types <- 'cddddddddddddd'
  
  # Type 11
  type11_header <- vector('character',3)
  type11_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t  %  \t Deg \t m/s \t mm  \t W/m2"
  type11_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t  Fuel \t   Dir \tMx Gust\t#2 Rain\t Solar "
  type11_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\tMoistr\t MxGust\t Speed \t Gauge \t  Rad. "
  type11_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp',  'FuelTemp', 'RelHumidty',
                       'BatteryVoltage', 'AvFuelMoistr', 'DirMxGust', 'MxGustSpeed', '2RainGauge', 'SolarRad')
  
  type11_names <- type11_rawNames
  type11_types <- 'cdddddddddddd'
  
  # Type 12
  type12_header <- vector('character',3)
  type12_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t  %  \t Deg \t m/s \tcbars\tDeg C\t W/m2"
  type12_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t  Fuel \t   Dir \tMx Gust\t Soil  \t4\" Soil\t Solar "
  type12_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\tMoistr\t MxGust\t Speed \tMoistr\tAv Temp\t  Rad. "
  type12_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp',  'FuelTemp', 'RelHumidty',
                         'BatteryVoltage', 'AvFuelMoistre', 'DirMxGust', 'MxGustSpeed', 'SoilMoistr', 'SoilAvTemp', 'SolarRad')
  
  type12_names <- type12_rawNames
  type12_types <- 'cddddddddddddd'
  
  # Type 13
  type13_header <- vector('character',3)
  type13_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t  %  \t Deg \t m/s \t W/m2\tDeg C\t  %  "
  type13_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t  Fuel \t   Dir \tMx Gust\t Solar \tAv. Air\tAve Rel"
  type13_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\tMoistr\t MxGust\t Speed \t  Rad. \tTemp #2\tHumd #2"
  type13_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp',  'FuelTemp', 'RelHumidty',
                       'BatteryVoltage', 'AvFuelMoistr', 'DirMxGust', 'MxGustSpeed', 'SolarRad', 'AvAirTemp2', 'AveRelHumd2')
  
  type13_names <- type13_rawNames
  type13_types <- 'cddddddddddddd'
  
  # Type 14
  type14_header <- vector('character',3)
  type14_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t  %  \t Deg \t m/s \tDeg C\tDeg C\t  %  \t  %  \t W/m2"
  type14_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t  Fuel \t   Dir \tMx Gust\t 24 hr \t 24 hr \t 24 hr \t 24 hr \t Solar "
  type14_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\tMoistr\t MxGust\t Speed \tMx Air \tMn Air \tMx Rel \tMn Rel \t  Rad. "
  type14_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp',  'FuelTemp', 'RelHumidty',
                         'BatteryVoltage', 'DirMxGust', 'MxGustSpeed', '24hrMxAir', '24hrMnAir', '24hrMxRel', '24hrMnRel', 'SolarRad')
  
  type14_names <- type14_rawNames
  type14_types <- 'cddddddddddddd'

  # Type 15
  type15_header <- vector('character',3)
  type15_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t Deg \t m/s \t  %  \tDeg C\tDeg C\t  %  \t  %  \t W/m2"
  type15_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t   Dir \tMx Gust\t  Fuel \t 24 hr \t 24 hr \t 24 hr \t 24 hr \t Solar "
  type15_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t MxGust\t Speed \tMoistr\tMx Air \tMn Air \tMx Rel \tMn Rel \t  Rad. "
  type15_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp',  'FuelTemp', 'RelHumidty',
                         'BatteryVoltage', 'DirMxGust', 'MxGustSpeed', 'AvFuelMoistre', '24hrMxAir', '24hrMnAir', '24hrMxRel', '24hrMnRel', 'SolarRad')
  
  type15_names <- type15_rawNames
  type15_types <- 'cddddddddddddd'
  
  # Type 16
  type16_header <- vector('character',3)
  type16_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t  %  \t Deg \t m/s \t W/m2\t mm  "
  type16_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t  Fuel \t   Dir \tMx Gust\t Solar \t#2 Rain"
  type16_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\tMoistr\t MxGust\t Speed \t  Rad. \t Gauge "
  type16_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp',  'FuelTemp', 'RelHumidty',
                         'BatteryVoltage', 'DirMxGust', 'MxGustSpeed', 'SolarRad', '2RainGauge')
  
  type16_names <- type16_rawNames
  type16_types <- 'cddddddddddd'
  
  # Type 17
  type17_header <- vector('character',3)
  type17_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t  %  \t Deg \t m/s \t mm  \t W/m2\t mm  \t  %  \t     \tDeg C\t  %  \t     \tDeg C"
  type17_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\tAv Fuel\t   Dir \tMx Gust\t Snow  \t Solar \t Snow  \tSoil M \t Soil C\t  Soil \tSoil M \t Soil C\t Soil T"
  type17_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t Moistr\t MxGust\t Speed \t Depth \t  Rad. \t Depth \t       \t1st Sen\t Temp 1\t   #2  \t2nd Sen\t2nd Sen"
  type17_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp',  'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', 'AvFuelMoistr', 'DirMxGust', 'MxGustSpeed', 'SnowDepth', 'SolarRad', 
                      'SnowDepth', 'SoilM', 'SoilC1stSen', 'SoilM2', 'SoilC2ndSen', 'SoilT2ndSen')
  
  type17_names <- type17_rawNames
  type17_types <- 'cdddddddddddddddddd'
  
  # Type 18
  type18_header <- vector('character',3)
  type18_header[1] <- ":       LST\t Unk \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\tmbar \tcbars\t W/m2\t Deg \t m/s \t mm  "
  type18_header[2] <- ": Date/Time\t Misc  \t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t Barom \t Soil  \t Solar \t   Dir \tMx Gust\t Precip"
  type18_header[3] <- ":YYMMDDhhmm\t  #1   \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t Press \tMoistr\t  Rad. \t MxGust\t Speed \t       "
  type18_rawNames <- c('LST_datestamp', 'Misc1', 'WindSpeed', 'WindDirec', 'AvAirTemp',  'FuelTemp', 'RelHumidty',
                         'BatteryVoltage', 'BaromPress', 'SoilMoistr', 'SolarRad', 'DirMxGust', 'MxGustSpeed', 'Precip')
  
  type18_names <- type18_rawNames
  type18_types <- 'cddddddddddddd'
  
  # Type 19
  type19_header <- vector('character',3)
  type19_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t Deg \t m/s \t  %  \t W/m2"
  type19_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t   Dir \tMx Gust\t  Fuel \t Solar "
  type19_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t MxGust\t Speed \tMoistr\t  Rad. "
  type19_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp',  'FuelTemp', 'RelHumidty',
                         'BatteryVoltage', 'DirMxGust', 'MxGustSpeed', 'AvFuelMoistr', 'SolarRad')
  
  type19_names <- type19_rawNames
  type19_types <- 'cddddddddddddd'
  
  # Type 20
  type20_header <- vector('character',3)
  type20_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\tcbars\tDeg C\t  %  \t Deg \t m/s \t W/m2"
  type20_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t Soil  \t4\" Soil\tAv Fuel\t   Dir \tMx Gust\t Solar "
  type20_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\tMoistr\tAv Temp\t Moistr\t MxGust\t Speed \t  Rad. "
  type20_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp',  'FuelTemp', 'RelHumidty',
                         'BatteryVoltage', 'SoilMoistr', 'SoilAvTemp', 'AvFuelMoistr', 'DirMxGust', 'MxGustSpeed', 'SolarRad')
  
  type20_names <- type20_rawNames
  type20_types <- 'cddddddddddddd'
  
  # Type 21
  type21_header <- vector('character',3)
  type21_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t  %  \t Deg \t m/s \tcbars\tDeg C\t W/m2"
  type21_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\tAv Fuel\t   Dir \tMx Gust\t Soil  \t4\" Soil\t Solar "
  type21_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t Moistr\t MxGust\t Speed \tMoistr\tAv Temp\t  Rad. "
  type21_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp',  'FuelTemp', 'RelHumidty',
                         'BatteryVoltage', 'DirMxGust', 'MxGustSpeed', 'SoilMoistr', 'SoilAvTemp', 'SolarRad')
  
  type21_names <- type21_rawNames
  type21_types <- 'cddddddddddddd'
  
  # Type 22
  type22_header <- vector('character',3)
  type22_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t  %  \tcbars\tDeg C\t Deg \t m/s "
  type22_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\tAv Fuel\t Soil  \t4\" Soil\t   Dir \tMx Gust"
  type22_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t Moistr\tMoistr\tAv Temp\t MxGust\t Speed "
  type22_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp',  'FuelTemp', 'RelHumidty',
                         'BatteryVoltage', 'AvFuelMoistr', 'SoilMoistr', 'SoilAvTemp', 'DirMxGust', 'MxGustSpeed')
  
  type22_names <- type22_rawNames
  type22_types <- 'cddddddddddddd'
  
  # Type 23
  type23_header <- vector('character',3)
  type23_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t W/m2\tcbars\tDeg C\tcbars\tcbars\t Deg \t m/s \t mm  \t  %  "
  type23_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t Solar \t Soil  \t4\" Soil\t Soil  \t Soil  \t   Dir \tMx Gust\t#2 Rain\tAv Fuel"
  type23_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t  Rad. \tMoistr\tAv Temp\tMoistr\tMoistr\t MxGust\t Speed \t Gauge \t Moistr"
  type23_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp',  'FuelTemp', 'RelHumidty',
                       'BatteryVoltage', 'SolarRad', 'SoilMoistr', 'SoilAvTemp', 'SoilMoistr', 'SoilMoistr', 
                       'DirMxGust', 'MxGustSpeed', '2RainGauge', 'AvFuelMoistr')
  
  type23_names <- type23_rawNames
  type23_types <- 'cdddddddddddddddd'
  
  # Type 24
  type24_header <- vector('character',3)
  type24_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\t  %  \t W/m2"
  type24_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Rel  \t Solar "
  type24_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \tHumidty\t  Rad. "
  type24_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'SolarRad')
  
  type24_names <- type24_rawNames
  type24_types <- 'cddddd'
  
  # Type 25
  type25_header <- vector('character',3)
  type25_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\tmbar \t Deg \t m/s \t  %  \t W/m2"
  type25_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t Barom \t   Dir \tMx Gust\tAv Fuel\t Solar "
  type25_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t Press \t MxGust\t Speed \t Moistr\t  Rad. "
  type25_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                       'BatteryVoltage', 'BaromPress', 'DirMxGust', 'MxGustSpeed', 'AvFuelMoistr', 'SolarRad')
  
  type25_names <- type25_rawNames
  type25_types <- 'cdddddddddddd'
  
  # Type 26
  type26_header <- vector('character',3)
  type26_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t Deg \t m/s \tDeg C\tDeg C\t  %  \t  %  \t W/m2"
  type26_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t   Dir \tMx Gust\t 12 hr \t 12 hr \t 12 hr \t 12 hr \t Solar "
  type26_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t MxGust\t Speed \tMx Air \tMn Air \tMx Rel \tMn Rel \t  Rad. "
  type26_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'BatteryVoltage',
                       'DirMxGust', 'MxGustSpeed', '12hrMxAir', '12hrMnAir', '12hrMxRel', '12hrMnRel', 'SolarRad')
  
  type26_names <- type26_rawNames
  type26_types <- 'cddddddddddddd'
  
  # Type 27
  type27_header <- vector('character',3)
  type27_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t  %  \t Deg \t m/s \t W/m2\t     \tDeg C\tDeg C"
  type27_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\tAv Fuel\t   Dir \tMx Gust\t Solar \t       \t       \t20\"Soil"
  type27_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t Moistr\t MxGust\t Speed \t  Rad. \t       \t       \tAv Temp"
  type27_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty', 'BatteryVoltage',
                       'AvFuelMoistr', 'DirMxGust', 'MxGustSpeed', 'SolarRad', 'empty', 'empty1', 'SoilAvTemp')
  
  type27_names <- type27_rawNames
  type27_types <- 'cdddddddddddddd'
  
  
  # Type 28
  type28_header <- vector('character',3)
  type28_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\tmbar \t  %  \t Deg \t m/s \t W/m2"
  type28_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t Barom \t  Fuel \t   Dir \tMx Gust\t Solar "
  type28_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t Press \tMoistr\t MxGust\t Speed \t  Rad. "
  type28_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                       'BatteryVoltage', 'BaromPress', 'AvFuelMoistr', 'DirMxGust', 'MxGustSpeed', 'SolarRad')
  
  type28_names <- type28_rawNames
  type28_types <- 'cdddddddddddd'
  
  # Type 29
  type29_header <- vector('character',3)
  type29_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t  %  \t Deg \t m/s \t W/m2\t mm  "
  type29_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\tAv Fuel\t   Dir \tMx Gust\t Solar \t Snow  "
  type29_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t Moistr\t MxGust\t Speed \t  Rad. \t Depth "
  type29_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                       'BatteryVoltage', 'AvFuelMoistr', 'DirMxGust', 'MxGustSpeed', 'SolarRad', 'SnowDepth')
  
  type29_names <- type29_rawNames
  type29_types <- 'cdddddddddddd'

  # Type 30
  type30_header <- vector('character',3)
  type30_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t  %  \t Deg \t m/s \tDeg C\tDeg C\t  %  \t  %  \t W/m2"
  type30_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\tAv Fuel\t   Dir \tMx Gust\t 24 hr \t 24 hr \t 24 hr \t 24 hr \t Solar "
  type30_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t Moistr\t MxGust\t Speed \tMx Air \tMn Air \tMx Rel \tMn Rel \t  Rad. "
  type30_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', 'AvFuelMoistr', 'DirMxGust', 'MxGustSpeed', '24hrMxAir', '24hrMnAir', '24hrMxRel',
                      '24hrMnRel', 'SolarRad')
  
  type30_names <- type30_rawNames
  type30_types <- 'cddddddddddddddd'

  # Type 31
  type31_header <- vector('character',3)
  type31_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\tmbar \t  %  \t Deg \t m/s \tDeg C\t W/m2"
  type31_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t Barom \tAv Fuel\t   Dir \tMx Gust\t4\" Soil\t Solar "
  type31_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t Press \t Moistr\t MxGust\t Speed \tAv Temp\t  Rad. "
  type31_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                       'BatteryVoltage', 'BaromPress', 'AvFuelMoistr', 'DirMxGust', 'MxGustSpeed', 'SoilAvTemp', 'SolarRad')
  
  type31_names <- type31_rawNames
  type31_types <- 'cddddddddddddd'

  # Type 32
  type32_header <- vector('character',3)
  type32_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\tmbar \t Deg \t m/s \tDeg C\tDeg C\tDeg C\t  %  \t  %  \t W/m2"
  type32_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t Barom \t   Dir \tMx Gust\t4\" Soil\t 12 hr \t 12 hr \t 12 hr \t 12 hr \t Solar "
  type32_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t Press \t MxGust\t Speed \tAv Temp\tMx Air \tMn Air \tMx Rel \tMn Rel \t  Rad. "
  type32_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty', 
                      'BatteryVoltage', 'BaromPress', 'DirMxGust', 'MxGustSpeed', 'SoilAvTemp', '12hrMxAir', '12hrMnAir',
                      '12hrMxRel', '12hrMnRel', 'SolarRad')
  
  type32_names <- type32_rawNames
  type32_types <- 'cdddddddddddddddd' 

  # Type 33
  type33_header <- vector('character',3)
  type33_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t Deg \t m/s \t mm  \t W/m2"
  type33_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t   Dir \tMx Gust\t#2 Rain\t Solar "
  type33_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t MxGust\t Speed \t Gauge \t  Rad. "
  type33_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', 'DirMxGust', 'MxGustSpeed', '2RainGauge', 'SolarRad')
  
  type33_names <- type33_rawNames
  type33_types <- 'cddddddddddd'

  # Type 34
  type34_header <- vector('character',3)
  type34_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\tmbar \t Deg \t m/s \t W/m2"
  type34_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t Barom \t   Dir \tMx Gust\t Solar "
  type34_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t Press \t MxGust\t Speed \t  Rad. "
  type34_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'RelHumidty', 'BatteryVoltage',
                      'BaromPress', 'DirMxGust', 'MxGustSpeed', 'SolarRad')
  
  type34_names <- type34_rawNames
  type34_types <- 'cdddddddddd'

  # Type 35
  type35_header <- vector('character',3)
  type35_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t Deg \t m/s \t  %  \t W/m2\t VWC \tDeg C"
  type35_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t   Dir \tMx Gust\tAv Fuel\t Solar \tSoil M \t8\" Soil"
  type35_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t MxGust\t Speed \t Moistr\t  Rad. \t@ 8 in.\tAv Temp"
  type35_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', 'DirMxGust', 'MxGustSpeed', 'AvFuelMoistr', 'SolarRad', 'SoilM8in', 'SoilAvTemp')
  
  type35_names <- type35_rawNames
  type35_types <- 'cddddddddddddd'

  # Type 36
  type36_header <- vector('character',3)
  type36_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\tmbar \t  %  \tDeg C\t Deg \t m/s \t W/m2"
  type36_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t Barom \tAv Fuel\t4\" Soil\t   Dir \tMx Gust\t Solar "
  type36_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t Press \t Moistr\tAv Temp\t MxGust\t Speed \t  Rad. "
  type36_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', 'BaromPress', 'AvFuelMoistr', 'SoilAvTemp', 'DirMxGust', 'MxGustSpeed', 'SolarRad')
  
  type36_names <- type36_rawNames
  type36_types <- 'cddddddddddddd'

  # Type 37
  type37_header <- vector('character',3)
  type37_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\tmbar \t  %  \t Deg \t m/s \tDeg C\t W/m2"
  type37_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t Barom \t  Fuel \t   Dir \tMx Gust\t4\" Soil\t Solar "
  type37_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t Press \tMoistr\t MxGust\t Speed \tAv Temp\t  Rad. "
  type37_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', 'BaromPress', 'AvFuelMoistr', 'DirMxGust', 'MxGustSpeed', 'SoilAvTemp', 'SolarRad')
  
  type37_names <- type37_rawNames
  type37_types <- 'cddddddddddddd'

  # Type 38
  type38_header <- vector('character',3)
  type38_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t Deg \t m/s \t  %  \t W/m2\t VWC \tDeg C"
  type38_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t   Dir \tMx Gust\t  Fuel \t Solar \tSoil M \t8\" Soil"
  type38_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t MxGust\t Speed \tMoistr\t  Rad. \t@ 8 in.\tAv Temp"
  type38_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', 'DirMxGust', 'MxGustSpeed', 'AvFuelMoistr', 'SolarRad', 'SoilM8in', 'SoilAvTemp')
  
  type38_names <- type38_rawNames
  type38_types <- 'cddddddddddddd'

  # Type 39
  type39_header <- vector('character',3)
  type39_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t Deg \t m/s \tDeg C\tDeg C\t  %  \t  %  \t     \t     \tDeg C\t     \t W/m2"
  type39_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t   Dir \tMx Gust\t 12 hr \t 12 hr \t 12 hr \t 12 hr \t       \t       \t       \t       \t Solar "
  type39_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t MxGust\t Speed \tMx Air \tMn Air \tMx Rel \tMn Rel \t       \t       \t       \t       \t  Rad. "
  type39_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', 'DirMxGust', 'MxGustSpeed', '12hrMxAir', '12hrMnAir', '12hrMxRel', '12hrMxRel',
                      'empty1', 'empty2', 'empty3', 'empty4', 'SolarRad')
  
  type39_names <- type39_rawNames
  type39_types <- 'cdddddddddddddddddd'

  # Type 40
  type40_header <- vector('character',3)
  type40_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t  %  \t m/s \t Deg \tDeg C\tDeg C\t  %  \t  %  \t W/m2"
  type40_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\tAv Fuel\tMx Gust\t   Dir \t 24 hr \t 24 hr \t 24 hr \t 24 hr \t Solar "
  type40_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t Moistr\t Speed \t MxGust\tMx Air \tMn Air \tMx Rel \tMn Rel \t  Rad. "
  type40_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', 'AvFuelMoistr', 'MxGustSpeed', 'DirMxGust', '24hrMxAir', '24hrMnAir', '24hrMxRel',
                      '24hrMnRel', 'SolarRad')
  
  type40_names <- type40_rawNames
  type40_types <- 'cddddddddddddddd'

  # Type 41
  type41_header <- vector('character',3)
  type41_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t  %  \t Deg \t m/s \t W/m2\t VWC \tDeg C"
  type41_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t  Fuel \t   Dir \tMx Gust\t Solar \tSoil M \t8\" Soil"
  type41_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\tMoistr\t MxGust\t Speed \t  Rad. \t@ 8 in.\tAv Temp"
  type41_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', 'AvFuelMoistr', 'DirMxGust', 'MxGustSpeed', 'SolarRad', 'SoilM8in', 'SoilAvTemp')
  
  type41_names <- type41_rawNames
  type41_types <- 'cddddddddddddd'

  # Type 42
  type42_header <- vector('character',3)
  type42_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t  %  \tDeg C\t Deg \t m/s \t W/m2"
  type42_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\tSoil M \t4\" Soil\t   Dir \tMx Gust\t Solar "
  type42_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t       \tAv Temp\t MxGust\t Speed \t  Rad. "
  type42_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', 'SoilM', 'SoilTemp', 'DirMxGust', 'MxGustSpeed', 'SolarRad')
  
  type42_names <- type42_rawNames
  type42_types <- 'cdddddddddddd'

  # Type 43
  type43_header <- vector('character',3)
  type43_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t Deg \t m/s \t  %  \t mm  \t W/m2"
  type43_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t   Dir \tMx Gust\t  Fuel \t#2 Rain\t Solar "
  type43_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t MxGust\t Speed \tMoistr\t Gauge \t  Rad. "
  type43_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                      'BatteryVoltage', 'DirMxGust', 'MxGustSpeed', 'AvFuelMoistr', '2RainGauge', 'SolarRad')
  
  type43_names <- type43_rawNames
  type43_types <- 'cdddddddddddd'
  
  # Type 44
  type44_header <- vector('character',3)
  type44_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t  %  \t  %  \tDeg C\t Deg \t m/s \t W/m2"
  type44_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t  Fuel \tSoil M \t4\" Soil\t   Dir \tMx Gust\t Solar "
  type44_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\tMoistr\t       \tAv Temp\t MxGust\t Speed \t  Rad. "
  type44_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                       'BatteryVoltage', 'AvFuelMoistr', 'SoilM', 'SoilTemp', 'DirMxGust', 'MxGustSpeed', 'SolarRad')
  
  type44_names <- type44_rawNames
  type44_types <- 'cddddddddddddd'
  
  # Type 45
  type45_header <- vector('character',3)
  type45_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t Deg \t m/s \t W/m2\t  %  \tDeg C"
  type45_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t   Dir \tMx Gust\t Solar \tSoil M \t  Soil "
  type45_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t MxGust\t Speed \t  Rad. \t       \t Temp 1"
  type45_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                       'BatteryVoltage', 'DirMxGust', 'MxGustSpeed', 'SolarRad', 'SoilM', 'SoilTemp1')
  
  type45_names <- type45_rawNames
  type45_types <- 'cdddddddddddd'
  
  # Type 46
  type46_header <- vector('character',3)
  type46_header[1] <- ":       LST\t mm  \t m/s \t Deg \tDeg C\tDeg C\t  %  \tvolts\t W/m2\t Deg \t m/s \t  %  \tDeg C"
  type46_header[2] <- ": Date/Time\t Precip\t  Wind \t Wind  \t Av Air\t  Fuel \t  Rel  \tBattery\t Solar \t   Dir \tMx Gust\tSoil M \t  Soil "
  type46_header[3] <- ":YYMMDDhhmm\t       \t  Speed\t Direc \t  Temp \t  Temp \tHumidty\tVoltage\t  Rad. \t MxGust\t Speed \t       \t Temp 1"
  type46_rawNames <- c('LST_datestamp', 'Precip', 'WindSpeed', 'WindDirec', 'AvAirTemp', 'FuelTemp', 'RelHumidty',
                       'BatteryVoltage', 'SolarRad', 'DirMxGust', 'MxGustSpeed', 'SoilM', 'SoilTemp1')
  
  type46_names <- type46_rawNames
  type46_types <- 'cdddddddddddd'
  

  # ----- Extract  header lines from the incoming fileString -------------------
  
  # NOTE:  Here are some example headers from WRCC ASCII output:
  # NOTE:
  # NOTE:  [1] "  Enumclaw  Washington "
  # NOTE:  [2] ":       LST	 mm  	 m/s 	 Deg 	Deg C	Deg C	  %  	volts	  %  	 Deg 	 m/s 	 W/m2"
  # NOTE:  [3] ": Date/Time	 Precip	  Wind 	 Wind  	 Av Air	  Fuel 	  Rel  	Battery	Av Fuel	   Dir 	Mx Gust	 Solar "
  # NOTE:  [4] ":YYMMDDhhmm	       	  Speed	 Direc 	  Temp 	  Temp 	Humidty	Voltage	 Moistr	 MxGust	 Speed 	  Rad. "
  # NOTE:
  # NOTE:  [1] " Sullivan  Indiana "
  # NOTE:  [2] ":       LST	 mm  	 m/s 	 Deg 	Deg C	  %  	volts	 Deg 	 m/s 	 W/m2"
  # NOTE:  [3] ": Date/Time	 Precip	  Wind 	 Wind  	 Av Air	  Rel  	Battery	   Dir 	Mx Gust	 Solar "
  # NOTE:  [4] ":YYMMDDhhmm	       	  Speed	 Direc 	  Temp 	Humidty	Voltage	 MxGust	 Speed 	  Rad. "
  
  lines <- readr::read_lines(fileString)
  
  # Strip spaces from the beginning and end but retain "\t" (This is why we can't use stringr::str_trim)
  lines <- stringr::str_replace(lines, '^ *', '')
  
  # Extract the header
  header <- lines[2:4]
  
  # ----- Assign the monitor type ----------------------------------------------
  
  # Default to "UNKONWN" type of monitor
  monitorType <- "UNKNOWN"
  rawNames <- vector('character')
  columnNames <- vector('character')
  columnTypes <- vector('character')
  
  # Test the header against known headers to determine the type
  if ( all(header == type1_header) ) {
    monitorType <- "WRCC_TYPE1"
    rawNames <- type1_rawNames
    columnNames <- type1_names
    columnTypes <- type1_types
  } else if ( all(header == type2_header) ) {
    monitorType <- "WRCC_TYPE2"
    rawNames <- type2_rawNames
    columnNames <- type2_names
    columnTypes <- type2_types
  } else if ( all(header == type3_header) ) {
    monitorType <- "WRCC_TYPE3"
    rawNames <- type3_rawNames
    columnNames <- type3_names
    columnTypes <- type3_types
  } else if ( all(header == type4_header) ) {
    monitorType <- "WRCC_TYPE4"
    rawNames <- type4_rawNames
    columnNames <- type4_names
    columnTypes <- type4_types
  } else if ( all(header == type5_header) ) {
    monitorType <- "WRCC_TYPE5"
    rawNames <- type5_rawNames
    columnNames <- type5_names
    columnTypes <- type5_types
  } else if ( all(header == type6_header) ) {
    monitorType <- "WRCC_TYPE6"
    rawNames <- type6_rawNames
    columnNames <- type6_names
    columnTypes <- type6_types
  } else if ( all(header == type7_header) ) {
    monitorType <- "WRCC_TYPE7"
    rawNames <- type7_rawNames
    columnNames <- type7_names
    columnTypes <- type7_types
  } else if ( all(header == type8_header) ) {
    monitorType <- "WRCC_TYPE8"
    rawNames <- type8_rawNames
    columnNames <- type8_names
    columnTypes <- type8_types
  } else if ( all(header == type9_header) ) {
    monitorType <- "WRCC_TYPE9"
    rawNames <- type9_rawNames
    columnNames <- type9_names
    columnTypes <- type9_types
  } else if ( all(header == type10_header) ) {
    monitorType <- "WRCC_TYPE10"
    rawNames <- type10_rawNames
    columnNames <- type10_names
    columnTypes <- type10_types
  } else if ( all(header == type11_header) ) {
    monitorType <- "WRCC_TYPE11"
    rawNames <- type11_rawNames
    columnNames <- type11_names
    columnTypes <- type11_types
  } else if ( all(header == type12_header) ) {
    monitorType <- "WRCC_TYPE12"
    rawNames <- type12_rawNames
    columnNames <- type12_names
    columnTypes <- type12_types
  } else if ( all(header == type13_header) ) {
    monitorType <- "WRCC_TYPE13"
    rawNames <- type13_rawNames
    columnNames <- type13_names
    columnTypes <- type13_types
  } else if ( all(header == type14_header) ) {
    monitorType <- "WRCC_TYPE14"
    rawNames <- type14_rawNames
    columnNames <- type14_names
    columnTypes <- type14_types
  } else if ( all(header == type15_header) ) {
    monitorType <- "WRCC_TYPE15"
    rawNames <- type15_rawNames
    columnNames <- type15_names
    columnTypes <- type15_types
  } else if ( all(header == type16_header) ) {
    monitorType <- "WRCC_TYPE16"
    rawNames <- type16_rawNames
    columnNames <- type16_names
    columnTypes <- type16_types
  } else if ( all(header == type17_header) ) {
    monitorType <- "WRCC_TYPE17"
    rawNames <- type17_rawNames
    columnNames <- type17_names
    columnTypes <- type17_types
  } else if ( all(header == type18_header) ) {
    monitorType <- "WRCC_TYPE18"
    rawNames <- type18_rawNames
    columnNames <- type18_names
    columnTypes <- type18_types
  } else if ( all(header == type19_header) ) {
    monitorType <- "WRCC_TYPE19"
    rawNames <- type19_rawNames
    columnNames <- type19_names
    columnTypes <- type19_types
  } else if ( all(header == type20_header) ) {
    monitorType <- "WRCC_TYPE20"
    rawNames <- type20_rawNames
    columnNames <- type20_names
    columnTypes <- type20_types
  } else if ( all(header == type21_header) ) {
    monitorType <- "WRCC_TYPE21"
    rawNames <- type21_rawNames
    columnNames <- type21_names
    columnTypes <- type21_types
  } else if ( all(header == type22_header) ) {
    monitorType <- "WRCC_TYPE22"
    rawNames <- type22_rawNames
    columnNames <- type22_names
    columnTypes <- type22_types
  } else if ( all(header == type23_header) ) {
    monitorType <- "WRCC_TYPE23"
    rawNames <- type23_rawNames
    columnNames <- type23_names
    columnTypes <- type23_types
  } else if ( all(header == type24_header) ) {
    monitorType <- "WRCC_TYPE24"
    rawNames <- type24_rawNames
    columnNames <- type24_names
    columnTypes <- type24_types
  } else if ( all(header == type25_header) ) {
    monitorType <- "WRCC_TYPE25"
    rawNames <- type25_rawNames
    columnNames <- type25_names
    columnTypes <- type25_types
  } else if ( all(header == type26_header) ) {
    monitorType <- "WRCC_TYPE26"
    rawNames <- type26_rawNames
    columnNames <- type26_names
    columnTypes <- type26_types
  } else if ( all(header == type27_header) ) {
    monitorType <- "WRCC_TYPE27"
    rawNames <- type27_rawNames
    columnNames <- type27_names
    columnTypes <- type27_types
  } else if ( all(header == type28_header) ) {
    monitorType <- "WRCC_TYPE28"
    rawNames <- type28_rawNames
    columnNames <- type28_names
    columnTypes <- type28_types
  } else if ( all(header == type29_header) ) {
    monitorType <- "WRCC_TYPE29"
    rawNames <- type29_rawNames
    columnNames <- type29_names
    columnTypes <- type29_types
  } else if ( all(header == type30_header) ) {
    monitorType <- "WRCC_TYPE30"
    rawNames <- type30_rawNames
    columnNames <- type30_names
    columnTypes <- type30_types
  } else if ( all(header == type31_header) ) {
    monitorType <- "WRCC_TYPE31"
    rawNames <- type31_rawNames
    columnNames <- type31_names
    columnTypes <- type31_types
  } else if ( all(header == type32_header) ) {
    monitorType <- "WRCC_TYPE32"
    rawNames <- type32_rawNames
    columnNames <- type32_names
    columnTypes <- type32_types
  } else if ( all(header == type33_header) ) {
    monitorType <- "WRCC_TYPE33"
    rawNames <- type33_rawNames
    columnNames <- type33_names
    columnTypes <- type33_types
  } else if ( all(header == type34_header) ) {
    monitorType <- "WRCC_TYPE34"
    rawNames <- type34_rawNames
    columnNames <- type34_names
    columnTypes <- type34_types
  } else if ( all(header == type35_header) ) {
    monitorType <- "WRCC_TYPE35"
    rawNames <- type35_rawNames
    columnNames <- type35_names
    columnTypes <- type35_types
  } else if ( all(header == type36_header) ) {
    monitorType <- "WRCC_TYPE36"
    rawNames <- type36_rawNames
    columnNames <- type36_names
    columnTypes <- type36_types
  } else if ( all(header == type37_header) ) {
    monitorType <- "WRCC_TYPE37"
    rawNames <- type37_rawNames
    columnNames <- type37_names
    columnTypes <- type37_types
  } else if ( all(header == type38_header) ) {
    monitorType <- "WRCC_TYPE38"
    rawNames <- type38_rawNames
    columnNames <- type38_names
    columnTypes <- type38_types
  } else if ( all(header == type39_header) ) {
    monitorType <- "WRCC_TYPE39"
    rawNames <- type39_rawNames
    columnNames <- type39_names
    columnTypes <- type39_types
  } else if ( all(header == type40_header) ) {
    monitorType <- "WRCC_TYPE40"
    rawNames <- type40_rawNames
    columnNames <- type40_names
    columnTypes <- type40_types
  } else if ( all(header == type41_header) ) {
    monitorType <- "WRCC_TYPE41"
    rawNames <- type41_rawNames
    columnNames <- type41_names
    columnTypes <- type41_types
  } else if ( all(header == type42_header) ) {
    monitorType <- "WRCC_TYPE42"
    rawNames <- type42_rawNames
    columnNames <- type42_names
    columnTypes <- type42_types
  } else if ( all(header == type43_header) ) {
    monitorType <- "WRCC_TYPE43"
    rawNames <- type43_rawNames
    columnNames <- type43_names
    columnTypes <- type43_types
  } else if ( all(header == type44_header) ) {
    monitorType <- "WRCC_TYPE44"
    rawNames <- type44_rawNames
    columnNames <- type44_names
    columnTypes <- type44_types
  } else if ( all(header == type45_header) ) {
    monitorType <- "WRCC_TYPE45"
    rawNames <- type45_rawNames
    columnNames <- type45_names
    columnTypes <- type45_types
  } else if ( all(header == type46_header) ) {
    monitorType <- "WRCC_TYPE46"
    rawNames <- type46_rawNames
    columnNames <- type46_names
    columnTypes <- type46_types
  }
  
  # ----- Return ---------------------------------------------------------------
  
  monitorTypeList <- list(
    monitorType = monitorType,
    rawNames = rawNames,
    columnNames = columnNames,
    columnTypes = columnTypes
  )
  
  return(monitorTypeList)
  
}
