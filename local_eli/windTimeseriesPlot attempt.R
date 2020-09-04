
fw13Meta <-fw13_loadMeta()
data <- fw13_load(nwsID = 451702, meta = fw13Meta) %>% raws_filterDate(20050801, 20050803) %>% raws_extractData()


data$windDirection <- data$windDirection + 90
data$windDirectionDir <- ifelse(90 < data$windDirection & data$windDirection < 270, -data$windDirection, data$windDirection)
data$wdRadians <- ((data$windDirection) * pi /180)

data$dx <- lubridate::minutes(round(data$windDirectionDir))
data$xEnd <- data$datetime + data$dx

data$dy <- sin(data$wdRadians)
data$yEnd <- data$windSpeed + data$dy

gg <- ggplot(data, aes(x = datetime, y = windSpeed)) +
  geom_point(aes(y = windSpeed), color = "blue") + 
  geom_line(aes(y = maxGustSpeed), color = "red") +
  geom_segment(aes(x = datetime, xend = xEnd, y = windSpeed, yend = yEnd), arrow = arrow(length = unit("0.2", "cm")))
