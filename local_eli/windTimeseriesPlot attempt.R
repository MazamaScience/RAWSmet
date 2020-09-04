
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


fw13Meta <-fw13_loadMeta()
data <- fw13_load(nwsID = 451702, meta = fw13Meta) %>% raws_filterDate(20050801, 20050803) %>% raws_extractData()

data$windDirection <- data$windDirection + 90
data$windDirectionDir <- ifelse(90 < data$windDirection & data$windDirection < 270, -data$windDirection, data$windDirection)
data$wdRadians <- ((data$windDirection) * pi /180) %% 2*pi

h = 1

dy = h * sin(data$wdRadians)
dx = h * cos(data$wdRadians)

data$xEnd <- data$datetime + lubridate::minutes(round(dx))
data$yEnd <- data$windSpeed + dy

gg <- ggplot(data, aes(x = datetime, y = windSpeed)) +
  geom_point()+
  geom_segment(aes(x = datetime, y = windSpeed, xend = xEnd, yend = yEnd))
