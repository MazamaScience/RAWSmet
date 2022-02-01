data <- 
  example_cefa_Saddle_Mountain %>%
  raws_filterDate("2017-09-01", "2017-09-01") %>%
  raws_getData()

plot(data$datetime, data$windSpeed)

addWindBarbs(x = data$datetime,
             y = data$windSpeed,
             speed = data$windSpeed,
             dir = data$windDirection,
             forMap = FALSE,
             barbLocation = "zero",
             col = "red",
             barbSize = 5)

addWindBarbs(x = data$datetime,
             y = data$windSpeed,
             speed = data$windSpeed,
             dir = data$windDirection,
             forMap = FALSE,
             barbLocation = "point",
             col = "blue",
             barbSize = 5)






















dir <- data$windDirection
speed <- data$windSpeed
extraBarbLength <- 0
barbSize <- 1
# Wind direction is measured in degrees clockwise from north
# We want to convert into counter-clockwise from east
dir <- (360 - dir + 90) %% 360

# Get dir in radians

rad <- dir * pi / 180

# Get x and y scale factors
pin <- par("pin")
usr <- par("usr")
xpi <- (usr[2] - usr[1]) / pin[1]
ypi <- (usr[4] - usr[3]) / pin[2]

  x <- as.numeric(x)
  # Get starting and ending points for barb
  xe <- x + (lx + extraBarbLength*lx) * cos(rad)
  ye <- y + (ly + extraBarbLength*ly) * sin(rad)
  
  xe <- as.POSIXct.numeric(xe, origin = "1970-01-01")
  
arrows(x0 = x, y0 = y, x1 = xe, y1 = ye)
