library(ggplot2)
library(metR)

cefaMeta <-cefa_loadMeta()

nwsID = "451702"

timezone = cefaMeta$timezone[cefaMeta$nwsID == "451702"]
locationName = cefaMeta$locationName[cefaMeta$nwsID == "451702"]
stateCode = cefaMeta$stateCode[cefaMeta$nwsID == "451702"]

startDate = MazamaCoreUtils::parseDatetime(20050801, timezone = timezone)
endDate = MazamaCoreUtils::parseDatetime(20050803, timezone = timezone)

data <- cefa_load(nwsID = "451702", meta = cefaMeta) %>% raws_filterDate(startDate, endDate) %>% raws_getData()


data$windDirection <- data$windDirection + 90

title = sprintf("Observed Wind at %s, %s %s \n From %s LST to %s LST", nwsID, locationName, stateCode, startDate, endDate)
gg <- ggplot(data, aes(x = datetime, y = windSpeed)) +
  geom_point(aes(y = windSpeed, color = "Winds")) +
  geom_line(aes(y = maxGustSpeed, color = "Gusts")) +
  metR::geom_arrow(aes(mag = 1, angle = windDirection), arrow.ends = "last", show.legend = F) +
  labs(title = title, x = "", y = "Speed in m/s", color = "") +
  scale_color_manual(values = c("blue", "red"),
                     breaks = c("Winds", "Gusts"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("blank", "solid"),
                        shape = c(16, NA)))) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  

