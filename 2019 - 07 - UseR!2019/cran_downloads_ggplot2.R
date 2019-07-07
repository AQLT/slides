library(lubridate)
library(dplyr)
Sys.setlocale("LC_TIME", "English")

data <- cranlogs::cran_downloads("ggplot2", from = "2015-01-01")
data$date <- ymd(data$date)
data$year <- year(data$date)
data$month <- month(data$date)
data$quarter <- quarter(data$date)
data$weekday <- wday(data$date, label = TRUE,abbr = FALSE,week_start = 1,
                     locale = "English_United States.1252")
data <- data[!(data$year == 2019 & data$month == 7),]
monthly_data <- data %>%  
  group_by(year, month) %>% 
  summarise(downloads = sum(count))
wday_data <- data %>%  
  group_by(weekday) %>% 
  summarise(downloads = round(mean(count)))
colnames(wday_data) <- c("Weekday", "CRAN downloads (/1000)")

monthly_ts <- ts(monthly_data$downloads,start=2015, frequency = 12)
monthly_ts <- window(monthly_ts, 2016)/1000
dataGraph <- data.frame(date = as.numeric(time(monthly_ts)),
                        value = as.numeric(monthly_ts))
saveRDS(dataGraph, "2019 - 07 - UseR!2019/ggplot2.RDS")
ggplot(data = dataGraph, aes(x = date, y = value)) +
  geom_line(size=0.70) + theme_bw() + 
  labs(x = NULL, y = "Monthly downloads (/1000)",
       title = "ggplot2") +
  scale_y_continuous(labels = function(x) format(x))
knitr::kable(wday_data, booktabs = TRUE, format = "latex",
             caption = "Number of CRAN downloads of ggplot2 per weekday",
             linesep = "")

