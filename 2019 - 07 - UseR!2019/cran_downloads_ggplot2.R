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
###############################################
############ ggdemetra gif ####################

library(ggdemetra)
p1 <- ggplot(data = ipi_c_eu_df, mapping = aes(x = date, y = FR)) +
  labs(title = "Seasonal adjustment of the French industrial production index",
       x = NULL, y = NULL) +
  geom_line() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    title = element_text(size = 16))
p2 <- p1 +
  geom_sa(component = "y_f", linetype = 2) 
p3 <- p2 + 
  geom_sa(component = "sa", color = "red") 
p4 <- p3 +
  geom_sa(component = "sa_f", color = "red", linetype = 2) 
p5 <- p4 +
  geom_arima(geom = "label", x_arima = -Inf, y_arima = -Inf, 
             vjust = -1, hjust = -0.1) 
p6 <- p5 +
  geom_outlier(geom = "label_repel", coefficients = TRUE,
               vjust = 4, ylim = c(NA, 65), force = 10,
               arrow = arrow(length = unit(0.03, "npc"),
                             type = "closed", ends = "last"))
diagnostics <- c(`Combined test` = "diagnostics.combined.all.summary",
                 `Residual qs-test (p-value)` = "diagnostics.qs",
                 `Residual f-test (p-value)` = "diagnostics.ftest")
p_diag <- ggplot(data = ipi_c_eu_df, mapping = aes(x = date, y = FR)) +
  geom_diagnostics(diagnostics = diagnostics,
                   table_theme = gridExtra::ttheme_default(base_size = 8),
                   message = FALSE) + 
  theme_void()
p7 <- gridExtra::arrangeGrob(p6, p_diag,
                       nrow = 2, heights  = c(4, 1))
all_graphs <- mget(sprintf("p%i", 1:7))
for(i in seq_along(all_graphs)){
  ggsave(filename = sprintf("2019 - 07 - UseR!2019/img/gif/ggdemetra/%i.png", i),
         plot = all_graphs[[i]])
}

