library(tidyverse)

# https://www.dwd.de/DE/leistungen/cdc/cdc_ueberblick-klimadaten.html
# https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/annual/air_temperature_mean/

data <- read.csv("data.csv", sep = ";") %>% 
  select(-X, -Jahr.1)

data_long <- pivot_longer(data, -Jahr, names_to = "Location", values_to = "Value")

ggplot(data_long, aes(x = Jahr, y = Value, color = Location)) + 
  geom_line() +
  scale_color_manual(values = c(rep("blue", length(unique(data_long$Location)) - 1), "Deutschland" = "red")) +
  theme_minimal() +
  labs(title = "Yearly Values by Bundesland and Germany",
       x = "Year",
       y = "Value",
       color = "Location")

data_hessen <- data_long %>% filter(Location == "Hessen")






ggplot(data_hessen, aes(x = Jahr, y = Value)) + 
  geom_point(aes(color = Value), alpha=0.5, size = 3, shape = 19) +  
  geom_smooth(method = "loess", span=0.4, color = "#D55E00", size = 1.2) +
  geom_hline(yintercept = median(data_hessen$Value), color = "#009E73", size = 1) +
  geom_text(aes(x = min(Jahr)+75, y = median(Value)-0.2, label = "Median"), color = "#009E73", hjust = 1.5, size = 5) +
  scale_color_gradient(low = "#56B4E9", high = "red") +
  geom_vline(xintercept = 1986.8, color = "lightgrey") + 
  theme_classic(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.title.x = element_text(size = 16, margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, margin = margin(r = 10))) +
  scale_y_continuous(limits = c(5, 12), breaks = seq(0, 12, 2)) +
  scale_x_continuous(limits = c(1950, 2020), breaks = seq(1950, 2020, 10)) +
  labs(title = "Average Temperature per Year in Hessen",
       subtitle = "Smoothed with LOESS",
       caption = "Data Source: opendata.dwd.de",
       x = "Year",
       y = "Temperature (°C)")

ggsave("plot.png",width = 10,height = 6.5, dpi = 1000)

