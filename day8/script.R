setwd("~/Insync/thhaase.soz@gmail.com/GoogleDrive/_1_Projects/30DayChartChallenge_2024/day8")
library(tidyverse)

# create Hessen Sunshine duration Data

jan <- read_csv2("regional_averages_sd_01.txt", skip = 1)
feb <- read_csv2("regional_averages_sd_02.txt", skip = 1)
mar <- read_csv2("regional_averages_sd_03.txt", skip = 1)
apr <- read_csv2("regional_averages_sd_04.txt", skip = 1)
may <- read_csv2("regional_averages_sd_05.txt", skip = 1)
jun <- read_csv2("regional_averages_sd_06.txt", skip = 1)
jul <- read_csv2("regional_averages_sd_07.txt", skip = 1)
aug <- read_csv2("regional_averages_sd_08.txt", skip = 1)
sep <- read_csv2("regional_averages_sd_09.txt", skip = 1)
oct <- read_csv2("regional_averages_sd_10.txt", skip = 1)
nov <- read_csv2("regional_averages_sd_11.txt", skip = 1)
dec <- read_csv2("regional_averages_sd_12.txt", skip = 1)

# fix line
Hessen <- jan %>% 
  select(Jahr, Hessen) %>% 
  rename(jan = Hessen) %>% 
  left_join(feb, by = "Jahr") %>% 
    select(Jahr, jan, Hessen) %>%
    rename(feb = Hessen) %>%
  left_join(mar, by = "Jahr") %>%
    select(Jahr, jan, feb, Hessen) %>%
    rename(mar = Hessen) %>% 
  left_join(apr, by = "Jahr") %>%
    select(Jahr, jan, feb, mar, Hessen) %>%
    rename(apr = Hessen) %>% 
  left_join(may, by = "Jahr") %>%
    select(Jahr, jan, feb, mar, apr, Hessen) %>%
    rename(may = Hessen) %>%
  left_join(jun, by = "Jahr") %>%
    select(Jahr, jan, feb, mar, apr, may, Hessen) %>%
    rename(jun = Hessen) %>%
  left_join(jul, by = "Jahr") %>%
    select(Jahr, jan, feb, mar, apr, may, jun, Hessen) %>%
    rename(jul = Hessen) %>%
  left_join(aug, by = "Jahr") %>%
    select(Jahr, jan, feb, mar, apr, may, jun, jul, Hessen) %>%
    rename(aug = Hessen) %>%
  left_join(sep, by = "Jahr") %>%
    select(Jahr, jan, feb, mar, apr, may, jun, jul, aug, Hessen) %>%
    rename(sep = Hessen) %>%
  left_join(oct, by = "Jahr") %>%
    select(Jahr, jan, feb, mar, apr, may, jun, jul, aug, sep, Hessen) %>%
    rename(oct = Hessen) %>%
  left_join(nov, by = "Jahr") %>%
    select(Jahr, jan, feb, mar, apr, may, jun, jul, aug, sep, oct, Hessen) %>%
    rename(nov = Hessen) %>%
  left_join(dec, by = "Jahr") %>%
    select(Jahr, jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, Hessen) %>%
    rename(dec = Hessen)

rm(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)

# long_data
Hessen_long <- Hessen %>%
  pivot_longer(cols = -Jahr, 
               names_to = "month", 
               values_to = "sunshine") %>%
  mutate(month = factor(month, 
                        levels = c("jan", "feb", "mar",
                                   "apr", "may", "jun",
                                   "jul", "aug", "sep", 
                                   "oct", "nov", "dec"),
                        labels = c("January", "February", "March",
                                   "April", "May", "June",
                                   "July", "August", "September", 
                                   "October", "November", "December")
                        )
         )


Hessen_year <- Hessen_long %>% 
  filter(Jahr == 2019)




ggplot(data = Hessen_year, aes(x = month, y = sunshine, fill = sunshine)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c(option = "plasma") +
  coord_polar() +
  theme_minimal() +
  theme(axis.line.x = element_blank(), 
        axis.line.y = element_blank(),
        axis.text = element_text(size = 10, 
                                 color = "black", 
                                 family = "Noto"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  family = "Noto"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(family = "Noto"),
        panel.background = element_rect(fill = "#fcf0b8", colour = NA),
        plot.background = element_rect(fill = "#fcf0b8", colour = NA),
        panel.grid = element_line(colour = "lightblue")
  ) +
  scale_y_continuous(breaks = NULL, limits = c(0,3300)) +
  theme(legend.position = "none") +
  labs(title = "Sunshine duration in Hessen",
       subtitle = "",
       caption = "Data: https://opendata.dwd.de")








####




make_my_plot <- function(data) {
  ggplot(data = data, aes(x = month, y = sunshine, fill = sunshine)) +
    geom_bar(stat = "identity") +
    scale_fill_viridis_c(option = "plasma") +
    coord_polar() +
    theme_minimal() +
    theme(axis.line.x = element_blank(), 
          axis.line.y = element_blank(),
          axis.text = element_text(size = 10, 
                                   color = "black", 
                                   family = "Noto"),
          plot.title = element_text(hjust = 0.5,
                                    face = "bold",
                                    family = "Noto"),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(family = "Noto"),
          panel.background = element_rect(fill = "#fcf0b8", colour = NA),
          plot.background = element_rect(fill = "#fcf0b8", colour = NA),
          panel.grid = element_line(colour = "lightblue")
    ) +
    scale_y_continuous(breaks = NULL, limits = c(0,3300)) +
    theme(legend.position = "none") +
    labs(title = "Sunshine duration in Hessen",
         subtitle = paste0("Year ", unique(data$Jahr)),
         caption = "Data: https://opendata.dwd.de")
}

range(Hessen$Jahr)

for(i in 1951:2023){
  Hessen_year <- Hessen_long %>% 
    filter(Jahr == i)
  
  make_my_plot(Hessen_year)
  
  ggsave(paste0("plot",i,".png"), width = 6, height = 6, dpi = 300)
}
