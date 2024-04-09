setwd("~/Insync/thhaase.soz@gmail.com/GoogleDrive/_1_Projects/30DayChartChallenge_2024/day7")
library(tidyverse)
library(maps)
library(gridExtra)

pirate_attacks <- read.csv("pirate_attacks.csv")

# subset pipe 1993-2000
pa1 <- pirate_attacks %>% 
  filter(date >= "1993-01-01" & date <= "2000-12-31")
pa2 <- pirate_attacks %>% 
  filter(date >= "2001-01-01" & date <= "2010-12-31")
pa3 <- pirate_attacks %>%
  filter(date >= "2011-01-01" & date <= "2020-12-31")

world_map <- map_data("world")

myplot <- function(mydata){
  ggplot() +
    geom_polygon(data = world_map, 
                 aes(x = long,y = lat, 
                     group = group), 
                 fill = "grey", 
                 color = "grey") +
    geom_density_2d(data = mydata, 
                    aes(x = longitude, y = latitude), 
                    color = "red", 
                    size = 0.4) +
    geom_point(data = mydata, 
               aes(x = longitude, y = latitude), 
               color = "blue", 
               size = 0.2, alpha = 0.5) +
    labs(title = "",
         subtitle = paste0("Years: ", year(min(mydata$date)), " - ", year(max(mydata$date))),
         x = "", y = "") +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          plot.margin = unit(c(-0.5, -0.5, -0.5, -0.5), "cm"),
          plot.subtitle = element_text(hjust = 0.5))+
    coord_fixed(xlim = range(world_map$long, na.rm = TRUE), 
                ylim = range(world_map$lat, na.rm = TRUE), 
                ratio = 1/1)
}

plot_1 <- myplot(pa1)
plot_2 <- myplot(pa2)
plot_3 <- myplot(pa3)
plot_all <- myplot(pirate_attacks)

grid <- grid.arrange(
  plot_all,
  arrangeGrob(plot_1, plot_2, plot_3, ncol=3),
  nrow = 2,
  heights = c(2, 1), 
  top = textGrob("Pirate Attacks", gp=gpar(fontsize=20, font=2)), 
  bottom = textGrob("Benden, P., Feng, A., Howell, C., & Dalla Riva G. V. (2021). \nCrime at Sea: A Global Database of Maritime Pirate Attacks (1993–2020). \nJournal of Open Humanities Data, 7: 19, pp. 1–6. DOI: https://doi.org/10.5334/johd.39", 
                    gp=gpar(fontsize=6))
)
grid

ggsave("plot.png",plot = grid, width = 9, height = 6, dpi = 300)
