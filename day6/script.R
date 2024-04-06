# https://data-explorer.oecd.org/vis?fs[0]=Topic%2C1%7CInnovation%20and%20technology%23INT%23%7CBibliometric%20indicators%23INT_BIB%23&pg=0&fc=Topic&bp=true&snb=3&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_BIBLIO%40DF_BIBLIO_COLLAB&df[ag]=OECD.STI.STP&df[vs]=1.0&pd=2013%2C2017&dq=.COLLAB...&ly[rw]=REF_AREA&ly[cl]=COUNTERPART_AREA&to[TIME_PERIOD]=false&vw=tb
setwd("~/Insync/thhaase.soz@gmail.com/GoogleDrive/_1_Projects/30DayChartChallenge_2024/day6")
library(tidyverse)
library(igraph)
library(ggforce)
library(ggraph)
library(grid)
library(scales)

# Load data
data <- read_csv("OECD.Scientific.Collabs.csv") %>% 
  select(-STRUCTURE, -STRUCTURE_ID, -STRUCTURE_NAME,
         - ACTION, -MEASURE, -Measure, -UNIT_MEASURE,
         -`Unit of measure`, -ASJC, 
         -`All Science Journal Classification`, 
         -`Time period`, -`Observation Value`, -DECIMALS, 
         -Decimals, -UNIT_MULT, -`Unit multiplier`) %>% 
  filter(REF_AREA != "EU27") %>% 
  filter(COUNTERPART_AREA != "EU27")

head(data,10)

data_aggregated <- data %>%
  group_by(`Reference area`, `Counterpart area`) %>%
  summarise(Total_OBS_VALUE = sum(OBS_VALUE), .groups = 'drop') %>% 
  filter(Total_OBS_VALUE > 10000)

get_unique_pair <- function(ref_area, counterpart_area) {
  sort(c(ref_area, counterpart_area))
}

data_aggregated <- data_aggregated %>%
  rowwise() %>%
  mutate(
    area_pair = list(get_unique_pair(`Reference area`, `Counterpart area`))
  ) %>%
  ungroup() %>%
  distinct(area_pair, .keep_all = TRUE) %>%
  select(-area_pair) 




g <- graph_from_data_frame(data_aggregated)

V(g)$name <- V(g)$name
max_weight <- max(E(g)$Total_OBS_VALUE)
min_weight <- min(E(g)$Total_OBS_VALUE)
E(g)$weight_scaled <- (E(g)$Total_OBS_VALUE - min_weight) / (max_weight - min_weight) * 1.9 + 0.1  # Scale between 0.1 and 2
E(g)$alpha_scaled <- (E(g)$Total_OBS_VALUE - min_weight) / (max_weight - min_weight) * 0.8 + 0.2  # Scale between 0.2 and 1

layout <- create_layout(g, layout = 'kk')


ggraph(layout) +
  geom_edge_link(aes(width = weight_scaled, alpha = alpha_scaled, color = Total_OBS_VALUE)) +
  scale_edge_color_gradient(low = "blue", high = "red") +
  geom_node_point(color = "darkgray", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE, size = 2.5, 
                 color = "black", check_overlap = TRUE) +
  theme_graph() +
  theme(legend.position = "none") +
  ggtitle("Scientific Collabiration Network between OECD countries") + 
  labs(caption = "Data Source: OECD Science and Technology Indicators",
       subtitle = "> 10.000 Scientific Collaborations Between 2013 - 2017")

ggsave("plot.png", width = 8, height = 5, dpi = 600)
