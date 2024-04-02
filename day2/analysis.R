library(tidyverse)
library(FactoMineR)
library(factoextra)
#devtools::install_github('thomasp85/ggfx')
library(ggfx)

# load data ----

d1 <- read.csv("film1_sent.csv")
d2 <- read.csv("film2_sent.csv")
d3 <- read.csv("film3_sent.csv")
d4 <- read.csv("film4_sent.csv")
dall <- read.csv("all_sent.csv")

# plot ----


mean_table <- rbind(
  round(c(
    mean(d1$anger),
    mean(d1$anticipation), 
    mean(d1$disgust), 
    mean(d1$fear), 
    mean(d1$joy), 
    mean(d1$sadness), 
    mean(d1$surprise), 
    mean(d1$trust)
  ), 2),
  round(c(
    mean(d2$anger),
    mean(d2$anticipation), 
    mean(d2$disgust), 
    mean(d2$fear), 
    mean(d2$joy), 
    mean(d2$sadness), 
    mean(d2$surprise), 
    mean(d2$trust)
  ), 2),
  round(c(
    mean(d3$anger),
    mean(d3$anticipation), 
    mean(d3$disgust), 
    mean(d3$fear), 
    mean(d3$joy), 
    mean(d3$sadness), 
    mean(d3$surprise), 
    mean(d3$trust)
  ), 2),
  round(c(
    mean(d4$anger),
    mean(d4$anticipation), 
    mean(d4$disgust), 
    mean(d4$fear), 
    mean(d4$joy), 
    mean(d4$sadness), 
    mean(d4$surprise), 
    mean(d4$trust)
  ), 2),
  round(c(
    mean(dall$anger),
    mean(dall$anticipation), 
    mean(dall$disgust), 
    mean(dall$fear), 
    mean(dall$joy), 
    mean(dall$sadness), 
    mean(dall$surprise), 
    mean(dall$trust)
  ), 2)
)

colnames(mean_table) <- c("anger", "anticipation", "disgust", "fear", 
                          "joy", "sadness", "surprise", "trust")

mean_table <- mean_table %>% 
  as.data.frame() %>% 
  slice(-5) 

rownames(mean_table) <- c("Matrix 1", "Matrix 2", "Matrix 3", "Matrix 4")

ca_results <- CA(mean_table, graph = FALSE)


# plot ----

row_coords <- as.data.frame(ca_results$row$coord)
row_coords$label <- rownames(row_coords)



with_outer_glow(
fviz_ca_biplot(ca_results, 
               repel = T, 
               col.row = "green",
               col.col = "white",
               label = "col",
               addEllipses = FALSE,
               arrows = c(F, F)
),colour = "green", sigma = 2, expand = 1.5) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "white") +
  labs(subtitle = "Correspondence Analysis of NRC-Emotions from Letterboxed Comments (n = 16221)",
       x = "Dim 1 (64%)", y = "Dim 2 (31%)") +
  ggtitle("Letterboxed Comments of The Matrix Movies") +
  theme_classic(base_family = "Courier") +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    text = element_text(color = "#00FF00", family = "Courier"),
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.title = element_text(size = 12, color = "#00FF00"),
    axis.text = element_text(size = 8, color = "white"),
    axis.line = element_line(color = "white"),
    axis.ticks = element_line(color = "white"),
    legend.position = "none"
  ) +
  with_outer_glow(
      geom_point(size = 3, color = "#00FF00"),
    colour = "green", sigma = 3, expand = 3 ) +
  with_outer_glow(
      geom_text(data = row_coords, aes(label = label, x = `Dim 1`, y = `Dim 2`), 
              color = "#00FF00", size = 5, vjust = 1.8, hjust = 0.2, family = "Courier"),
    colour = "green", sigma = 2, expand = 2
  )

