library(magick)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(grid)

m1 <- image_read("moon1.png")
m2 <- image_read("moon2.png")

m1g <- image_convert(m1, 'gray')
m2g <- image_convert(m2, 'gray')

m1_data <- as.integer(image_data(m1g))
m2_data <- as.integer(image_data(m2g))

m1_dims <- dim(m1_data)
m2_dims <- dim(m2_data)

m1mat <- matrix(m1_data, nrow = m1_dims[1], ncol = m1_dims[2], byrow = TRUE)
m2mat <- matrix(m2_data, nrow = m2_dims[1], ncol = m2_dims[2], byrow = TRUE)


# Function to shift the zero-frequency component to the center of the spectrum
fftshift <- function(mat) {
  return(matrix(data = mat, nrow = nrow(mat), ncol = ncol(mat))[c((nrow(mat)/2+1):nrow(mat), 1:(nrow(mat)/2)), c((ncol(mat)/2+1):ncol(mat), 1:(ncol(mat)/2))])
}

# Perform Fourier Transform on both images
fft_m1 <- fft(m1mat)
fft_m2 <- fft(m2mat)

# Shift the zero-frequency component to the center
fft_m1_shifted <- fftshift(Mod(fft_m1))
fft_m2_shifted <- fftshift(Mod(fft_m2))









fft_m1_vector <- as.vector(fft_m1_shifted)
fft_m2_vector <- as.vector(fft_m2_shifted)

# Take a sample from the larger vector if they are of different lengths
if(length(fft_m1_vector) > length(fft_m2_vector)) {
  fft_m1_vector <- sample(fft_m1_vector, length(fft_m2_vector))
} else {
  fft_m2_vector <- sample(fft_m2_vector, length(fft_m1_vector))
}

data <- data.frame(
  Intensity = c(fft_m1_vector, fft_m2_vector),
  Image = rep(c("Old Moon Image", "New Moon Image"), each=length(fft_m1_vector))
)
data$IntensityLog <- log1p(data$Intensity)  # log1p is used to avoid log(0)

annot <- data %>%
  group_by(Image) %>%
  summarize(
    x = mean(IntensityLog), 
    y = max(density(IntensityLog)$y)
  ) %>%
  mutate(label = case_when(
    Image == "Old Moon Image" ~ "Old Moon Image",
    Image == "New Moon Image" ~ "New Moon Image",
    TRUE ~ as.character(Image)  # Handles cases with more than two groups
  ))

# Fix the color assignments
colors <- c("Old Moon Image" = "#DC143C", "New Moon Image" = "#FFD700")

# Create the plot
p <- ggplot(data, aes(x=IntensityLog, color=Image, fill=Image)) +
  geom_density(alpha=0.6) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  geom_text(data=annot, aes(x=x, y=y, 
                            label=label, 
                            color=Image), 
            hjust = -0.07,
            vjust = 2,
            size = 4.5) +
  theme_ipsum(base_size = 14, base_family = "Helvetica") +
  theme(
    legend.position="none",
    plot.background = element_rect(fill = "#001040"), 
    panel.background = element_rect(fill = "#001040"),
    text = element_text(color = "#ffffff"),
    axis.text = element_text(color = "#ffffff"),
    axis.title = element_text(color = "#ffffff"),
    plot.title = element_text(color = "#ffffff"),
    plot.subtitle = element_text(color = "#ffffff"),
    panel.grid.major = element_line(color = "#ffffff", size = 0.2),
    panel.grid.minor = element_line(color = "#ffffff", size = 0.1),
    strip.background = element_rect(fill = "#001040"),
    strip.text = element_text(color = "#ffffff"),
    plot.caption = element_text(face = "plain"),
  ) + 
  labs(title= "Moon Picture Comparison", 
       subtitle="Log Density of Frequency Magnitudes from Fourier Transformation", 
       x="Log(Magnitude)", 
       y="Density",
       caption="HOW TO INTERPRET: \nhigher peak --> more high frequencies --> more details/edges \nwider distribution --> variation in frequencies --> more structure/texture \nmore overlap --> more similar frequencies --> higher similarity")

print(p)






# Convert to raster objects
moon_grob1 <- rasterGrob(as.raster(m1), interpolate=TRUE, width=unit(0.8, "inches"), height=unit(0.8, "inches"))
moon_grob2 <- rasterGrob(as.raster(m2), interpolate=TRUE, width=unit(0.8, "inches"), height=unit(0.8, "inches"))

# Create your plot
p <- p

# Add annotations for density peaks as placeholders for image positions
annot_img <- data %>%
  group_by(Image) %>%
  summarize(x = mean(IntensityLog), y = max(density(IntensityLog)$y))


x_offset1 <- 5
y_offset1 <- -0.15  

x_offset2 <- 5  
y_offset2 <- -0.15 

# save with width=894&height=548
picture_multiplicator <- 6.6

png("moon_comparison.png", width=894*picture_multiplicator, height=548*picture_multiplicator, units = "px", res = 600)

p + 
  annotation_custom(grob = moon_grob1, 
                    xmin = annot$x[1] + x_offset1, 
                    xmax = annot$x[1] + x_offset1 + 0.1,  
                    ymin = annot$y[1] + y_offset1, 
                    ymax = annot$y[1] + y_offset1 + 0.2) +  
  annotation_custom(grob = moon_grob2, 
                    xmin = annot$x[2] + x_offset2, 
                    xmax = annot$x[2] + x_offset2 + 0.1,  
                    ymin = annot$y[2] + y_offset2, 
                    ymax = annot$y[2] + y_offset2 + 0.2)  

dev.off()

