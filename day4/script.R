# #install.packages("waffle")
library(tidyverse)
library(waffle)
library(RColorBrewer)

# load data ---- 
data <- read.csv("messier_catalog.csv")

object_types <- c(
  Oc = "Open Cluster",
  Gc = "Globular Cluster",
  Pl = "Planetary Nebula",
  Di = "Diffuse Nebula",
  As = "Asterism",
  Ds = "Double Star",
  MW = "Milky Way Patch",
  Sp = "Spiral Galaxy",
  Ba = "Barred Galaxy",
  Ln = "Lenticular Galaxy",
  El = "Elliptical Galaxy",
  Ir = "Irregular Galaxy",
  Sn = "Supernova Remnant"
)


# cleaningpipe ----
data <- data %>%
  rename(
    CatalogNumber = M,
    Magnitude = Mag.,
    Size = Sizearcmin,
    DistanceLY = Distance..ly.,
    RightAscension = RightAscension,
    Declination = Declination,
    Constellation = Con,
    BestViewingSeason = Viewing.Season,
    CommonName = common_name
  ) %>%
  mutate(
    Type = object_types[Type],  # Replace abbreviations with full names using the named vector
    SizeWidth = str_extract(Size, "^[^x]*"),  # Extract width before 'x'
    SizeHeight = str_extract(Size, "(?<=x).*$"),  # Extract height after 'x'
    DistanceLY = as.numeric(DistanceLY),  # Convert distance to numeric
    # Extract and convert RA and Dec into a more usable numeric format
    RAHour = as.numeric(str_extract(RightAscension, "^[^h]*")),
    RAMinute = as.numeric(str_extract(str_extract(RightAscension, "[0-9]+\\.[0-9]+m$"), "[0-9]+\\.[0-9]+")),
    DecDegrees = as.numeric(str_extract(Declination, "^[^-+]*")),
    DecMinutes = as.numeric(str_extract(str_extract(Declination, "[0-9]+′$"), "[0-9]+"))
  ) %>%
  select(-Size, -RightAscension, -Declination) %>% 
  mutate(
    grob_type = case_when(
      Type %in% c("Spiral Galaxy", "Barred Galaxy", "Lenticular Galaxy", "Elliptical Galaxy", "Irregular Galaxy") ~ "Galaxies",
      Type %in% c("Open Cluster", "Globular Cluster") ~ "Clusters",
      Type %in% c("Planetary Nebula", "Diffuse Nebula", "Supernova Remnant") ~ "Nebulae",
      TRUE ~ as.character(Type)
    )
  )



# plot ----


grob_type_counts <- data %>%
  count(grob_type, sort = TRUE) %>%
  drop_na() 

space_colors <- c("#6a5acd", "#4169e1", "#9370db", "#8a2be2", "#483d8b", "#4b0082", "#9400d3", "#8b008b", "#800080", "#0000ff", "#00008b")


if (n_distinct(grob_type_counts$grob_type) > length(space_colors)) {
  space_colors <- colorRampPalette(space_colors)(n_distinct(grob_type_counts$grob_type))
}

waffle(grob_type_counts, rows = 10, size = 0.5, colors = space_colors) +
  labs(title = "Distribution of Messier Object Categories") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.box = "horizontal") +
  scale_fill_manual(values = space_colors, name = "Object Type", labels = grob_type_counts$grob_type)

