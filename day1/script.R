setwd("~/Downloads/dataverse_files")
library(tidyverse)
library(quanteda) 
library(quanteda.textstats)
library(quanteda.textplots)
library(quanteda.textmodels)
library(striprtf)
library(tidytext)

data <- read.csv("2_Metadataset_Abel_Biograms.csv")
head(data)
names(data)

table(data$Geschlecht)
plot(table(1934 - year(dmy(data$Geburtsdatum))))

median(c(1934 - year(dmy(data$Geburtsdatum))), na.rm = TRUE)

data <- data %>%
  mutate(Alter = 1934 - year(dmy(Geburtsdatum)))

data1 <- data %>%
  filter(Alter < 36)
data2 <- data %>%
  filter(Alter >= 36)

# create 2 corpora ----


read_rtf_to_text <- function(filepath) {
  rtf_content <- read_rtf(filepath)
  return(rtf_content)
}

id_set_1 <- data1$BriefID
id_set_2 <- data2$BriefID

rtf_dir <- "./0_Biograms"
rtf_files <- list.files(path = rtf_dir, pattern = "\\.rtf$", full.names = TRUE)


filter_files_by_id <- function(files, id_set) {
  pattern <- paste0(id_set, collapse = "|") # Create a regex pattern that matches any ID in the set
  matched_files <- grep(pattern, files, value = TRUE)
  return(matched_files)
}

files_set_1 <- filter_files_by_id(rtf_files, id_set_1)
files_set_2 <- filter_files_by_id(rtf_files, id_set_2)
files <- c(files_set_1, files_set_2)

text_set_1 <- sapply(files_set_1, read_rtf_to_text, USE.NAMES = FALSE)
text_set_2 <- sapply(files_set_2, read_rtf_to_text, USE.NAMES = FALSE)
text_set <- c(text_set_1, text_set_2)

corpus_1 <- corpus(unlist(text_set_1))
corpus_2 <- corpus(unlist(text_set_2))
corpus <- corpus(unlist(text_set))
# create dfm ----

toks <- tokens(corpus, remove_punct = TRUE)
dfm <- dfm(toks)
dfm <- dfm_remove(dfm, pattern = stopwords("de"))
dfm <- dfm_trim(dfm, min_termfreq = 200)

kw_hitler <- kwic(toks, pattern =  "hitler")
head(kw_hitler)

kwh <- c(kw_hitler$pre, kw_hitler$post)


# plots ----

tibble_hitler <- tibble(line = 1:length(kwh), text = kwh)
tibble_all <- tibble(line = 1:length(unlist(text_set)), text = unlist(text_set))

german_stopwords <- tibble(word = c(stopwords("de"), "wurde", "dass", "schon", "wurden", "konnte", "deutsche"))

word_counts_hitler <- tibble_hitler %>%
  unnest_tokens(word, text) %>%
  anti_join(german_stopwords, by = "word") %>%
  count(word, sort = TRUE)

word_counts_all <- tibble_all %>%
  unnest_tokens(word, text) %>%
  anti_join(german_stopwords, by = "word") %>%
  count(word, sort = TRUE)

library(tools)
top_hitler <- word_counts_hitler %>%
  top_n(10, wt = n) 
top_all <- word_counts_all %>%
  top_n(10, wt = n)

top_hitler$word <- toTitleCase(top_hitler$word)
top_all$word <- toTitleCase(top_all$word)

# Create the plot
plot_hitler <- ggplot(top_hitler, aes(x = reorder(word, n), y = n, fill = n)) +
  geom_col() +
  scale_fill_gradient(low = "lightblue", high = "navy") +
  coord_flip() +  
  labs(x = "", y = "Frequency", title = 'Words appearing around "hitler"') +
  theme_classic() +
  theme(legend.position = "none")

plot_all <- ggplot(top_all, aes(x = reorder(word, n), y = n, fill = n)) +
  geom_col() +
  scale_fill_gradient(low = "orange", high = "darkred") +
  coord_flip() +  
  labs(x = "", y = "Frequency", title = 'Words of all Biograms') +
  theme_classic() +
  theme(legend.position = "none")


library(patchwork)
plot_grid <- plot_all + plot_hitler

plot_final <- plot_grid + plot_annotation(title = 'Top 10 Words of Nazi Biograms from 1934', caption="Data: Spörlein, Pfuhlmann, Albert and Spörlein. 2020.\n Machine-readable Nazi Biograms originally collected by Theodore Abel.\n Harvard Dataverse, https://doi.org/10.7910/DVN/SQ8DVX", theme = theme(plot.title = element_text(hjust = 0.5)))

# save plot ----

ggsave("top_words.png", plot_final, width = 10, height = 5, dpi = 600)
