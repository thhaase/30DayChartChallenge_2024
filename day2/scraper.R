library(rvest)
library(tidyverse)

scrape_page <- function(page_number, letterboxed_url) {
  url <- paste0(letterboxed_url,"page/", page_number, "/")
  page <- read_html(url)
  
  name_data <- page %>% html_elements(".name") %>% html_text(trim = TRUE)
  comment_data <- page %>% html_elements(".collapsible-text p") %>% html_text(trim = TRUE)
  date_data <- page %>% html_elements("._nobr") %>% html_text(trim = TRUE)
  rating_data <- page %>% html_elements(".-green") %>% html_text(trim = TRUE)
  
  # Ensure all vectors have the same length by using NA for missing elements
  max_length <- max(length(name_data), 
                    length(comment_data), 
                    length(date_data), 
                    length(rating_data))
  length(name_data) <- max_length
  length(comment_data) <- max_length
  length(date_data) <- max_length
  length(rating_data) <- max_length
  
  data.frame(name_data, comment_data, date_data, rating_data)
}

extract_and_replace_stars <- function(rating_text) {
  stars_count <- nchar(gsub("[^★]", "", rating_text))
  stars_count_text <- paste0(stars_count)
  return(stars_count_text)
}


################################################################################
# Script start ----

## setting parameters ----

# global letterboxed pagenumber limit
num_pages <- 256

url_1 <- "https://letterboxd.com/film/the-matrix/reviews/"
df_1 <- data.frame()

url_2 <- "https://letterboxd.com/film/the-matrix-reloaded/reviews/"
df_2 <- data.frame()

url_3 <- "https://letterboxd.com/film/the-matrix-revolutions/reviews/"
df_3 <- data.frame()

url_3 <- "https://letterboxd.com/film/the-matrix-resurrections/reviews/"
df_3 <- data.frame()

## scraping pages ----

### film 1 ---- 
for (page_number in 1:num_pages) {
  cat("Scraping page:", page_number, "\n")
  scraped_data <- scrape_page(page_number, url_1)
  df_1 <- rbind(df_1, scraped_data)
}

# cleaning
df_1 <- df_1 %>%
  rename(name = name_data,comment = comment_data,
         date = date_data,rating = rating_data)

# replace stars
df_1$rating <- as.numeric(lapply(df_1$rating, extract_and_replace_stars))

write.csv(df_1, file = "./film1.csv", row.names = FALSE)




### film 2 ----

for (page_number in 1:num_pages) {
  cat("Scraping page:", page_number, "\n")
  scraped_data <- scrape_page(page_number, url_2)
  df_2 <- rbind(df_2, scraped_data)
}

# cleaning
df_2 <- df_2 %>%
  rename(name = name_data,comment = comment_data,
         date = date_data,rating = rating_data)

# replace stars
df_2$rating <- as.numeric(lapply(df_2$rating, extract_and_replace_stars))

write.csv(df_2, file = "./film2.csv", row.names = FALSE)




### film 3 ----

for (page_number in 1:num_pages) {
  cat("Scraping page:", page_number, "\n")
  scraped_data <- scrape_page(page_number, url_3)
  df_3 <- rbind(df_3, scraped_data)
}

# cleaning
df_3 <- df_3 %>%
  rename(name = name_data,comment = comment_data,
         date = date_data,rating = rating_data)

# replace stars
df_3$rating <- as.numeric(lapply(df_3$rating, extract_and_replace_stars))

write.csv(df_3, file = "./film3.csv", row.names = FALSE)




### film 4 ----

for (page_number in 1:num_pages) {
  cat("Scraping page:", page_number, "\n")
  scraped_data <- scrape_page(page_number, url_4)
  df_4 <- rbind(df_4, scraped_data)
}

# cleaning
df_4 <- df_4 %>%
  rename(name = name_data,comment = comment_data,
         date = date_data,rating = rating_data)

# replace stars
df_4$rating <- as.numeric(lapply(df_4$rating, extract_and_replace_stars))

write.csv(df_4, file = "./film4.csv", row.names = FALSE)
