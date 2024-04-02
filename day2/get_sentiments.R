library(tidyverse)
library(syuzhet)

# load data ----
d1 <- read.csv("film1.csv")
d2 <- read.csv("film2.csv")
d3 <- read.csv("film3.csv")
d4 <- read.csv("film4.csv")

# get sentiments  ----
sent1 <- get_nrc_sentiment(d1$comment)
sent2 <- get_nrc_sentiment(d2$comment)
sent3 <- get_nrc_sentiment(d3$comment)
sent4 <- get_nrc_sentiment(d4$comment)

d1 <- cbind(d1, sent1)
d2 <- cbind(d2, sent2)
d3 <- cbind(d3, sent3)
d4 <- cbind(d4, sent4)

rm(sent1, sent2, sent3, sent4)

dall <- rbind(d1, d2, d3, d4)

## save ----
write.csv(d1, "film1_sent.csv")
write.csv(d2, "film2_sent.csv")
write.csv(d3, "film3_sent.csv")
write.csv(d4, "film4_sent.csv")
write.csv(dall, "all_sent.csv")

################################################################################



