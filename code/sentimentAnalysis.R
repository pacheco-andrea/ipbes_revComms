# Analyzing external review comments of the Nexus assessment 

# this script delves into testing what sentiment analysis could look like

# libraries
library(tidytext)
library(textdata)
library(dplyr)
library(ggplot2)


# directories
wdmain <- "G:/My Drive/Projects/IPBES-Nexus/00_analyses/RevComms/"
source("G:/My Drive/Projects/IPBES-Nexus/00_analyses/RevComms/code/nexus_colours.R")
setwd(paste0(wdmain, "data/processed/nexus_FOD-review/"))

# get sentiment data
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")
# get FOD review data
fod <- read.csv("FOD_revComments_unnested.csv")

# sentiment analysis ----
# The nrc lexicon categorizes words in a binary fashion (“yes”/“no”) into categories of 
# positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.
# bing lexicon categorizes words in a binary fashion into positive and negative categories
# AFINN lexicon assigns words with a score that runs between -5 and 5, 
# with negative scores indicating negative sentiment and positive scores indicating positive sentiment.

fod_sentiments <- fod %>%
  inner_join(get_sentiments("bing")) %>%
  count(chapter, index = ID, sentiment) %>% # i believe this would tell it to get count the sentiments per comment
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

plot_fod_sentiments <- fod_sentiments %>%
  filter(chapter != "ch7" & chapter != "glossary") %>%
  ggplot() +
  aes(index, sentiment, fill = chapter) +
  geom_col(show.legend = F) +
  theme_light() +
  facet_wrap(~chapter, ncol = 5, scales = "free_x")
plot_fod_sentiments

# note, something to be added would be the comparison of sentiment dictionaries across bing, nrc, and afinn
# also, what i would find most interesting is to compare these sentiments across FOD and SOD...

# save plot
setwd(paste0(wdmain, "output/"))
png("bing-sentiments_FOD.png", units = "px", width = 3000, height = 1700, res = 300)
plot_fod_sentiments
dev.off()

# what are the most common negative/positive words used? 
sentiment_wc <- fod %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = T) %>%
  ungroup

sentiment_wc %>% 
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

# I would say this isn't soo so interesting... 
# though it does help understand and explain what precise words are driving the sentiment analysis results

# make wordclouds 
fod %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200))

fod %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("lightsalmon3", "lightblue4"), max.words = 100)

# yeah, i also don't think this is as interesting...


# tf-idf ----
# calculate and plot the term frequency and the inverse document frequency
# i.e. an index for how important the word is in the overall document

# get the total number of words per chapter
head(fod)
fod_words <- fod %>%
  count(chapter, word, sort = TRUE)
total_fodwords <- fod_words %>%
  group_by(chapter) %>%
  summarize(total = sum(n))
fod_words <- left_join(fod_words, total_fodwords)

fod_tf.idf <- fod_words %>%
  bind_tf_idf(word, chapter, n) # idf and tf-idf are 0 for common words

fod_tf.idf %>%
  filter(chapter != "ch7" & chapter != "glossary") %>%
  group_by(chapter) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = chapter)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~chapter, ncol = 5, scales = "free") + 
  labs(x = "tf-idf", y = NULL)


# investigate satanic?
setwd(paste0(wdmain, "data/processed/nexus_FOD-review/"))
fod_original <-  read.csv("FOD_revComments.csv")

satanic <- grep("satanic", fod_original$comment)
fod_original[satanic,]

# ok this makes it clear that there needs to be some further cleaning of the comments,
# there are many numbers referencing the sections in the comments,
# then, we'd need to consider how some of the crazy comments like the satanic one above are used?

# n-grams? ----