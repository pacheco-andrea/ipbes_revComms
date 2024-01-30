# Learn text mining 
# following: https://www.tidytextmining.com/



# install packages
# install.packages("tidytext")
# install.packages("textdata")
# install.packages("wordcloud")
# install.packages("reshape2")
# install.packages("gutenbergr")
library(janeaustenr)
library(tidytext)
library(textdata)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(forcats)
library(gutenbergr)

# basic unnesting of tokens and removing stop words ----
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text
text_df <- tibble(line = 1:4, text = text)
text_df
text_df %>%
  unnest_tokens(word, text)

text_df %>%
  unnest_tokens(word, text) %>% anti_join(stop_words)

# sentiments: ----
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")
  
tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = T)
  
# compare +/- sentiments across books
jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber%% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(jane_austen_sentiment) +
  aes(index, sentiment, fill = book) +
  geom_col(show.legend = F) +
  facet_wrap(~book, ncol = 2, scales = "free_x")
  
pride_prejudice <- tidy_books %>% filter(book == "Pride & Prejudice")

afinn_pp <- pride_prejudice %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %% 80) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method =  "AFINN")
  
bing_nrc_pp <- bind_rows(
  pride_prejudice %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))) %>% # missing parenthesis
    mutate(method = "NRC") # there was a mistake in their original code, the mutate was in the wrong line
)  %>%
  count(method, index = linenumber %% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, 
              values_from = n,
              values_fill = 0) %>%
  mutate(sentiment = positive - negative)
  
bind_rows(afinn_pp, bing_nrc_pp) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")
# ok still not exactly the same as the tutorial (maybe the algorithm changed?)

# but, there is a clear difference between the bing and the NRC sentiments.
# this will depend on the proportion of words coded +/- from each of these datasets
get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment)

get_sentiments("bing") %>%
  count(sentiment)

# look at most common positive and negative words used
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>% # note that miss in this example is probably referring to a young lady, should remove
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

# I would say this isn't as interesting... unless it was to explain what words were driving the sentiment analysis results

# wordclouds ----

tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("lightsalmon3", "lightblue4"), max.words = 100)


# tokenizing into sentences
pp_sentences <- tibble(text = prideprejudice) %>%
  unnest_tokens(sentence, text, token = "sentences")



# word and document frequency: tf-idf ----
# tf: term frequency 
# idf: inverse document frequency (calculated by ln(n-documents/n-documents containing the term))
# tf*idf (tf-idf): the frequency of a term adjusted by how rarely its used
# or, how important a word is to a document in a collectin of documents


book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words

# plot the n/total - (tf) the number of times a word appears in a novel divided by the total number of terms (words) in that novel.
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")
# the longer the tail, the more extremely rare the words

# Zipf's law: the frequency of a word appearing is inversely proportional to its rank
# ?? what is meant by rank??
freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), # because the table is already ordered by n, which is the rank
         `term frequency` = n/total) %>%
  ungroup()

freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

# The idea of tf-idf is to find the important words for the content of each document by 
# decreasing the weight for commonly used words and increasing the weight for words that are not
# used very much in a collection or corpus of documents

book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n) # idf and tf-idf are 0 for common words

book_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") + 
  labs(x = "tf-idf", y = NULL)
# all proper nouns!

# interesting to see this measure - why are diversity indices not used in this context? 

# physics examples ----
physics <- gutenberg_download(c(37729, 14725, 13476, 30155), 
                              meta_fields = "author")

physics_words <- physics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE)

physics_words

plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan", 
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))
plot_physics %>% 
  group_by(author) %>% 
  slice_max(tf_idf, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(tf_idf, word, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~author, ncol = 2, scales = "free")


# n-grams and correlations ----
# n-grams are to see what follows certain words, which can be used to model relationships bt them

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

austen_bigrams

austen_bigrams %>%
  count(bigram, sort = TRUE)

# remove cases where there is a stop word in the bi-gram
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

# go back to most common bigrams without stopwords
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

# trigram:
austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram)) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

# can take the bigrams, and weigh them by tf-idf as well
bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf
