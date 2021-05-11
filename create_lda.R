require(tidytext)
require(topicmodels)
require(tidyverse)

# This is redundant if you've just run the other scripts, but here for resilience
load("my_wordlist.RData")
load("my_articles.RData")
source("short_words.R")

# Filter out short words and words appearing 1-3 times
in_use_word_list <- my_wordlist %>%
  filter(wordcount > 3) %>%
  filter(!word %in% short_words) %>%
  filter(document %in% my_articles$document)

# Create a Document Term Matrix 
my_dtm <- cast_dtm(in_use_word_list, document, word, wordcount)

# Build the lda
# k is the number of topics
# seed is to allow replication; vary this to see how different model runs behave
# Note that this can get slow - the real one I run takes 8 hours, though if you're following this script, it should take seconds
my_lda <- LDA(my_dtm, k = 10, control = list(seed = 22031848, verbose = 1))
save(my_lda, file="my_lda.RData")

# The start on analysis - extract topic probabilities
my_gamma <- tidy(my_lda, matrix = "gamma")

# Now extract probability of each word in each topic
my_beta <- tidy(my_lda, matrix = "beta")


## Alternately - what follows is applying the existing LDA to new data
nj_lda <- posterior(refinedlda, my_dtm)
nj_gamma<- as_tibble(nj_lda$topics, rownames = NA) %>%
  rownames_to_column(var = "document") %>%
  pivot_longer(-document) %>%
  select(document, topic = name, gamma = value) %>%
  mutate(topic = as.integer(topic)) %>%
  inner_join(year_topic_mean, by = "topic") %>%
  select(document, topic = rank, gamma) %>%
  arrange(document, -gamma)

nj_main <- inner_join(nj_gamma, my_articles, by = "document") %>%
  select(citation, journal, year, topic, gamma)