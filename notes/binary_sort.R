require(tidyverse)
require(tidytext)
require(topicmodels)
require(knitr)
require(kableExtra)
library(ggplot2); theme_set(theme_light())

gamma_sorter <- function(x)
  case_when(
    x < 0.01 ~ 1,
    x < 0.5 ~ 2,
    x < 0.99 ~ 3,
    x < 1.01 ~ 4
  )

journal_short_names <- c(
  "Analysis" = "Analysis",
  "British Journal for the Philosophy of Science" = "BJPS",
  "Ethics" = "Ethics",
  "Journal of Philosophy" = "Journal of Philosophy",
  "Mind" = "Mind",
  "Noûs" = "Noûs",
  "Philosophical Review" = "Philosophical Review",
  "Philosophy and Phenomenological Research" = "PPR",
  "Philosophy and Public Affairs" = "P&PA",
  "Philosophy of Science" = "Philosophy of Science",
  "Proceedings of the Aristotelian Society" = "Aristotelian Society",
  "The Philosophical Quarterly" = "Philosophical Quarterly"
)


#load("fixed_articles.RData")
#load("highly_cites_articles.RData")

i <- 22031848
cats <- 90


#getldaname <- paste0("~/Dropbox/journals-lda/home/jan_17_lda_",cats,"_",i,".RData")
#load(getldaname)

# all_journals_gamma <- tidy(thelda, matrix = "gamma")
# 
# all_journals_classifications <- all_journals_gamma %>%
#   group_by(document) %>%
#   top_n(1, gamma) %>%
#   ungroup()
# 
# all_journals_titles_and_topics <- merge(all_journals_classifications, articles, by.x="document", by.y="file")
# 
# year_topic_mean <- all_journals_titles_and_topics %>% ungroup() %>% 
#   group_by(topic)  %>% 
#   dplyr::summarize(date = mean(year)) %>% 
#   mutate(rank = rank(date))
# 
# relabeled_articles <- merge(all_journals_titles_and_topics, year_topic_mean) %>% 
#   select(-topic) %>% 
#   dplyr::rename(topic = rank)
# 
load("all_journals_word_list.RData")
# 
source("short_words.R")
# 
word_list <- all_journals_tibble %>%
   filter(wordcount > 3) %>%
   filter(!word %in% short_words) %>%
   filter(filename %in% articles$file)
# 
# all_dtm <- cast_dtm(word_list, filename, word, wordcount)

binary_tracker <- tribble(
  ~topic, ~confident, ~confident_01, ~confident_02
)

binary_keywords <- tribble(
  ~topic, ~keywords_01, ~keywords_02
)

subtopics <- tribble(
  ~document, ~subtopic
)

i <- 22031848

for (j in 22:22){

temp_articles <- relabeled_articles %>%
  filter(topic == j)

temp_list <- word_list %>%
  filter(filename %in% temp_articles$document)

temp_dtm <- cast_dtm(temp_list, filename, word, wordcount)

templda <- LDA(temp_dtm, k = 2, control = list(seed = i, verbose = 0))

temp_gamma <- tidy(templda, matrix = "gamma") %>% 
  filter(topic == 1)

gamma_summary <- temp_gamma %>%
  mutate(gamma = gamma_sorter(gamma)) %>%
  group_by(gamma) %>%
  dplyr::summarise(n = n_distinct(document))

confidence = (gamma_summary[[1,2]] + gamma_summary[[4,2]]) / (gamma_summary[[2,2]] + gamma_summary[[3,2]])

binary_tracker <- binary_tracker %>% 
  add_row(topic = j, 
          confident = confidence, 
          confident_01 = gamma_summary[[1,2]]/nrow(temp_articles), 
          confident_02 = gamma_summary[[4,2]]/nrow(temp_articles)
          )

temp_topics <- tidy(templda, matrix = "beta")
temp_score <- temp_topics %>%
  group_by(term) %>%
  dplyr::summarise(sumbeta = sum(beta)) %>%
  arrange(desc(sumbeta))

temp_topics <- merge(temp_topics, temp_score) %>%
  filter(sumbeta > 0.001) %>%
  mutate(score = beta/sumbeta) %>%
  arrange(-score, -sumbeta)

temp_first <- temp_topics %>%
  filter(topic == 1) %>%
  slice(1:5)

temp_first_topics <- temp_first$term %>%
  paste(collapse = ", ")

temp_second <- temp_topics %>%
  filter(topic == 2) %>%
  slice(1:5)

temp_second_topics <- temp_second$term %>%
  paste(collapse = ", ")


binary_keywords <- binary_keywords %>%
  add_row(topic = j, keywords_01 = temp_first_topics, keywords_02 = temp_second_topics)

temp_subtopics <- temp_gamma %>%
  mutate(subtopic = case_when(
    gamma < 0.5 ~ 2,
    TRUE ~ 1
  )
  ) %>%
  select(document, subtopic)

subtopics <- bind_rows(subtopics, temp_subtopics)
}

save(subtopics, file="subtopics.RData")
save(binary_keywords, file="binary_keywords.RData")
save(binary_tracker, file="binary_tracker.RData")