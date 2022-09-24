require(tidyverse)
# load("big_tibble.RData")
# load("final_articles.RData")
# load("article_year_tibble.RData")
# load("word_year_count.RData")
# load("word_year_journal_count.RData")

word_year_frequency <- function(x){
  big_tibble %>%
    filter(word == x) %>%
    group_by(year) %>%
    dplyr::summarise(c = sum(wordcount)) %>% 
    right_join(word_year_count, by="year") %>% 
    replace_na(list(c = 0)) %>%
    mutate(f = c / a) %>%
    mutate(term = x)  %>% 
    arrange(year)
}

word_year_appearances <- function(x){
  big_tibble %>%
    filter(word == x) %>%
    group_by(year) %>%
    count(name = "t") %>% 
    right_join(article_year_count, by="year") %>% 
    replace_na(list(t = 0)) %>%
    mutate(f = t / n) %>%
    mutate(term = x)  %>% 
    arrange(year)
}

where_word <- function(x){
  big_tibble %>% 
    filter(word == x) %>% 
    slice_max(wordcount, n = 100) %>% 
    left_join(articles, by = c("document", "year")) %>% 
    select(citation, year)
}


word_journal_year_count <- function(x){
  big_tibble %>%
    filter(word == x, wordcount > 1) %>%
    group_by(year, journal) %>%
    count(name = "t") %>% 
    replace_na(list(t = 0)) %>%
    mutate(term = x)  %>% 
    arrange(year)
}
view(word_journal_year_count("spinoza"))

spinner <- word_journal_year_count("attribute") %>% 
  filter(year >= 1930) %>% 
  filter(year < 1990) %>% 
  mutate(decade = floor((year-1900)/10)) %>% 
  group_by(journal, decade) %>% 
  summarise(art_count = sum(t))