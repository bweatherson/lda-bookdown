require(tidyverse)
require(tidytext)
require(topicmodels)
require(knitr)
require(kableExtra)
library(ggplot2); theme_set(theme_light())
#detach(plyr)
library(dplyr)

cats <- 2

    all_journals_gamma <- tidy(my_lda, matrix = "gamma")
    
    all_journals_classifications <- all_journals_gamma %>%
      group_by(document) %>%
      top_n(1, gamma) %>%
      ungroup()
    
    all_journals_titles_and_topics <- merge(all_journals_classifications, filtered_meta, by.x="document", by.y="id")
    
    year_topic_mean <- all_journals_titles_and_topics %>% ungroup() %>% 
      group_by(topic)  %>% 
      dplyr::summarize(date = mean(year)) %>% 
      mutate(rank = rank(date))
    
    relabeled_articles <- merge(all_journals_titles_and_topics, year_topic_mean) %>% 
      select(-topic) %>% 
      dplyr::rename(topic = rank)
    
    termlistname <- paste0("terms_",i)
    assign(termlistname, terms(my_lda, 10))
    
    yearcount <-  relabeled_articles  %>%
      group_by(year) %>%
      dplyr::summarise(n = n_distinct(document))
    
    yeartopics <- relabeled_articles  %>%
      group_by(year, topic) %>%
      dplyr::summarise(tn = n_distinct(document)) %>%
      ungroup() %>%
      complete(year, topic, fill = list(tn = 0))
    
    yeargraphs <- merge(yearcount, yeartopics) %>%
      mutate(frequency = round(tn/n,3))
    
    yeargraphs$topic <- as.factor(yeargraphs$topic)
    
    phil_topics <- tidy(my_lda, matrix = "beta")
    relabeled_topics <- merge(phil_topics, year_topic_mean) %>%
      as_tibble() %>%
      select(-topic) %>%
      dplyr::rename(topic = rank)
    
    word_score <- relabeled_topics %>%
      group_by(term) %>%
      dplyr::summarise(sumbeta = sum(beta)) %>%
      arrange(desc(sumbeta))
    

    
    relabeled_gamma <- merge(all_journals_gamma, year_topic_mean) %>%
      as_tibble() %>%
      select(-topic) %>%
      dplyr::rename(topic = rank) |>
      mutate(document = as.numeric(document))
    
    relabeled_gamma <- merge(relabeled_gamma, filtered_meta, by.x = "document", by.y = "id") %>%
      select(document, gamma, topic, year, journal, sortjournal, words) %>%
      mutate(words = case_when(
        is.na(words) ~ 1,
        TRUE ~ words
      ))
    
    journalgamma <- relabeled_gamma  %>%
      group_by(year, topic, sortjournal) %>%
      dplyr::summarise(gamsum = sum(gamma)) %>%
      ungroup() %>%
      complete(year, topic, sortjournal, fill = list(gamsum = 0))
    
    journalgamma$topic <- as.factor(journalgamma$topic)
    
    journalpagegamma <- relabeled_gamma  %>%
      mutate(value = words * gamma) %>%
      group_by(year, topic, sortjournal) %>%
      dplyr::summarise(valsum = sum(value)) %>%
      ungroup() %>%
      complete(year, topic, sortjournal, fill = list(gamsum = 0))
    
    journalpagegamma$topic <- as.factor(journalpagegamma$topic)
    
    yeargamma <- relabeled_gamma  %>%
      group_by(year, topic) %>%
      dplyr::summarise(gamsum = sum(gamma)) %>%
      ungroup() %>%
      complete(year, topic, fill = list(gamsum = 0))
    
    yeargamma$topic <- as.factor(yeargamma$topic)
    
    pagegamma <- relabeled_gamma  %>%
      mutate(value = gamma * words) %>%
      group_by(year, topic) %>%
      dplyr::summarise(valsum = sum(value)) %>%
      ungroup() %>%
      complete(year, topic, fill = list(gamsum = 0))
    
    pagegamma$topic <- as.factor(pagegamma$topic)


for (jjj in 1:cats){    
    cat("\\newpage \n")
    
    cat("\n")
    cat("### Characteristic Articles for Topic ", jjj, "\n")
    char_art <- relabeled_articles %>%
      filter(topic == jjj) %>%
      mutate(score = gamma * log10(words + 1)) %>%
      arrange(desc(score)) %>%
      slice(1:15)
    article_list <- char_art$citation
    #  print(kable(article_list, col.names = NULL, booktabs = T, linesep = "") %>%
    #        kable_styling(latex_options = c("striped", "hold_position")))
    
    for (jj in 1:15){
      cat(jj,". ", article_list[jj], "\n\n", sep="")
    }
    cat("\\newpage \n")


}        
    
    
    