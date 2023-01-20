require(tidyverse)
require(tidytext)
require(topicmodels)
require(knitr)
#require(kableExtra)
library(ggplot2); theme_set(theme_light())

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


load("2022-data/Big3-1920-1979-meta.RData")
load("2022-data/big3-lda-4081789-72.RData")

cats <- 72

all_journals_gamma <- tidy(my_lda, matrix = "gamma") |> 
  rename(id = document) |>
  mutate(id = as.numeric(id))

all_journals_classifications <- all_journals_gamma |>
  group_by(id) |>
  top_n(1, gamma) |>
  ungroup() 

all_journals_titles_and_topics <- left_join(all_journals_classifications, filtered_metadata, by = "id")

year_topic_mean <- all_journals_titles_and_topics |> ungroup() |> 
  group_by(topic)  |> 
  dplyr::summarize(date = mean(year)) |> 
  mutate(rank = rank(date))





relabeled_gamma <- left_join(all_journals_gamma, year_topic_mean, by = "topic") |>
  as_tibble() |>
  select(-topic) |>
  dplyr::rename(topic = rank) 

relabeled_gamma <- left_join(relabeled_gamma, filtered_metadata, by = "id") |>
  select(id, gamma, topic, year, journal, sortjournal, words) |>
  mutate(words = case_when(
    is.na(words) ~ 1,
    TRUE ~ words
  ))

journalgamma <- relabeled_gamma  |>
  group_by(year, topic, sortjournal) |>
  dplyr::summarise(gamsum = sum(gamma)) |>
  ungroup() |>
  complete(year, topic, sortjournal, fill = list(gamsum = 0)) |>
  ungroup() |>
  group_by(year, sortjournal) |>
  mutate(freq = gamsum / sum(gamsum)) |>
  ungroup()

journalgamma$topic <- as.factor(journalgamma$topic)


topic_midpoints <- journalgamma |>
  ungroup() |>
  group_by(sortjournal, topic) |>
  summarise(midpoint = weighted.mean(year, freq), 
            totweight = mean(freq),
            .groups = "drop") |>
  ungroup() |>
  group_by(topic) |>
  mutate(topicavg = mean(midpoint)) |>
  mutate(salience = abs((midpoint - topicavg) * totweight * (100 + as.numeric(topic)))) |>
  arrange(-salience)

salient_points <- topic_midpoints |>
  ungroup() |>
  slice(1:10)

topic_midpoint_graph <-   ggplot(topic_midpoints, aes(x = midpoint, 
                                                      y = as.numeric(topic), 
                                                      size = totweight, 
                                                      color = sortjournal,
                                                      group = sortjournal)) +
  geom_point() +
  geom_smooth(se = FALSE, size = 0.5) + 
  theme_minimal() +
  labs(x = "Weighted Average Year",
       y = "Topic",
       color = "Journal",
       size = "Topic Frequency") +
  ggrepel::geom_text_repel(data = salient_points, aes(label = topic, size = .2)) +
  guides(color = guide_legend("Journal"), size = "none")

topic_midpoint_graph