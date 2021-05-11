auth_list <- relabeled_articles %>%
  mutate(auth1 = str_remove(auth1, "†")) %>%
  mutate(auth1 = str_remove(auth1, "‡")) %>%
  mutate(auth1 = str_remove(auth1, "[[:punct:]]")) %>%
  group_by(auth1) %>%
  tally()

only_paper <- auth_list %>%
  filter(n == 1)

old_and_only <- relabeled_articles %>%
  filter(year < 1946) %>%
  filter(auth1 %in% only_paper$auth1)

weird_analysis <- relabeled_articles %>%
  filter(journal == "Analysis", length < 4) %>%
  inner_join(relabeled_gamma, by = "document") %>%
  group_by(document) %>%
  arrange(-gamma.y) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(gamma.x) %>%
  select(citation, topic = topic.x, gamma = gamma.x) %>%
  as_tibble()