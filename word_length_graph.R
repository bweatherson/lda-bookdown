require(tidyverse)

facetstyle <-   theme_minimal() +
  theme(text = element_text(family="Lato"),
        plot.title = element_text(size = rel(1),
                                  family = "Lato",
                                  face = "bold",
                                  margin = margin(0, 0, 10, 0)),
        strip.text = element_blank(),
        panel.spacing.x = unit(-0.05, "lines"),
        panel.background = element_blank(),
        panel.spacing.y = unit(1, "lines"),
        axis.title.x = element_text(size = rel(1),
                                    margin = margin(t = 6, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = rel(1),
                                    margin = margin(t = 0, r = 8, b = 0, l = 0)),
        panel.grid.major.y = element_line(color = "grey85", size = 0.07),
        panel.grid.minor.y = element_line(color = "grey85", size = 0.03),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position="none")
spaghettistyle <- facetstyle +
  theme(panel.grid.major.y = element_line(color = "grey80", size = 0.08),
        panel.grid.minor.y = element_line(color = "grey85", size = 0.04),
        legend.text = element_text(size = rel(0.5)),
        plot.caption = element_text(size = rel(0.7))
  )
freqstyle <-   spaghettistyle +
  theme(legend.text = element_text(size = rel(0.75)),
        panel.grid.major.y = element_line(color = "grey85", size = 0.08),
        legend.position = "right")

load("unigrams-fourjournals-to-1969.RData")
load("metadata-fourjournals-to-1969.RData")

graph_unigrams <- left_join(
  filtered_meta |> select(id, year, sortjournal),
  unigrams,
  by = "id"
) |>
  filter(nchar(ngram) > 0)# |>
  #mutate(word_length = nchar(ngram))# |>
  #filter(word_length > 2)

word_length_by_year <- graph_unigrams |>
  ungroup() |>
  mutate(count = as.numeric(count)) |>
  group_by(year,sortjournal) |>
  summarise(w = weighted.mean(word_length, count), .groups = "drop")

word_length_graph <-  ggplot(word_length_by_year, aes(x = year, y = w, color=sortjournal)) +
#  spaghettistyle +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(color = "Journal") +
  geom_point(size = 0.5) +
  geom_smooth(se=FALSE, size = 0.2) +
  labs(x = "Year", y = "Average Word Length", title = "Average Word Length in Each journal Per Year")

word_length_graph
