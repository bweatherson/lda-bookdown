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

#load("unigrams-fourjournals-to-2016.RData")
#load("metadata-fourjournals-to-2016.RData")

roman_unigrams <- unigrams |>
  filter(!grepl("^m{0,4}(cm|cd|d?c{0,3})(xc|xl|l?x{0,3})(ix|iv|v?i{0,3})$",ngram))

graph_unigrams <- left_join(
  filtered_meta |> select(id, year, sortjournal),
  unigrams,
  by = "id"
) |>
  filter(nchar(ngram) > 0)# |>
#mutate(word_length = nchar(ngram))# |>
#filter(word_length > 2)

word_by_year <- graph_unigrams |>
  group_by(year, ngram) %>% 
  mutate(count = as.numeric(count)) |>
  summarise(n = sum(count)) %>%
  mutate(freq = n / sum(n))

#the_words <- c("ask", "certainly", "really", "surely", "try", "anything", "quite")
the_words <- c("ask", "surely", "answer") # Ordinary Language
the_words <- c("worry", "worries")
the_words <- c("account", "commitment", "approach", "take")
the_words <- c("kripke", "kuhn", "rawls", "lakatos", "fraassen", "quine")
the_words <- c("fodor")
word_freq_graph <-  ggplot(word_by_year |> filter(ngram %in% the_words), 
                           aes(x = year, y = freq, color=ngram)) +
  #  spaghettistyle +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(color = "Journal") +
  geom_point(size = 0.5) +
  geom_smooth(se=FALSE, size = 0.2) +
  labs(x = "Year", y = "Word Frequency", title = "Frequencies of Various Words")

print(word_freq_graph)

all_word_count <- word_by_year |>
  ungroup() |>
  group_by(ngram) |>
  summarise(s = sum(n)) |>
  slice_max(s, n=10000)

top_word_by_year <- word_by_year |>
  filter(ngram %in% all_word_count$ngram) |>
  ungroup() |>
  complete(year, ngram, fill=list(n = 0, freq=0)) |>
  ungroup() |>
  group_by(ngram) |>
  mutate(z = (n-mean(n))/sd(n)) |>
  mutate(diff = (freq - sum(n)/89972867)/sum(n)^0.5)

word_graph_list <- c("problem",
                     "commitment",
                     "worry",
                     "account",
                     "epistemic",
                     "mereological",
                     "though",
                     "since",
                     "because",
                     "question",
                     "whatever",
                     "consciousness",
                     "experience",
                     "psychical",
                     "bergson",
                     "any",
                     "but",
                     "us",
                     "from",
                     "only",
                     "whatever",
                     "schelling",
                     "culture",
                     "life",
                     "her",
                     "idealism",
                     "realism"
                     )

word_graph_list_2 <- c(
"fodor"
)

for (i in word_graph_list_2){
  print(
    ggplot(word_by_year |> filter(ngram == i), 
           aes(x = year, y = freq, color=ngram)) +
      #  spaghettistyle +
      theme_minimal() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.position="none") +
      labs(color = "Journal") +
      geom_point(size = 0.5) +
      geom_smooth(se=FALSE, size = 0.2) +
      labs(x = "Year", y = "Word Frequency", title = tools::toTitleCase(i))
  )
  ggsave(filename=paste0(i,".png"), bg='#ffffff')
}