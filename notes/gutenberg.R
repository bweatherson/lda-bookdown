# Get Books with Chapter Headings

# 5827 is Problems of Philosophy
# 46743 is Methods of Ethics
# 15776 is Economic Consequences of the Peace
# 852 is Democracy and Education

require(gutenbergr)

titles <- c(5827, 46743, 15776)

books <- gutenberg_download(titles, meta_fields = "title")

# Have to Get Moore separately because it doesn't have metadata in gutenberg package
# Not sure why this throws up so many errors
# Note we'll have to have all this happening offline - can't download the books every time we compile

moore <- gutenberg_download(53430) %>%
  mutate(title = "Principia Ethica")

books <- bind_rows(books, moore)

# On Liberty
# Have to Get Rid of opening or it thinks the table of contents is five chapters

mill <- gutenberg_download(34901, meta_fields = "title") %>%
  slice(-1:-440)

books <- bind_rows(books, mill)

require(stringr)

# divide into documents, each representing one chapter
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^CHAPTER ", ignore_case = FALSE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# Get Books with lower case chapter headings
# In this case just Democracy and Education
# And need to slice it as well because of the annoying table of contents

dewey <- gutenberg_download(852) %>%
  add_column(title = "Democracy and Education")

dewey_chapters <- dewey %>% 
  slice(-1:-52) %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# Get Books with Lecture Headings

lecture_titles <- c("Our Knowledge of the External World as a Field for Scientific Method in Philosophy", "The Analysis of Mind")

lecture_books <- gutenberg_works(title %in% lecture_titles) %>%
  gutenberg_download(meta_fields = "title")

# divide into documents, each representing one chapter
lecture_by_chapter <- lecture_books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^LECTURE ", ignore_case = FALSE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# Put the lectures and the chapters back together

by_chapter <- bind_rows(by_chapter, lecture_by_chapter, dewey_chapters)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  filter(!word %in% short_words) %>%
  filter(nchar(word) > 2) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

# Create DTM

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

# Apply the old LDA to them
gutenberg_lda <- posterior(thelda, chapters_dtm)

# Get the topic probabilities
# The mess here is to move the topic numbers that it uses internally onto my topic numbers (which are chronological)
gutenberg_gamma<- as_tibble(gutenberg_lda$topics, rownames = NA) %>%
  rownames_to_column(var = "document") %>%
  pivot_longer(-document) %>%
  select(document, topic = name, gamma = value) %>%
  mutate(topic = as.integer(topic)) %>%
  inner_join(year_topic_mean, by = "topic") %>%
  select(document, topic = rank, gamma) %>%
  arrange(document, -gamma)

View(gutenberg_gamma)

gg_revised <- gutenberg_gamma %>% 
  mutate(chap_num = str_extract(document, "[^_]+$")) %>%
  mutate(book_name = str_extract(document, "^.*(?=_)"))

gg_summary <- gg_revised %>%
  group_by(book_name, topic) %>%
  summarise(g = mean(gamma)) %>%
  ungroup() %>%
  group_by(book_name) %>%
  filter(topic > 45) %>%
  top_n(1, g)
