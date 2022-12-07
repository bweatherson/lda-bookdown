require(tidyverse)
metadata <-
  list.files(path="take-two", pattern = "*metadata.csv") %>% 
  map_df(~read_csv(paste0("take-two/",.), col_types = cols(.default = "c")))

unigrams <-
  list.files(path="take-two", pattern = "*unigrams.csv") %>% 
  map_df(~read_csv(paste0("take-two/",.), col_types = cols(.default = "c")))

unigrams$ngram <- gsub("[[:punct:]]", "", unigrams$ngram) # Remove punctuation
unigrams$ngram <- gsub("[[:digit:]]", "", unigrams$ngram) # Remove numbers
unigrams$ngram <- tolower(unigrams$ngram)

filtered_meta <- metadata |> 
  filter(docSubType == "research-article") |> 
  select(id, title, journal = isPartOf, year = publicationYear, number = issueNumber, vol = volumeNumber, author = creator, fpage = pageStart, lpage = pageEnd, words = wordCount, pages = pageCount) |> 
  mutate(year = as.numeric(year),
         number = as.numeric(number),
         vol = as.numeric(vol),
         fpage = as.numeric(fpage),
         lpage = as.numeric(lpage),
         words = as.numeric(words),
         pages = as.numeric(pages))