require(tidyverse)
metadata <-
  list.files(path="early-mind", pattern = "*metadata.csv") |> 
  map_df(~read_csv(paste0("early-mind/",.), col_types = cols(.default = "c")))

allgrams <-
  list.files(path="early-mind", pattern = "*unigrams.csv") |> 
  map_df(~read_csv(paste0("early-mind/",.), col_types = cols(.default = "c")))

allgrams$ngram <- gsub("[[:punct:]]", "", allgrams$ngram) # Remove punctuation
allgrams$ngram <- gsub("[[:digit:]]", "", allgrams$ngram) # Remove numbers
allgrams$ngram <- tolower(allgrams$ngram)
allgrams$id <- str_sub(allgrams$id, start=29) |> as.numeric()

filtered_meta <- metadata |> 
  filter(docSubType == "research-article") |> 
  select(id, 
         title, 
         journal = isPartOf, 
         year = publicationYear, 
         number = issueNumber, 
         vol = volumeNumber, 
         author = creator, 
         fpage = pageStart, 
         lpage = pageEnd, 
         words = wordCount, 
         pages = pageCount) |> 
  filter(is.na(author) == FALSE) |> 
  mutate(year = as.numeric(year),
         number = as.numeric(number),
         vol = as.numeric(vol),
         fpage = as.numeric(fpage),
         lpage = as.numeric(lpage),
         words = as.numeric(words),
         pages = as.numeric(pages)) |> 
  filter(!grepl("Correction",title)) |>
  filter(!grepl("Foreword",title)) |>
  filter(!(title == "Descriptive Notices")) |>
  filter(!(title == "Editorial")) |>
  filter(!(title == "Letter to Editor")) |>
  filter(!(title == "Letter")) |>
  filter(!(title == "Introduction")) |>
  filter(!grepl("Introductory Note",title)) |>
  filter(!grepl("Foreword",title)) |>
  filter(!grepl("Errat",title)) |>
  filter(!grepl("Erata",title)) |>
  filter(!grepl("Abstract of C",title)) |>
  filter(!grepl("Abstracts of C",title)) |>
  filter(!grepl("To the Editor",title)) |>
  filter(!grepl("Corrigenda",title)) |>
  filter(!grepl("Obituary",title)) |>
  filter(!grepl("Congress",title)) |> 
  mutate(citation = paste0(author,
                           ", ",
                           year,
                           ", \"",
                           title,
                           ",\" _",
                           journal,
                           "_ ",
                           vol,
                           ":",
                           fpage,
                           "–",
                           lpage,
                           "."),
         sortjournal = str_replace(journal, 
                                   "The Journal of Philosophy, Psychology and Scientific Methods", 
                                   "The Journal of Philosophy"),
         sortjournal = str_replace(sortjournal, 
                                   "Proceedings of the Aristotelian Society, Supplementary Volumes", 
                                   "Proceedings of the Aristotelian Society"),
         id = str_sub(id, start=29) |> as.numeric()
)

require(tidytext)
require(topicmodels)
source("short_words.R")

short_words <- c(short_words, " ", "", "vol")

doubles <- expand_grid(w1 = short_words, w2 = short_words) |>
  mutate(pairs = paste0(w1, " ", w2))

bad_pairs <- c("sensecertainty",
               "a nc",
               "nc of",
               "a nc of",
               "a sc",
               "a sc of",
               "f t t",
               "g l",
               "ab in",
               "sc of",
               "t t f",
               "f t",
               "t f t",
               "m is",
               "donc",
               "whence",
               "whence by",
               "argument to",
               "b in",
               "reality p",
               "r b",
               "and reality p",
               "sec",
               "r f",
               "f alfred",
               "of s",
               "y is",
               "w e",
               "is p",
               "g p",
               "sb p",
               "op cit",
               "cit p",
               "i p",
               "ibid p",
               "ii p",
               "p v",
               "p is",
               "op cit p",
               "vol i",
               "vol ii",
               "s is",
               "of b",
               "s and",
               "x is",
               "prima",
               "facie",
               "prima facie",
               "miss",
               "proposi"
               )

filtered_allgrams <- allgrams |>
  filter(nchar(ngram) > 2,
         !ngram %in% short_words,
         !ngram %in% doubles$pairs,
         !ngram %in% bad_pairs,
         id %in% filtered_meta$id) |>
    mutate(count = as.numeric(count))  |>
  filter(!grepl("^m{0,4}(cm|cd|d?c{0,3})(xc|xl|l?x{0,3})(ix|iv|v?i{0,3})$",ngram)) |>
  filter(!stringr::str_ends(ngram," ")) |>
  filter(!stringr::str_starts(ngram," "))

#save(filtered_meta, file="metadata-mind-1903-to-1939.RData")
#save(allgrams, file="allgrams-mind-1903-to-1939.RData")

my_dtm <- cast_dtm(filtered_allgrams, id, ngram, count)

for (seed in c(05061789, 20061789, 14071789, 04081789, 26081789, 05101789, 08101792, 09201792, 09221792,15121793)){
  for (cats in c(2, 4, 6, 8, 10, 12, 15, 16, 20, 24)){

    my_lda <- LDA(my_dtm, k = cats, control = list(seed = seed, verbose = 1))
    
    # The start on analysis - extract topic probabilities
    #my_gamma <- tidy(my_lda, matrix = "gamma")
    
    # Now extract probability of each word in each topic
    #my_beta <- tidy(my_lda, matrix = "beta")
    
    save(my_lda, file="mind_lda.RData")
    #save(filtered_meta, file="mind_articles.RData")
    #save(allgrams, file="allgrams_mind_prewar.RData")
    
    rmarkdown::render("mind-lda-summary.Rmd", 
                      "pdf_document", 
                      params = list(
                        cats = cats, seed = as.character(seed)
                      ),
                      output_file = paste0("mind-lda-summary-",seed,"-",cats,".pdf")
    )
  }
}