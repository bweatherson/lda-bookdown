# Install / load packages
require(tidyverse)
require(tidytext)
require(topicmodels)

# Initialise which journal is being used
jcode <- "jphil"
jname <- "Journal of Philosophy"
metadataname <- "JPhil1940-1989-meta.RData"
unigramname <- "JPhil1940-1989-unigram.RData"
ldaname <- "JPhil1940-1990-lda.RData"


# Add every journal that you're using here as an extra line
journals <- tribble(
  ~code, ~fullname,
  "jphil", "Journal of Philosophy",
)

all_metadata <- tibble()

# Identify path to metadata folder and read csv
path1 <- paste0("2022-data/",jcode,"/meta/")
files <- list.files(path1)

for (x in files) {
  new_metadata <- read_csv(paste0(path1, "/", x))
  all_metadata <- bind_rows(all_metadata, new_metadata)
  rm(new_metadata)
}

filtered_metadata <- all_metadata |>
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
         number = as.numeric(str_sub(number, end = 2)),
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
         id = str_sub(id, start = 29) |> as.numeric()
  )

# Identify path to unigram folder and read csvs
path1 <- paste0("2022-data/",jcode,"/unigram/")
files <- list.files(path1)

all_unigrams <- c()

for (x in files) {
  new_unigrams <- read_csv(paste0(path1, "/", x))
  all_unigrams <- bind_rows(all_unigrams, new_unigrams)
  rm(new_unigrams)
}

source("short_words.R") # Words we're not using

filtered_unigrams <- all_unigrams |>
  mutate(id = str_sub(id, start = 29) |> as.numeric()) |>
  filter(nchar(ngram) > 2,
         !ngram %in% short_words,
         id %in% filtered_metadata$id,
         ngram != "",
         ngram != "vol")  |> # This just looks like bibliographic data
  mutate(count = as.numeric(count))  |>
  filter(!grepl("^m{0,4}(cm|cd|d?c{0,3})(xc|xl|l?x{0,3})(ix|iv|v?i{0,3})$",ngram))

filtered_unigrams$ngram <- gsub("[[:punct:]]", "", filtered_unigrams$ngram) # Remove punctuation
filtered_unigrams$ngram <- gsub("[[:digit:]]", "", filtered_unigrams$ngram) # Remove numbers
filtered_unigrams$ngram <- tolower(filtered_unigrams$ngram)

filtered_unigrams <- filtered_unigrams |> 
  filter(nchar(ngram) > 2,
         !ngram %in% short_words,
         id %in% filtered_metadata$id,
         ngram != "",
         ngram != "vol") # Do again after punctuation - looks redundant but quicker to do things this way

save(filtered_metadata, file = paste0("2022-data/",metadataname))
save(filtered_unigrams, file = paste0("2022-data/",unigramname))


for (seed in c(05061789, 20061789, 14071789, 04081789, 26081789, 05101789, 08101792, 09201792, 09221792,15121793)) {
  for (cats in c(2, 4, 6, 8, 10, 12, 15, 16, 20, 24)) {

    my_lda <- LDA(my_dtm, k = cats, control = list(seed = seed, verbose = 1))
    
    # The start on analysis - extract topic probabilities
    #my_gamma <- tidy(my_lda, matrix = "gamma")
    
    # Now extract probability of each word in each topic
    #my_beta <- tidy(my_lda, matrix = "beta")
    
    save(my_lda, file = "jphil_lda.RData")
    #save(filtered_meta, file="mind_articles.RData")
    #save(allgrams, file="allgrams_mind_prewar.RData")
    
    rmarkdown::render("jphil-lda-summary.Rmd", 
                      "pdf_document", 
                      params = list(
                        cats = cats, seed = as.character(seed)
                      ),
                      output_file = paste0("/Users/weath/Dropbox (Personal)/GoodReader Transfers/mind-lda-summaries/jphil-lda-summary-",seed,"-",cats,".pdf")
    )
  }
}