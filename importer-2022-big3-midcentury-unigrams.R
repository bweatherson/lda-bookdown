# Install / load packages
require(tidyverse)
require(tidytext)
require(topicmodels)

# Initialise which journal is being used
jcode <- c("jphil", "mind","philrev")
#jname <- c("Journal of Philosophy", "Philosophical Review")
metadataname <- "Big3-1920-1979-meta.RData"
unigramname <- "Big3-1920-1979-unigram.RData"
#bigramname <- "Big3-1920-1979-bigram.RData"
gramname <- "Big3-1920-1979-grams.RData"
#ldaname <- "Big3-1920-1979-lda.RData"


# Add every journal that you're using here as an extra line
journals <- tribble(
  ~code, ~fullname,
  "jphil", "Journal of Philosophy",
  "mind","Mind",
  "philrev", "Philosophical Review"
)

all_metadata <- tibble()

for (i in jcode) {
  # Identify path to metadata folder and read csv
  path1 <- paste0("2022-data/",i,"/meta/")
  files <- list.files(path1)
  
  for (x in files) {
    new_metadata <- read_csv(paste0(path1, "/", x)) |>
      mutate(issueNumber = as.character(issueNumber)) |>
      filter(!is.na(creator)) |>
      mutate(pageStart = as.numeric(pageStart)) |>
      mutate(pageEnd = as.numeric(pageEnd))
    all_metadata <- bind_rows(all_metadata, new_metadata)
    rm(new_metadata)
  }
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

# Focus down to the years we're interested in
filtered_metadata <- filtered_metadata |>
  filter(year >= 1920, year <= 1979) |>
  distinct(id, .keep_all = TRUE) # Need this because sometimes I use overlapping input sets

# Remove surveys of foreign philosophy, which end up confusing the model
filtered_metadata <- filtered_metadata |>
  filter(!author %in% c("André Lalande", 
                        "Andre Lalande",
                        "Arthur Liebert", 
                        "A. Lalande",
                        "Jean Piaget",
                        "H. J. de Vleeschauwer",
                        "Janina Lindenbaum Hosiasson",
                        "D. Parodi",
                        "Abel Rey",
                        "Wendell T. Bush",
                        "William H. Werkmeister",
                        "Edgar Wind",
                        "Nicolai Hartmann"
                        ))


all_grams <- c()
path1 <- c()

for (i in jcode) {
  
  # Identify path to unigram folder and read csvs
  path1 <- paste0("2022-data/",i,"/unigram/")
  files <- list.files(path1)
  
  for (x in files) {
    new_unigrams <- read_csv(paste0(path1, x))
    all_grams <- bind_rows(all_grams, new_unigrams)
    rm(new_unigrams)
  }
}

# Commenting out because not doing bigrams
# # Identify path to bgram folder and read csvs
# path1 <- paste0("2022-data/",jcode,"/bigram/")
# files <- list.files(path1)
# 
# for (x in files) {
#   new_bigrams <- read_csv(paste0(path1, "/", x))
#   all_grams <- bind_rows(all_grams, new_bigrams)
#   rm(new_bigrams)
# }


source("short_words-2023.R") # Words we're not using


filtered_grams <- all_grams |>
  filter(str_sub(id, end = 1) == "h") |>
  filter(!str_sub(ngram, -1) == "-") |>
  mutate(id = str_sub(id, start = 29) |> as.numeric()) |>
  filter(nchar(ngram) > 2,
         !ngram %in% short_words,
         id %in% filtered_metadata$id,
         ngram != "",
         ngram != "vol")  |> # This just looks like bibliographic data
  mutate(count = as.numeric(count))#  |>
  #filter(!grepl("^m{0,4}(cm|cd|d?c{0,3})(xc|xl|l?x{0,3})(ix|iv|v?i{0,3})$",ngram))  # Not using because now in short_words

filtered_grams$ngram <- gsub("[[:punct:]]", "", filtered_grams$ngram) # Remove punctuation
filtered_grams$ngram <- gsub("[[:digit:]]", "", filtered_grams$ngram) # Remove numbers
filtered_grams$ngram <- tolower(filtered_grams$ngram)

# Another pass to remove stop words that appear after removing punctuation and spaces

filtered_grams <- filtered_grams |> 
  filter(nchar(ngram) > 2,
         !ngram %in% short_words,
         id %in% filtered_metadata$id,
         ngram != "",
         ngram != "vol") # Do again after punctuation - looks redundant but quicker to do things this way

# The next batch of lines is for cleaning bigrams specifically
# Now remove the undesired bigrams

# doubles <- expand_grid(w1 = short_words, w2 = short_words) |>
#   mutate(pairs = paste0(w1, " ", w2))
# 
# bad_pairs <- c("sensecertainty",
#                "a nc",
#                "nc of",
#                "a nc of",
#                "a sc",
#                "a sc of",
#                "f t t",
#                "g l",
#                "ab in",
#                "sc of",
#                "t t f",
#                "f t",
#                "t f t",
#                "m is",
#                "donc",
#                "whence",
#                "whence by",
#                "argument to",
#                "b in",
#                "reality p",
#                "r b",
#                "and reality p",
#                "sec",
#                "r f",
#                "f alfred",
#                "of s",
#                "y is",
#                "w e",
#                "is p",
#                "g p",
#                "sb p",
#                "op cit",
#                "cit p",
#                "i p",
#                "ibid p",
#                "ii p",
#                "p v",
#                "p is",
#                "op cit p",
#                "vol i",
#                "vol ii",
#                "s is",
#                "of b",
#                "s and",
#                "x is",
#                "prima",
#                "facie",
#                "prima facie",
#                "miss",
#                "proposi"
# )
# 
# filtered_grams <- filtered_grams |>
#   filter(!ngram %in% doubles$pairs,
#          !ngram %in% bad_pairs) |>
#   filter(!stringr::str_ends(ngram," ")) |>
#   filter(!stringr::str_starts(ngram," "))

save(filtered_metadata, file = paste0("2022-data/",metadataname))
save(filtered_grams, file = paste0("2022-data/",gramname))

my_dtm <- cast_dtm(filter(filtered_grams, count > 1), id, ngram, count)

#for (seed in c(05061789, 20061789, 14071789, 04081789, 26081789, 05101789, 08101792, 09201792, 09221792, 15121793)) {
for (seed in c(04081789, 26081789, 05101789, 08101792, 09201792, 09221792, 15121793)) {
    for (cats in c(72, 96)) {

    my_lda <- LDA(my_dtm, k = cats, control = list(seed = seed, verbose = 1))
    
    # The start on analysis - extract topic probabilities
    #my_gamma <- tidy(my_lda, matrix = "gamma")
    
    # Now extract probability of each word in each topic
    #my_beta <- tidy(my_lda, matrix = "beta")
    
    save(my_lda, 
         file = paste0(
           "2022-data/big3-lda-",
           seed,
           "-",
           cats,
           ".RData"
         )
    )
    save(my_lda, 
         file = paste0(
           "/Users/weath/Dropbox (University of Michigan)/lda-backups/big3-lda-",
           seed,
           "-",
           cats,
           ".RData"
         )
    )
    #save(filtered_meta, file="mind_articles.RData")
    #save(allgrams, file="allgrams_mind_prewar.RData")
    
    rmarkdown::render("american-lda-summary.Rmd", 
                      "pdf_document", 
                      params = list(
                        cats = cats, 
                        seed = as.character(seed),
                        lda_file = paste0(
                          "2022-data/big3-lda-",
                          seed,
                          "-",
                          cats,
                          ".RData"
                        )
                      ),
                      output_file = paste0("/Users/weath/Dropbox (Personal)/GoodReader Transfers/mind-lda-summaries/jphil-lda-summary-",
                                           seed,
                                           "-",
                                           cats,
                                           ".pdf")
    )
  }
}