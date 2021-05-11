#################################
# Read ngrams
# Based on script by John A. Bernau 2018
##########################################################
require(tidyverse)
require(quanteda)

# Journal List
journals <- tribble(
  ~code, ~fullname,
  "revmeta", "Review of Metaphysics",
  "synthese", "Synthese",
  "erk", "Erkenntnis"
)

jlist <- journals$code

# Initialise huge tibble
huge_tibble <- tibble(filename = character(), word = character(), wordcount = numeric())

for (journal in jlist){
  # Set up files paths
  path <- paste0("data/ngram/",journal)
  n_files <- list.files(path)
  
  # Connecting Words to Filter out
  source("short_words.R")
  
  big_tibble <- tibble(filename = character(), word = character(), wordcount = numeric())
  
  for (i in seq_along(n_files)){
    # Remove junk to get codename
    codename <- str_remove(str_remove(n_files[i], "-ngram1.txt"),"journal-article-")
    
    # Get metadata for it
    meta <- my_articles %>% filter(document == codename)
    
    # If it is in article list, extract text
    if(nrow(meta) > 0){
        small_tibble <- read.table(paste0(path, "/", n_files[i]))
        small_tibble <- small_tibble %>%
          dplyr::rename(word = V1, wordcount = V2) %>%
          add_column(document = codename, .before=1) %>%
          mutate(digit = str_detect(word, "[:digit:]"),
                 len = str_length(word)) %>% 
          filter(digit == F & len > 2) %>% 
          filter(!(word %in% short_words)) %>% 
          select(-digit, -len)
        big_tibble <- rbind(big_tibble, small_tibble)
    }
    if (i %% 250 == 0){
      print(paste0("Extracting document # ", journal, " - ", i))
      print(Sys.time())
    }
  }
  huge_tibble <- rbind(huge_tibble, big_tibble)
}

# Adjust data types
my_wordlist <- as_tibble(huge_tibble)
my_wordlist$document <- as.character(my_wordlist$document)
my_wordlist$word <- as.character(my_wordlist$word)

save(my_wordlist, file  = "my_wordlist.RData")
