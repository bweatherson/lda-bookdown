# I Messed this up and over-wrote a file that was used to generate the original LDA
# I need to find the original copy, but this shouldn't be hard because it was on a bunch of different computers
# It is now saved as generate_lda.R


require(tidytext)
require(topicmodels)
require(tidyverse)


load("all_journals_word_list.RData")

source("short_words.R")

load("fixed_articles.RData")

mid_articles <- articles %>%
  filter(journal == "Mind" | journal == "Philosophical Review" | journal == "Journal of Philosophy") %>%
  filter(year > 1934, year < 1965)

mid_word_list <- all_journals_tibble %>%
  filter(wordcount > 3) %>%
  filter(!word %in% short_words) %>%
  filter(filename %in% mid_articles$file)


mid_all_dtm <- cast_dtm(mid_word_list, filename, word, wordcount)

# Check that it's right

mid_all_dtm

mid_lda <- LDA(mid_all_dtm, k = 30, control = list(seed = 22031848, verbose = 1))

save(mid_lda, file = "mid_lda.RData")

# Build LDA

for (cats in 60:60){
  
  for (i in c(10081792, 20091792, 22091792)){
    
    # The Code here is a bit redundant to allow for looping and building lots of LDAs
    
    ldaname <- paste0("jan_6_lda_",cats,"_",i)  
    #
    assign(ldaname, LDA(all_dtm, k = cats, control = list(seed = i, verbose = 0)))
    #
    ldafilename <- paste0("~/Dropbox/journals-lda/home/",ldaname, ".RData")
    thelda<-get(ldaname)
    save(thelda, file=ldafilename)
    print(perplexity(thelda))
  }
  
}
