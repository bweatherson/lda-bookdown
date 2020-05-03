#################################
# Text Analysis with JSTOR Archives
# John A. Bernau 2018
# JSTOR Data Prep #1
# Parsing out xml files
#################################

# Install / load packages
# install.packages("xml2")
# install.packages("tidyverse")
require(xml2)
require(tidyverse)
require(plyr)
require(dplyr)

journals <- tribble(
  ~code, ~fullname,
  "papa", "Philosophy and Public Affairs",
  "analysis", "Analysis",
  "nous", "Noûs",
  "bjps", "British Journal for the Philosophy of Science",
  "arist", "Proceedings of the Aristotelian Society",
  "pq", "The Philosophical Quarterly",
  "psci", "Philosophy of Science",
  "ethics", "Ethics",
  "philrev", "Philosophical Review",
  "mind", "Mind",
  "jphil", "Journal of Philosophy",
  "ppr", "Philosophy and Phenomenological Research"
)

all_metadata <- tibble()

journal_count <- nrow(journals)

for (j in 1:journal_count){

# Identify path to metadata folder and list files
path1 <- paste0("/Users/weath/Downloads/",journals$code[j],"_bigrams/metadata")
files <- list.files(path1)

# Initialize empty set
final_data <- NULL

# Using the xml2 package: for each file, extract metadata and append row to final_data
for (x in files){
  path <- read_xml(paste0(path1, "/", x))
  
  # File name - without .xml to make it easier for lookup purposes
  file <- str_remove(str_remove(x, ".xml"),"journal-article-")
  
  # Article type
  type <- xml_find_all(path, "/article/@article-type") %>% 
    xml_text()
  
  # Title
  title <- xml_find_all(path, xpath = "/article/front/article-meta/title-group/article-title") %>% 
    xml_text()
  
  # Author names
  authors <- xml_find_all(path, xpath = "/article/front/article-meta/contrib-group/contrib") %>%
    xml_text()
  auth1 <- authors[1]
  auth2 <- authors[2]
  auth3 <- authors[3]
  auth4 <- authors[4]
  
  # Affiliations
  #  affil <- xml_find_all(path, xpath = "/article/front/article-meta/contrib-group/aff") %>%
  #    xml_text()
  #  affil1 <- affil[1]
  #  affil2 <- affil[2]
  #  affil3 <- affil[3]
  #  affil4 <- affil[4]
  #  affil5 <- affil[5]
  #  affil6 <- affil[6]
  #  affil7 <- affil[7]
  #  affil8 <- affil[8]
  #  affil9 <- affil[9]
  #  affil10 <- affil[10]
  
  # Abstract
  #  abstract <- xml_find_all(path, xpath = "/article/front/article-meta/abstract") %>% 
  #    xml_text()
  
  # Month
  #  month <- xml_find_all(path, xpath = "/article/front/article-meta/pub-date/month") %>% 
  #    xml_text()
  
  # Year
  year <- xml_find_all(path, xpath = "/article/front/article-meta/pub-date/year") %>% 
    xml_text()
  
  # Volume
  vol <- xml_find_all(path, xpath = "/article/front/article-meta/volume") %>% 
    xml_text()
  
  # Issue
  iss <- xml_find_all(path, xpath = "/article/front/article-meta/issue") %>% 
    xml_text()
  
  # First page
  fpage <- xml_find_all(path, xpath = "/article/front/article-meta/fpage") %>% 
    xml_text()
  
  # Last page
  lpage <- xml_find_all(path, xpath = "/article/front/article-meta/lpage") %>% 
    xml_text()
  
  # Footnote
  #  notes <- xml_find_all(path, xpath = "/article/front/notes") %>% 
  #    xml_text()

  # Language
  lang <-  xml_find_all(path, xpath = "/article/front/article-meta/custom-meta-group/custom-meta/meta-value") %>%  
      xml_text()

  # Bind all together
  article_meta <- cbind(file, type, title, 
                        auth1, auth2, auth3, auth4, year, vol, iss, fpage, lpage, lang)
  
  final_data <- rbind.fill(final_data, data.frame(article_meta, stringsAsFactors = FALSE))
  
  # Print progress 
  if (nrow(final_data) %% 250 == 0){
    print(paste0("Extracting document # ", nrow(final_data)," - ", journals$code[j]))
    print(Sys.time())
  }
}

# Check output
#names(final_data)
#str(final_data)

# Shorter name
fd <- c()
fd <- final_data

# Adjust data types
fd$type <- as.factor(fd$type)
#fd$month <- as.numeric(fd$month)
fd$year <- as.numeric(fd$year)
fd$vol <- as.numeric(fd$vol)
fd$iss <- str_replace(fd$iss, "S", "10")
fd$iss <- as.numeric(fd$iss)



# Remove variables if ALL rows are "NA" - this is killing me with 4 authored papers
#not_all_na <- function(x) any(!is.na(x))
#fd <- fd %>% select_if(not_all_na)



# Create date variable
#fd$date <- paste("01", fd$month, fd$year, sep = "-")
#fd$date <- as.Date(fd$date, "%d-%m-%Y")
#class(fd$date)
#head(fd$date)

#######################################################
# Page variables
#######################################################
# Examine for any unusual values ("cover", "ix", etc)
#View(count(fd$fpage))
#View(count(fd$lpage))

# Convert to numeric (roman numerals converted to NA by default)
fd$fpage <- str_replace(fd$fpage, "S", "1000")
fd$lpage <- str_replace(fd$lpage, "S", "1000")
fd$fpage <- as.numeric(fd$fpage)
fd$lpage <- as.numeric(fd$lpage)
fd <- fd %>%
  mutate(
    fpage = case_when(
      fpage > 1000000 ~ fpage - 990000,
      fpage > 100000 ~ fpage - 90000,
      TRUE ~ fpage
      )
  )
fd <- fd %>%
  mutate(
    lpage = case_when(
      lpage > 1000000 ~ lpage - 990000,
      lpage > 100000 ~ lpage - 90000,
      TRUE ~ lpage
    )
  )
fd$fpage[fd$fpage == ""] <- NA
fd$lpage[fd$lpage == ""] <- NA



# Create length variable
fd$length <- fd$lpage - fd$fpage + 1
# Are you getting negative length variables? 
# See "negative page length.R"

# Convert S pages back - this isn't working for now so commented out - will do it in citation get
# fd <- fd %>%
#   mutate(
#     fpage = case_when(
#       fpage < 10000 ~ 10000,
#       TRUE ~ paste0("S",fpage-10000)
#     )
#   )
# fd <- fd %>%
#   mutate(
#     lpage = case_when(
#       lpage < 10000 ~ 10000,
#       TRUE ~ paste0("S",lpage-10000)
#     )
#   )
# 

# Examine data
#View(arrange(fd, desc(length)))
#
#ggplot(fd, aes(length)) +
#  geom_histogram(bins = 50)

#######################################################
# Explore data
#######################################################
# Examine type categories
# dplyr::count(fd, type)
# Careful of "research-articles" with titles like "Book Review" or "Review"
#filter(fd, type == "research-articles" & title == "Book Reviews")

# Save as .RData file for future work
#save(fd, file = "j_phil1.RData")

# Convert to tibble

fd <- as_tibble(fd)


# Filter out things that aren't research-article, have no author

fd <- fd %>%
  arrange(desc(-length)) %>%
  filter(type == "research-article") %>%
  filter(is.na(auth1) == FALSE)

# Filter articles that we don't want  

fd <- fd %>%
  filter(!grepl("Correction",title)) %>%
  filter(!grepl("Foreword",title)) %>%
  filter(!(title == "Descriptive Notices")) %>%
  filter(!(title == "Editorial")) %>%
  filter(!(title == "Letter to Editor")) %>%
  filter(!(title == "Letter")) %>%
  filter(!(title == "Introduction")) %>%
  filter(!grepl("Introductory Note",title)) %>%
  filter(!grepl("Foreword",title)) %>%
  filter(!grepl("Errat",title)) %>%
  filter(!grepl("Erata",title)) %>%
  filter(!grepl("Abstract of C",title)) %>%
  filter(!grepl("Abstracts of C",title)) %>%
  filter(!grepl("To the Editor",title)) %>%
  filter(!grepl("Corrigenda",title)) %>%
  filter(!grepl("Obituary",title)) %>%
  filter(!grepl("Congress",title))

fd <- fd %>%
  filter(!lang == "fre") %>%
  filter(!lang == "ger")

#View(fd)
#View(j_phil_articles)

# Convert file to character to avoid cast_dtm bug

fd$file <- as.character(fd$file)

# Add a column for journal name

fd <- fd %>%
  mutate(journal = journals$fullname[j])

all_metadata <- rbind(fd, all_metadata)

}

all_metadata <- all_metadata %>%
  filter(!lang == "fre") %>%
  filter(!lang == "ger")

save(all_metadata, file  = "all_journals_article_list.RData")

