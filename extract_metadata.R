#################################
# Parsing out xml files
# Based on script by John A. Bernau 2018
#################################

# Install / load packages
require(xml2)
require(tidyverse)
require(plyr)
require(dplyr)

# Add every journal that you're using here as an extra line
journals <- tribble(
  ~code, ~fullname,
  "jphil", "Journal of Philosophy",
)

all_metadata <- tibble()

journal_count <- nrow(journals)

for (j in 1:journal_count){
  
  # Identify path to metadata folder and list files
  path1 <- paste0("data/metadata/",journals$code[j])
  files <- list.files(path1)
  
  # Initialize empty set
  final_data <- NULL
  
  # Using the xml2 package: for each file, extract metadata and append row to final_data
  for (x in files){
    path <- read_xml(paste0(path1, "/", x))
    
    # File name - without .xml to make it easier for lookup purposes
    document <- str_remove(str_remove(x, ".xml"),"journal-article-")
    
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
    # Language
    lang <-  xml_find_all(path, xpath = "/article/front/article-meta/custom-meta-group/custom-meta/meta-value") %>%  
      xml_text()
    
    # Bind all together
    article_meta <- cbind(document, type, title, 
                          auth1, auth2, auth3, auth4, year, vol, iss, fpage, lpage, lang)
    
    final_data <- rbind.fill(final_data, data.frame(article_meta, stringsAsFactors = FALSE))
    
    # Print progress 
    if (nrow(final_data) %% 250 == 0){
      print(paste0("Extracting document # ", nrow(final_data)," - ", journals$code[j]))
      print(Sys.time())
    }
  }
  
  # Shorter name
  fd <- c()
  fd <- final_data
  
  # Adjust data types
  fd$type <- as.factor(fd$type)
  fd$year <- as.numeric(fd$year)
  fd$vol <- as.numeric(fd$vol)
  fd$iss <- str_replace(fd$iss, "S", "10") # A hack for special issues
  fd$iss <- as.numeric(fd$iss)
  
  # Convert to numeric (roman numerals converted to NA by default)
  fd$fpage <- str_replace(fd$fpage, "S", "1000") # We are going to replace S with some large number, and then undo it a few lines later
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
  
  # Filter foreign language articles. Can't filter on lang = "eng" because some articles have blank
  fd <- fd %>%
    filter(!lang == "fre") %>%
    filter(!lang == "ger")
  
  # Convert file to character to avoid cast_dtm bug
  fd$document <- as.character(fd$document)
  
  # Add a column for journal name
  fd <- fd %>%
    mutate(journal = journals$fullname[j])
  
  # Put the metadata for this journal with metadata for other journals
  all_metadata <- rbind(fd, all_metadata) %>%
    arrange(year, fpage)
}

save(all_metadata, file  = "my_journals_metadata.RData")

# The rest of this is a bunch of tweaks to make the metadata more readable

my_articles <- all_metadata

# Get Rid of All Caps
my_articles$title <- str_to_title(my_articles$title)
my_articles$auth1 <- str_to_title(my_articles$auth1)
my_articles$auth2 <- str_to_title(my_articles$auth2)
my_articles$auth3 <- str_to_title(my_articles$auth3)

#Get rid of messy spaces in titles
my_articles$title <- str_squish(my_articles$title)
my_articles$auth1 <- str_squish(my_articles$auth1)
my_articles$auth2 <- str_squish(my_articles$auth2)
my_articles$auth3 <- str_squish(my_articles$auth3)

# Note that this sometimes leaves us with duplicated articles in my_articles
# The following is the fix duplication code
my_articles <- my_articles %>% 
  rowid_to_column("ID") %>%
  group_by(document) %>%
  top_n(1, ID) %>%
  ungroup()

# Making a list of authors; uses 'et al' for 4 or more authors
my_articles <- my_articles %>%
  mutate(authall = case_when(
    is.na(auth2) ~ auth1,
    is.na(auth3) ~ paste0(auth1," and ", auth2),
    is.na(auth4) ~ paste0(auth1,", ",auth2," and ",auth3),
    TRUE ~ paste0(auth1, " et al")
  ))

# Code for handling page numbers starting with S
my_articles <- my_articles %>% 
  mutate(citation = case_when(
    journal == "Philosophy of Science" & fpage > 10000 ~ paste0(authall," (",year,") \"", title,"\" ",journal," ",vol,":S",fpage-10000,"-S",lpage-10000,"."),
    journal == "Proceedings of the Aristotelian Society" & year - vol > 1905 ~ paste0(authall," (",year,") \"", title,"\" ",journal," (Supplementary Volume) ",vol,":",fpage,"-",lpage,"."),
    TRUE ~ paste0(authall," (",year,") \"", title,"\" ",journal," ",vol,":",fpage,"-",lpage,".")
  )
  )

# Remove Errant Articles - Duplicates, not English etc that didn't get caught earlier
errant_articles <- c(
  "10.2307_2250251",
  "10.2307_2102671",
  "10.2307_2102690",
  "10.2307_4543952",
  "10.2307_2103816",
  "10.2307_185746",
  "10.2307_3328062"
)

# Last list of things to exclude
my_articles <- my_articles %>%
  filter(!document %in% errant_articles) %>%
  filter(!lang == "spa")

save(my_articles, file="my_articles.RData")