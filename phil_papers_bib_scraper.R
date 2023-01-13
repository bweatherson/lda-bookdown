require(tidyverse)
require(rvest)
the_bib <- c()

for (i in 1988:1989){
  web_source <- paste0("https://philpapers.org/asearch.pl?pub=570&year=",i,"&format=bib")
  phil_papers_data <- read_html(web_source) |> html_text()
  the_bib <- c(the_bib, phil_papers_data)
}

cat(the_bib, file = "jphil.bib")