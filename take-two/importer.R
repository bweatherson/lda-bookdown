require(tidyverse)
metadata <-
  list.files(pattern = "*metadata.csv") %>% 
  map_df(~read_csv(.))