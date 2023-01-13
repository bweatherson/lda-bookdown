# Parsing out xml files
# Based on a script by John A. Bernau 2018

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
