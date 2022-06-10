load("Highly_Cites_articles.RData")

highly_cited <- highly_cited %>% 
  mutate(document = file) %>% 
  mutate(title = str_replace_all(Title, "[/*]", "")) %>% 
  mutate(auth1 = str_replace_all(auth1, "[/*†‡]", "")) %>% 
  mutate(lastlet = str_sub(auth1, -1)) %>% 
  mutate(auth1 = case_when((lastlet == "," | lastlet == " ") ~ str_sub(auth1, end = -2),
                           TRUE ~ auth1)) %>% 
  mutate(auth1 = case_when((str_sub(auth1, -4) == " and" | (str_sub(auth1, -3) == "And")) ~ str_sub(auth1, end = -4),
                           TRUE ~ auth1)) %>% 
  mutate(lastlet = str_sub(auth1, -1)) %>% 
  mutate(auth1 = case_when((lastlet == "," | lastlet == " ") ~ str_sub(auth1, end = -2),
                           TRUE ~ auth1)) %>% 
  mutate(auth2 = str_replace_all(auth2, "[/*†‡]", "")) %>% 
  mutate(lastlet = str_sub(auth2, -1)) %>% 
  mutate(auth2 = case_when((lastlet == "," | lastlet == " ") ~ str_sub(auth2, end = -2),
                           TRUE ~ auth2)) %>% 
  mutate(auth2 = case_when((str_sub(auth2, -4) == " and" | (str_sub(auth2, -3) == "And")) ~ str_sub(auth2, end = -4),
                           TRUE ~ auth2)) %>% 
  mutate(lastlet = str_sub(auth2, -1)) %>% 
  mutate(auth2 = case_when((lastlet == "," | lastlet == " ") ~ str_sub(auth2, end = -2),
                           TRUE ~ auth2)) %>% 
  mutate(lastlet = str_sub(auth2, -1)) %>% 
  mutate(auth2 = case_when((lastlet == "," | lastlet == " ") ~ str_sub(auth2, end = -2),
                           TRUE ~ auth2)) %>% 
  mutate(auth3 = str_replace_all(auth3, "[/*†‡]", "")) %>% 
  mutate(lastlet = str_sub(auth3, -1)) %>% 
  mutate(auth3 = case_when((lastlet == "," | lastlet == " ") ~ str_sub(auth3, end = -2),
                           TRUE ~ auth3)) %>% 
  mutate(auth3 = case_when((str_sub(auth3, -4) == " and" | (str_sub(auth3, -3) == "And")) ~ str_sub(auth3, end = -4),
                           TRUE ~ auth3)) %>% 
  mutate(lastlet = str_sub(auth3, -1)) %>% 
  mutate(auth3 = case_when((lastlet == "," | lastlet == " ") ~ str_sub(auth3, end = -2),
                           TRUE ~ auth3)) %>% 
  mutate(lastlet = str_sub(auth3, -1)) %>% 
  mutate(auth3 = case_when((lastlet == "," | lastlet == " ") ~ str_sub(auth3, end = -2),
                           TRUE ~ auth3)) %>% 
  mutate(auth4 = str_replace_all(auth4, "[/*†‡]", "")) 

# Regenerate authall after all those changes
highly_cited <- highly_cited %>% 
  mutate(authall = case_when(
    is.na(auth2) ~ auth1,
    is.na(auth3) ~ paste0(auth1," and ", auth2),
    is.na(auth4) ~ paste0(auth1,", ",auth2," and ",auth3),
    TRUE ~ paste0(auth1, " et al")
  ))

# Change format to Michigan Publishing preferred style
highly_cited <- highly_cited %>% 
  mutate(adjlpage = case_when(floor(fpage/10) == floor(lpage/10) & fpage < 10000 ~ lpage - 10*floor(lpage/10),
                              floor(fpage/100) == floor(lpage/100) & fpage < 10000 ~ lpage - 100*floor(lpage/100),
                              TRUE ~ lpage)) %>% 
  mutate(citation = case_when(
    journal == "Philosophy of Science" & fpage > 10000 ~ paste0(authall,", ",year,", “", toTitleCase(title),",” ",journal," ",vol,":S",fpage-10000,"–S",lpage-10000,"."),
    journal == "Proceedings of the Aristotelian Society" & year - vol > 1905 ~ paste0(authall,", ",year,", “", toTitleCase(title),",” ",journal," (Supplementary Volume) ",vol,":",fpage,"–",adjlpage,"."),
    #    TRUE ~ paste0(authall," (",year,") \"", title,"\" ",journal," ",vol,":",fpage,"-",lpage,".")
    TRUE ~ paste0(authall,", ",year,", “", toTitleCase(title),",” ",journal," ",vol,":",fpage,"–",adjlpage,".")
  )
  )

highly_cited <- highly_cited %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mca", "McA")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mcb", "McB")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mcc", "McC")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mcd", "McD")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mce", "McE")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mcf", "McF")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mcg", "McG")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mch", "McH")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mci", "McI")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mcj", "McJ")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mck", "McK")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mcl", "McL")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mcm", "McM")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mcn", "McN")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mco", "McO")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mcp", "McP")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mcq", "McQ")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mcr", "McR")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mcs", "McS")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mct", "McT")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mcu", "McU")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mcv", "McV")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mcw", "McW")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mcx", "McX")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mcy", "McY")) %>% 
  mutate(across(.cols = c(Title, auth1:auth4), str_replace, "Mcz", "McZ"))

save(highly_cited, file="highly_cited_articles.RData")
