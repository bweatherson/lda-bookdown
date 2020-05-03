if (jjj < 10){topic_crossref <- paste0("#topic0",jjj)}
if (jjj > 9){topic_crossref <- paste0("#topic",jjj)}


cat("## Topic ", jjj, " - ", the_categories$subject[[jjj]], " {", topic_crossref, "}\n", sep="")

if(!the_categories$cat_num[jjj] == 13){
  cat_nam <- the_categories$cat_name[jjj]
}
if(the_categories$cat_num[jjj] == 13){
  temp_category_tibble <- tibble(
    topic = (jjj*100+1):(jjj*100+2))
  temp_category_tibble <- temp_category_tibble %>%
    inner_join(the_categories, by = "topic")
  cat_nam <- paste(temp_category_tibble$cat_name, sep = "/", collapse = "/")
}

cat("**Category**: ", cat_nam, "\n\n")

distinctive <- distinctive_topics[(jjj-1)*15 + 1, 1]
for (jj in 2:15){
  distinctive <- paste(distinctive, distinctive_topics[(jjj-1)*15 + jj, 1], sep = ", ")
}

cat("**Keywords**: ", distinctive, "\n\n")

require(toOrdinal)

cat("**Number of Articles**: ", overall_stats$r_count[jjj], " \n <br>", sep="")
cat("**Percentage of Total**: ", 100*overall_stats$r_percent[jjj], "%\n <br> ", sep="")
cat("**Rank**: ", toOrdinal(overall_stats$r_rank[jjj]), "\n\n", sep="")

cat("**Weighted Number of Articles**: ", overall_stats$w_count[jjj], "\n <br>", sep="")
cat("**Percentage of Total**: ", 100*overall_stats$w_percent[jjj], "%\n <br>", sep="")
cat("**Rank**: ", toOrdinal(overall_stats$w_rank[jjj]), "\n\n", sep="")

cat("**Mean Publication Year**: ", overall_stats$mean_y[jjj], "\n <br>", sep="")
cat("**Weighted Mean Publication Year**: ", overall_stats$wy[jjj], "\n <br>", sep="")
cat("**Median Publication Year**: ", overall_stats$median_y[jjj], "\n <br>", sep="")
cat("**Modal Publication Year**: ", overall_stats$modal_y[jjj], "\n\n", sep="")

cat("**Topic with Most Overlap**: ",
    the_categories$subject[closest_neighbour$othertopic[jjj]], 
    " (", 
    round(closest_neighbour$g[jjj],4),
    ")\n <br>",
    sep="")
cat("**Topic this Overlaps Most With**: ",
    the_categories$subject[closest_neighbour_inverse$topic[jjj]], 
    " (", 
    round(closest_neighbour_inverse$g[jjj],4),
    ")\n <br>",
    sep="")
cat("**Topic with Least Overlap**: ",
    the_categories$subject[furthest_neighbour$othertopic[jjj]], 
    " (", 
    round(furthest_neighbour$g[jjj],5),
    ")\n <br>",
    sep="")
cat("**Topic this Overlaps Least With**: ",
    the_categories$subject[furthest_neighbour_inverse$topic[jjj]], 
    " (", 
    round(furthest_neighbour_inverse$g[jjj],5),
    ")\n\n <br>",
    sep="")

yupper <- max(filter(journalgamma_frequency, topic == jjj)$gamfre, 0.25, na.rm=TRUE)

indiv_journal_graphs <- ggplot(data = filter(journalgamma_frequency, topic == jjj) %>% drop_na(), aes(x = year, y = gamfre))  +
  geom_point(size = 0.2, colour = hcl(h = (jjj-1)*(360/cats)+15, l = 65, c = 100)) +
  theme(legend.position="none") +
  coord_cartesian(ylim=c(0, yupper), expand = TRUE) +
  labs(x = "Year", y = "Weighted Proportion of Articles", title = paste0("Articles from topic ",jjj," in each Journal")) +
  facet_wrap(~journal, ncol = 3, labeller = as_labeller(journal_short_names))
print(indiv_journal_graphs)

indiv_topic_graphs <- ggplot(data = filter(weight_ratio, topic == jjj), aes(x = year, y = f))  +
  geom_point(size = 1, colour = hcl(h = (jjj-1)*(360/cats)+15, l = 65, c = 100)) +
  theme(legend.position="none") +
  coord_cartesian(ylim=c(0, max(filter(weight_ratio, topic == jjj)$f, 0.05, na.rm=TRUE)), expand = TRUE) +
  #      coord_cartesian(ylim=c(0, 0.25), expand = TRUE) +
  labs(x = "Year", y = "Weighted Proportion of Articles", title = paste0("Articles from topic ",jjj))
print(indiv_topic_graphs)

cat(" \n \n")
# cat("**Characteristic Articles for Topic ", jjj, "** \n \n")
char_art <- relabeled_articles %>%
  filter(topic == jjj) %>%
  mutate(score = gamma * log10(length + 1)) %>%
  arrange(desc(score))
article_list <- char_art$citation
#  print(kable(article_list, col.names = NULL, booktabs = T, linesep = "") %>%
#        kable_styling(latex_options = c("striped", "hold_position")))

# Make this be more than 15
# Have DT of citation and probability
# Have 15 per page I guess


temp_dt <- datatable(select(char_art, year, citation, gamma),           
          colnames = c("Year", "Article", "Probability"), 
          rownames = FALSE,
          options = list(columnDefs = list(list(className = 'dt-left', targets = 0:2)),
                         pageLength = 10
                         ),
          caption = htmltools::tags$caption(paste0("Characteristic Articles for Topic ",jjj), style = "font-weight: bold")
    )%>%
      formatSignif('gamma',4) %>%
      formatStyle(1:3,`text-align` = 'left')

# This is how I used to do the list of articles
#for (jj in 1:15){
#  cat(jj,". ", article_list[jj], " \n", sep="")
#}

cat("\n ")


these_cites <- high_cite_gamma %>% 
  filter(topic == jjj) %>%
  as_tibble() %>%
  arrange(desc(Cites)) %>%
  select(year = Year, citation.x, gamma)
  
if (nrow(these_cites) > 0){
#  cat("\n**Highly Cited Articles in Topic ", jjj, "** \n \n")
#  for (jj in 1:nrow(these_cites)){
#    cat(jj,". ", these_cites$citation.x[[jj]], " (", these_cites$gamma[[jj]], ") \n", sep="")   
#  }
high_table <- datatable(these_cites,           
                        colnames = c("Year", "Article", "Probability"), 
                        rownames = FALSE,
                        options = list(columnDefs = list(list(className = 'dt-left', targets = 0:2)),
                                       pageLength = 5
                        ),
                        caption = htmltools::tags$caption(paste0("Highly Cited Articles for Topic ",jjj), style = "font-weight: bold")
)%>%
  formatSignif('gamma',4) %>%
  formatStyle(1:3,`text-align` = 'left')
is_high_cites <- 1
}
if (nrow(these_cites) == 0){is_high_cites <- 0}

