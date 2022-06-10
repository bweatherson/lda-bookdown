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
  )%>%
    formatSignif('gamma',4) %>%
    formatStyle(1:3,`text-align` = 'left')
  is_high_cites <- 1

  cat("<table style=\'margin-bottom:0px\'>",
      paste0("<caption>",
             paste0("(#tab:t",
                    str_sub(topic_crossref,-2),
                    "e)"
             ),
             paste0("Highly cited articles in the ",
                    the_categories$sub_lower[jjj],
                    " topic."),
             "</caption>",
             "</table>", sep =" ")
  )
  }
if (nrow(these_cites) == 0){is_high_cites <- 0}