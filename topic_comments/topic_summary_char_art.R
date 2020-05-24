char_art <- relabeled_articles %>%
  filter(topic == jjj) %>%
  mutate(score = gamma * log10(length + 1)) %>%
  arrange(desc(score)) %>%
  mutate(gamma = round(gamma, 3))

temp_dt <- datatable(select(char_art, year, citation, gamma),           
                     colnames = c("Year", "Article", "Probability"), 
                     rownames = FALSE,
                     options = list(columnDefs = list(list(className = 'dt-left', targets = 0:2)),
                                    pageLength = 10
                     ),
                     caption = htmltools::tags$caption(paste0("Characteristic Articles of ",the_categories$subject[jjj]), style = "font-weight: bold")
)%>%
  formatStyle(1:3,`text-align` = 'left')