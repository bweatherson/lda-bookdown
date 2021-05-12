bg <- epistemology_yeargamma_postwar %>%
  mutate(topic = as.numeric(topic)) %>%
  inner_join(the_categories, by = "topic") %>%
  select(-topic)

print(ggplot(data = filter(epistemology_yeargamma_postwar, topic == jjj), 
             aes(x = year, y = gamrat_local)) +
        geom_smooth(data = epistemology_yeargamma_postwar, 
                    aes(group=topic), 
                    size = 0.1, 
                    color = "grey85", 
                    method = "loess", 
                    alpha = 0.3,
                    se = F,
                    formula = "y ~ x") +
        scale_x_continuous(minor_breaks = 5 * 1:402,
                           expand = expansion(mult = c(0.01, 0.01))) +
        scale_y_continuous(expand = expansion(mult = c(0.01, .03))) +
        epistemologystyle +
        geom_point(size = 1.5, colour = hcl(h = (jjj-1)*(9)+15, l = 65, c = 100)) +
        geom_smooth(se = F, 
                    formula = "y ~ x", 
                    size = 0.5, 
                    method = "loess",
                    colour = hcl(h = (jjj-1)*(9)+15, l = 65, c = 100)) +
        theme(legend.position="none") +
        labs(x = element_blank(), 
             y = "Weighted Number of Articles")
)

temp <- filter(epistemology_yeargamma_postwar, topic == jjj)

alt_text <- paste0(
  "A scatterplot showing the proportion of epistemology articles that are in the epistemology subtopic ",
  cate_nam,
  " each year from 1945-2013. The average value is ",
  percent(
    mean(
      temp$gamrat_local
    ),
    accuracy = 0.1
  ),
  ", and the median value is ",
  percent(
    median(
      temp$gamrat_local
    ),
    accuracy = 0.1
  ),
  ". It reaches a peak value of ",
  percent(
    slice_max(
      temp, gamrat_local, n = 1
    )$gamrat_local[1],
    accuracy = 0.1
  ),
  " in ",
  slice_max(
    temp, gamrat_local, n = 1
  )$year[1],
  ", and has a minimum value of ",
  percent(
    slice_min(
      temp, gamrat_local, n = 1
    )$gamrat_local[1],
    accuracy = 0.1
  ),
  " in ",
  slice_min(
    temp, gamrat_local, n = 1
  )$year[1],
  "."
)
  
cat("\n\n**Characteristic Articles** \n \n")
for (jj in 1:10){
  cat(jj,". ", epistemology_the_articles$citation[jjj*10 + jj - 10], " \n", sep="")
}

these_cites <- epistemology_high_cite_gamma %>% 
  as_tibble() %>%
  filter(topic == jjj) %>%
  arrange(desc(Cites))

if (nrow(these_cites) > 0){
  cat(" \n")
  cat("**Highly Cited Articles** \n \n")
  for (jj in 1:nrow(these_cites)){
    cat(jj,". ", these_cites$citation.x[[jj]], " (", these_cites$gamma[[jj]], ")\n", sep="")   
  }
}