cate_nam <- epistemology_subjects$subject[jjj]

cat(" \n")
cat("## ", cate_nam, " {#e",jjj,"}\n\n", sep="")

keyword_list <- epistemology_distinctive_topics %>% filter(topic == jjj)
keyword_paste <- paste(keyword_list$term, sep = ", ", collapse = ", ")

cat("**Keywords**: ", keyword_paste, " \n \n")

raw_count <- epistemology_article_count$n[jjj]
weighted_count <- epistemology_article_gamma$g[jjj]

cat("**Number of Articles**: ", raw_count, "  \n")
cat("**Weighted Number of Articles**: ", weighted_count, "  \n  \n")

bg <- epistemology_yeartopics_postwar %>%
  mutate(topic = as.numeric(topic)) %>%
  inner_join(the_categories, by = "topic") %>%
  select(-topic)

print(
  ggplot(data = filter(epistemology_yeartopics_postwar, topic == jjj), 
         aes(x = year, y = tn)) +
    geom_smooth(data = epistemology_yeartopics_postwar, 
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
         y = "Raw Number of Articles")
)

temp <- filter(epistemology_yeartopics_postwar, topic == jjj)

alt_text <- paste0(
  "A scatterplot showing the raw number of articles that are in the epistemology subtopic ",
  cate_nam,
  " each year from 1945-2013. The average value is ",
    round(
      mean(
      temp$tn
      ),
      2
    ),
  ", and the median value is ",
    median(
      temp$tn
    ),
  ". It reaches a peak value of ",
    slice_max(
      temp, tn, n = 1
    )$tn[1],
  " in ",
  slice_max(
    temp, tn, n = 1
  )$year[1],
  ", and has a minimum value of ",
    slice_min(
      temp, tn, n = 1
    )$tn[1],
  " in ",
  slice_min(
    temp, tn, n = 1
  )$year[1],
  "."
)