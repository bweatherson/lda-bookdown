require(scales)
yupper <- max(filter(journalgamma_frequency, topic == jjj)$gamfre, 0.04, na.rm=TRUE)

facet_labels <- chap_two_facet_labels %>%
  mutate(year = case_when(jjj < 5 ~ 2009,
                          jjj < 48 ~1944.5,
                          TRUE ~ 1880),
          gamfre = case_when(jjj < 5 ~ yupper,
                             jjj < 48 ~ yupper * 1.2,
                             TRUE ~ yupper))

facet_labels$journal <- factor(facet_labels$journal, levels = journal_order)

mybreaks <- function(x){
  labeling::extended(dmin = 0, dmax = max(x) * 0.6, m = 3, Q = 0.005 * 1:2)
}

indiv_journal_graphs <- ggplot(data = filter(journalgamma_frequency, topic == jjj) %>% drop_na(), aes(x = year, y = gamfre))  +
  facetstyle +
  theme(legend.position="none",
        strip.text = element_blank(),
        panel.spacing.x = unit(0.3, "lines"),
        panel.background = element_blank(),
        panel.spacing.y = unit(0.4, "lines"),
        plot.title = element_text(colour = hcl(h = (jjj-1)*(360/cats)+15, l = 65, c = 100))) +
#  coord_cartesian(ylim=c(0, yupper), expand = TRUE) +
  scale_y_continuous(breaks = mybreaks,
                     minor_breaks = 0.01 * 1:30) +
  labs(x = element_blank(), y = "Weighted Proportion of Articles", title = the_categories$subject[jjj]) +
  geom_point(size = 0.2, colour = hcl(h = (jjj-1)*(360/cats)+15, l = 65, c = 100)) + 
  facet_wrap(~journal, ncol = 3, labeller = as_labeller(journal_short_names)) +
  geom_text(data = facet_labels,
            mapping = aes(label = short_name),
            vjust = "inward", 
            hjust = "inward",
            fontface = "bold", 
            size = 3,
            colour = "grey40")
print(indiv_journal_graphs)

temp <- filter(journalgamma_frequency, topic == jjj) %>% drop_na()

temp_by_year <- temp %>% 
  ungroup() %>% 
  group_by(year) %>% 
  summarise(m = mean(gamfre))

temp_by_journal <- temp %>% 
  ungroup() %>% 
  group_by(journal) %>% 
  summarise(m = mean(gamfre))

alt_text_journals <- paste0(
  "A set of twelve scatterplots showing the proportion of articles in each journal in each year that are in the topic ",
  the_categories$subject[jjj],
  ". There is one scatterplot for each of the twelve journals that are the focus of this book.",
  " In each scatterplot, the x-axis is the year, and the y-axis is the proportion of articles in that year in that journal in this topic.",
  " This topic appears most frequently in ",
  slice_max(temp_by_journal, m, n = 1)$journal,
  " where it makes up, on average, ",
  percent(slice_max(temp_by_journal, m, n = 1)$m, accuracy = 0.1),
  " of the articles. And it appears least frequently in ",
  slice_min(temp_by_journal, m, n = 1)$journal,
  " where it makes up, on average, ",
  percent(slice_min(temp_by_journal, m, n = 1)$m, accuracy = 0.1),
  " of the articles. It reaches its zenith in year ",
  slice_max(temp_by_year, m, n = 1)$year,
  " when it makes up, on average across the journals, ",
  percent(slice_max(temp_by_year, m, n = 1)$m, accuracy = 0.1),
  " of the articles. And it hits a minimum in year ",
  slice_min(temp_by_year, m, n = 1)$year,
  " when it makes up, on average across the journals, ",
  percent(slice_min(temp_by_year, m, n = 1)$m, accuracy = 0.1),
  " of the articles. The full table that provides the data for these graphs is available in Appendix A.",
  jjj,
  " - ",
  the_categories$subject[jjj] 
)
