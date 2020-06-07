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