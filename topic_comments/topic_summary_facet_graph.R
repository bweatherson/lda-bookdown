yupper <- max(filter(journalgamma_frequency, topic == jjj)$gamfre, 0.23, na.rm=TRUE)

facet_labels <- chap_two_facet_labels %>%
  mutate(year = 1944.5, gamfre = yupper)

indiv_journal_graphs <- ggplot(data = filter(journalgamma_frequency, topic == jjj) %>% drop_na(), aes(x = year, y = gamfre))  +
  theme(legend.position="none") +
  coord_cartesian(ylim=c(0, yupper), expand = TRUE) +
  labs(x = "Year", y = "Weighted Proportion of Articles", title = the_categories$subject[jjj]) +
  facet_wrap(~journal, ncol = 3, labeller = as_labeller(journal_short_names)) +
  theme_minimal() +
  theme(plot.title = element_text(size = rel(1), 
                                  face = "bold",
                                  colour = hcl(h = (jjj-1)*(360/cats)+15, l = 65, c = 100),
                                  margin = margin(0, 0, 5, 0)),
        strip.text = element_blank(),
        panel.spacing.x = unit(0.3, "lines"),
        panel.background = element_blank(),
        panel.spacing.y = unit(0.4, "lines"),
        axis.title.x = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1)),
        panel.grid.major = element_line(color = "grey85", size = 0.08),
        panel.grid.minor = element_line(color = "grey88", size = 0.05),
        legend.position="none") +
  geom_point(size = 0.2, colour = hcl(h = (jjj-1)*(360/cats)+15, l = 65, c = 100)) + 
  geom_text(data = facet_labels,
            mapping = aes(label = short_name),
            vjust = "inward", 
            hjust = "middle",
            fontface = "bold", 
            size = 3,
            colour = "grey40")
print(indiv_journal_graphs)