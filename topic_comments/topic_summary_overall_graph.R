require(scales)
indiv_topic_graphs <- ggplot(data = filter(weight_ratio, topic == jjj), aes(x = year, y = y))  +
#  theme_minimal() +
  spaghettistyle +
  theme(legend.position="none",
        plot.title = element_text(colour = hcl(h = (jjj-1)*(360/cats)+15, l = 65, c = 100))) +
  geom_point(size = 1, colour = hcl(h = (jjj-1)*(360/cats)+15, l = 65, c = 100)) +
  coord_cartesian(ylim=c(0, max(filter(weight_ratio, topic == jjj)$y, 0.07, na.rm=TRUE)), expand = TRUE) +
  labs(x = element_blank(), y = "Weighted proportion of articles", title = the_categories$subject[jjj]) +
  scale_x_continuous(minor_breaks = 10 * 188:201,
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(minor_breaks = 0.005 * 1:100,
                     expand = expansion(mult = c(0.01, .03)))

print(indiv_topic_graphs)

max_year <- filter(weight_ratio, topic == jjj) %>% 
  filter(y == max(filter(weight_ratio, topic == jjj)$y))

min_year <- filter(weight_ratio, topic == jjj) %>% 
  filter(y == min(filter(weight_ratio, topic == jjj)$y))

alt_text <- paste0(
  "A scatterplot showing which proportion of articles each year are in the topic ", 
  the_categories$sub_lower[jjj],
  ". The x-axis shows the year, the y-axis measures the proportion of articles each year in this topic. There is one dot per year. The highest value is in ",
  max_year$year[1],
  " when ",
  scales::percent(max_year$y[1], accuracy = 0.1),
  " of articles were in this topic. The lowest value is in ",
  min_year$year[1],
  " when ",
  scales::percent(min_year$y[1], accuracy = 0.1),
  " of articles were in this topic. The full table that provides the data for this graph is available in Table A.",
  jjj,
  " in Appendix A."
)
