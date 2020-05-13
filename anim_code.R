decisive_csv <- read_csv("decisive.csv")

animation_input <- weight_numerator %>%
  mutate(topic = as.numeric(topic)) %>%
  arrange(topic, year) %>%
  group_by(topic) %>%
  mutate(ry = roll_mean(y, 5, na.rm = TRUE, fill = NA)) %>%
  ungroup() %>%
  inner_join(decisive_csv, by = "topic")

for (jjj in 11:90){

thecolor <- hcl(h = (decisive_csv$cat_num[jjj]-1)*(360/12)+15, l = 65, c = 100)

animation_bg <- animation_input %>%
  filter(topic < jjj) %>%
  mutate(revealer = 1878)

animation_foreground <- animation_input %>%
  filter(topic == jjj) %>%
  mutate(revealer = as.numeric(year))

the_gif <- ggplot(animation_foreground %>% drop_na(),
                  aes(x = year, y = ry, group = topic, color = thecolor, na.rm = TRUE)) +
  scale_x_continuous(limits = c(1878, 2011),
                     expand = expansion(mult = c(0.01, 0.01)),
                     breaks = 1:100 * 20) +
  scale_y_continuous(limits = c(0,28.3),
                     expand = expansion(mult = c(0.01, 0.01)),
                     breaks = 1:10 * 10) +
  geom_line(data = animation_bg, size = 0.2, alpha = 0.5, color = "grey70", na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE, color = thecolor) +
  geom_line(size = 1, na.rm = TRUE, color = thecolor) +
  theme_minimal() +
  theme(legend.position = "None",
        panel.grid.major = element_line(color = "grey85", size = 0.1),
        panel.grid.minor = element_line(color = "grey85", size = 0.05),
        plot.title = element_text(size = rel(2), 
                                  face = "bold",
                                  margin = margin(0, 0, 5, 0)),
        axis.text = element_text(size = rel(1.5))) +
  labs(title = decisive_csv$subject[jjj], x = NULL, y = NULL) +
  transition_reveal(revealer)
    
gifname <- paste0(jjj,".gif", sep = "")

animate(the_gif, 
        nframes = 77, 
        start_pause = 0, 
        end_pause = 13, 
        duration = 3.6, 
        fps = 25, 
        detail = NULL, 
        width = 480,
        height = 480,
        gifski_renderer(gifname))
}

jjj <- 1

thecolor <- hcl(h = (decisive_csv$cat_num[jjj]-1)*(360/12)+15, l = 65, c = 100)

# animation_bg <- animation_input %>%
#   filter(topic < jjj) %>%
#   mutate(revealer = 1878)

animation_foreground <- animation_input %>%
  filter(topic == jjj) %>%
  mutate(revealer = as.numeric(year))

the_gif <- ggplot(animation_foreground %>% drop_na(),
                  aes(x = year, y = ry, group = topic, color = thecolor, na.rm = TRUE)) +
  scale_x_continuous(limits = c(1878, 2011),
                     expand = expansion(mult = c(0.01, 0.01)),
                     breaks = 1:100 * 20) +
  scale_y_continuous(limits = c(0,28.3),
                     expand = expansion(mult = c(0.01, 0.01)),
                     breaks = 1:10 * 10) +
#  geom_line(data = animation_bg, size = 0.2, alpha = 0.5, color = "grey70", na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE, color = thecolor) +
  geom_line(size = 1, na.rm = TRUE, color = thecolor) +
  theme_minimal() +
  theme(legend.position = "None",
        panel.grid.major = element_line(color = "grey85", size = 0.1),
        panel.grid.minor = element_line(color = "grey85", size = 0.05),
        plot.title = element_text(size = rel(2), 
                                  face = "bold",
                                  margin = margin(0, 0, 5, 0)),
        axis.text = element_text(size = rel(1.5))) +
  labs(title = decisive_csv$subject[jjj], x = NULL, y = NULL) +
  transition_reveal(revealer)

gifname <- paste0(jjj,".gif", sep = "")

animate(the_gif, 
        nframes = 77, 
        start_pause = 0, 
        end_pause = 13, 
        duration = 3.6, 
        fps = 25, 
        detail = NULL, 
        width = 480,
        height = 480,
        gifski_renderer(gifname))

commd <- "1.gif"

for (jjj in 2:90){
  commd <- paste0(commd," ",jjj,".gif")
}

commd