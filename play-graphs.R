
word_frequency_graphs_trimmed <- function(x, y){
  t <- lapply(x, word_year_frequency) %>% bind_rows() %>% filter(year >= y)
#  h <- lapply(x, frequency_summary) %>% bind_rows() %>% filter(year >= y)
  ggplot(t, aes(x = year, y = f, color = term, group = term)) +
    freqstyle +
    geom_point(size = 0.6, alpha = 0.8) +
    #  geom_hline(yintercept = h$the_mean, col = group) +
    stat_summary(fun = mean, 
                 aes(x = round(2013 + y) / 2, yintercept = ..y.., group = term), 
                 geom = "hline",
                 linetype = "dashed",
                 size = 0.2) +
    scale_x_continuous(minor_breaks = 10 * 1:201,
                       expand = expansion(mult = c(0.01, 0.01))) +
    scale_y_continuous(expand = expansion(mult = c(0.01, .03)),
                       minor_breaks = scales::breaks_pretty(n = 12),
                       breaks = scales::breaks_pretty(n = 3),
                       labels = function(x) ifelse(x > 0, paste0("1/",round(1/x,0)), 0)) +
    #  scale_y_continuous(labels = scale_inverter) +
    labs(x = element_blank(), y = "Word Frequency") +
    theme(legend.title = element_blank())
}

word_frequency_graphs_trimmed(c("lewis", "kripke", "putnam", "frankfurt", "rawls", "thomson"), 1967)
word_frequency_graphs_trimmed(c("probability", "modal", "abortion", "quantifier"), 1953)
word_frequency_graphs_trimmed(c("abortion", "discrimination"), 1953)
word_frequency_graphs_trimmed(c("abortion", "discrimination", "famine"), 1953)
word_frequency_graphs(c("frege", "moore", "wittgenstein"))
word_frequency_graphs(c("moore", "wittgenstein"))
word_frequency_graphs(c("moorean", "wittgensteinian", "fregean"))

