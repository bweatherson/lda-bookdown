jjj <- 2

temp <-  filter(weight_ratio, topic == jjj) %>% 
  mutate(year2 = year^2)

temper <- lm(y ~ year + year2, data = temp)

lma <- as.numeric(temper$coefficients[3])
lmb <- as.numeric(temper$coefficients[2])

lmpeak <- round(-lmb / (2 * lma))

lma
lmpeak


# There basically aren't any parabolas
# If the a value is positive, it's single peaked.