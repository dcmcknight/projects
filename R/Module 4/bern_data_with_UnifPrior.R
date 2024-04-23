library(tidyverse)
library(greta)
library(causact)

successData = c(rep(1,2), rep(0,1))

theta = uniform(min = 0, max = 1)

x = as_data(successData)
distribution(x) = bernoulli(prob = theta)

fit = model()
plot(fit)

draws = mcmc(fit)

drawsDF = draws %>% as.matrix() %>% as_tibble()

drawsDF %>% mutate(under20 = ifelse(theta<0.20,1,0)) %>%
  summarize(avgUnder20 = mean(under20))
