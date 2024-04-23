library(tidyverse)
library(greta)
library(causact)

## flavor B = 1, flavor A = 0
successData = c(rep(1,36), rep(0,24))

theta = beta(2,2)

x = as_data(successData)
distribution(x) = bernoulli(prob = theta)

fit = model()
plot(fit)

draws = mcmc(fit)

drawsDF = draws %>% as.matrix() %>% as_tibble()

drawsDF %>% mutate(above50 = ifelse(theta>0.50,1,0)) %>%
  summarize(avgAbove50 = mean(above50))
