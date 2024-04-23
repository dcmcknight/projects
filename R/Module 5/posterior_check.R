library(greta)
library(causact)
library(dplyr)
library(tidyverse)

# extract relevant data
baseDF = causact::baseballData %>% as_tibble() %>%
  filter(Home == "COL") %>%
  mutate(runsInGame = HomeScore + VisitorScore) %>%
  select(Home, runsInGame) %>%
  gather(key = Home, value = runsInGame)

# plot data
baseDF %>% ggplot() +
  geom_histogram(aes(x=runsInGame), breaks = 0:30)

graph = dag_create() %>%
  dag_node("Count Data","k",
           rhs = poisson(lambda),
           data = baseDF$runsInGame) %>%
  dag_node("Rate Parameter","lambda",
           rhs = uniform(0,50),
           child = "k") %>%
  dag_plate("Observation","i",
            nodeLabels = "k")

graph %>% dag_render()

drawsDF = graph %>% dag_greta()

drawsDF %>% dagp_plot()

lambdaPost = drawsDF %>%  # posterior dist.
  slice_sample(n=20) %>%  # get a random row
  pull(lambda)  # convert from tibble to single value
lambdaPost # print value

simData = rpois(n = 405, lambda = lambdaPost)

# make data frame for ggplot
plotDF = tibble(k_observed = baseDF$runsInGame,
                k_simulated = simData)

# make df in tidy format (use tidyr::pivot_longer)
# so fill can be mapped to observed vs simulated data
plotDF = plotDF %>%
  pivot_longer(cols = c(k_observed,k_simulated),
               names_to = "dataType",
               values_to = "RunCount")  

colors = c("k_observed" = "navyblue", 
           "k_simulated" = "cadetblue")

ggplot(plotDF, aes(x = RunCount)) + 
  geom_density(aes(fill = dataType),
               alpha = 0.5) +
  scale_fill_manual(values = colors)
