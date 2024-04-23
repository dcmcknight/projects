library(dplyr)
library(causact)
# extract relevant data
baseDF = causact::baseballData %>% as_tibble() %>%
  filter(Home == "COL") %>%
  mutate(runsInGame = HomeScore + VisitorScore) %>%
  select(Home, runsInGame) %>%
  gather(key = Home, value = runsInGame)

# plot data
baseDF %>% ggplot() +
  geom_histogram(aes(x=runsInGame), breaks = 0:30)

library(greta)
graph = dag_create() %>%
  dag_node("Runs Scored","k",
           rhs = poisson(lambda),
           data = baseDF$runsInGame) %>%
  dag_node("Avg runs","lambda",
           rhs = uniform(0,50),
           child = "k") %>%
  dag_plate("Observation","i",
            nodeLabels = "k")

graph %>% dag_render()
drawsDF = graph %>% dag_greta()

drawsDF %>% mutate(above11 = ifelse(lambda>=11,1,0)) %>%
  summarize(avgAbove11 = mean(above11))
