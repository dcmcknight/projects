library(tidyverse)
library(greta)
library(causact)

graph = dag_create() %>% dag_node("RandomVariable",rhs = gamma(1,1))

graph %>% dag_render()

drawsDF = graph %>% dag_greta(mcmc = TRUE)

median(drawsDF %>% pull(1))
