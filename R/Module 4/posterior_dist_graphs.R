library(tidyverse)
library(greta)
library(causact)

successData = c(rep(1,2), rep(0,1))

graph = dag_create() %>%  
  dag_node(descr = "Store Data", label = "x", 
           rhs = bernoulli(theta), 
           data = successData) %>%
  dag_node(descr = "Success Probability", label = "theta",  
           rhs = uniform(0,1)) %>%  
  dag_edge(from = "theta",
           to = "x")

graph %>% dag_render()  ## final viz of generative DAG

drawsDF = graph %>% dag_greta() ## runs greta code
drawsDF

pdf("Posterior_distributions_Drew.pdf")

drawsDF %>% 
  ggplot(aes(x=theta)) + 
  geom_density(fill = "cadetblue") + labs(title = "Drew McKnight - Posterior")

# the super quick causact way
drawsDF %>% dagp_plot()

dev.off()

