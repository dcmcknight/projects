library(greta)
library(causact)

titanicDF = read.csv("/Users/drewmcknight1/Desktop/BUAD621/homework/data/titanic.csv")

graph = dag_create() %>%
  dag_node("Passenger Survived","x",
           rhs = bernoulli(theta),
           data = titanicDF$Survived) %>%
  dag_node("Pass. Surv. Prob.","theta",
           rhs = beta(2,2),
           child = "x") %>%
  dag_plate("Passenger Class", "k",
            data = titanicDF$Pclass,
            nodeLabels = "theta",
            addDataNode = TRUE)  %>%
  dag_plate("Observations", "i",
            nodeLabels = c("x","k")) 

graph %>% dag_render()

drawsDF = graph %>% dag_greta()

drawsDF %>% dagp_plot()

drawsDF %>% mutate(abovetheta = ifelse(theta_1>=(theta_2 + 0.10),1,0)) %>%
  summarize(avgAbovetheta = mean(abovetheta))
