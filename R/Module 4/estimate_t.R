library(greta)
library(causact)

tDF = read.csv("/Users/drewmcknight1/Desktop/BUAD621/homework/data/estimateTheT.csv")

graph = dag_create() %>%
  dag_node("Student t Data","x",
           rhs = student(nu,mu,sigma),
           data = tDF$x) %>%
  dag_node("Degrees of Freedom","nu",
           rhs = gamma(2,0.1),
           child = "x") %>%
  dag_node("Mean","mu",
           rhs = normal(50,24.5),
           child = "x") %>%
  dag_node("Std. Dev","sigma",
           rhs = normal(0,50),
           child = "x") %>%
  dag_plate("Observation","i",
            nodeLabels = "x")

graph %>% dag_render()

drawsDF = graph %>% dag_greta()

drawsDF %>% dagp_plot()

drawsDF %>% mutate(above46 = ifelse(mu>46,1,0)) %>%
  summarize(avgAbove46 = mean(above46))
