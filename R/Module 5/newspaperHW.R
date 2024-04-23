# quick coding example
library(tidyverse)
library(greta)
library(causact)
theme_set(theme_minimal(16))

# some fake demand data
demandData = c(70,55,80,60,61)

# create geenrative DAG
graph = dag_create() %>%
  dag_node("Demand","x",
           rhs = normal(mu,sigma),
           data = demandData) %>%
  dag_node("Exp Demand","mu",
           rhs = uniform(50,100)) %>%
  dag_node("Std Dev of Demand","sigma",
           rhs = gamma(3,0.2)) %>%
  dag_edge(from = c("mu","sigma"),
           to = "x") %>%
  dag_node("Future Demand Simul","x_sim",
           rhs = normal(mu,sigma)) %>%
  dag_edge(from = c("mu","sigma"),
           to = "x_sim") 

graph %>% dag_render()

drawsDF = graph %>% dag_greta()

drawsDF %>% dagp_plot()

# revenue function - takes vectors of demand and orders
# returns vector of revenues
revFunction = function(demand, order) {
  rev = 2 * pmin(demand,order) + 
    (ifelse(demand<order,(pmax(demand,order) - pmin(demand,order)) * 0.25,
            0))
  return(rev)
}

revFunction(c(3,4,5),c(4,7,8)) #est passes

# expense function - inputs q outputs expense $
expenseFunction = function(order) {
  expense = 1 * order
  return(expense)
}

expenseFunction(c(2,4,8))

profitFunction = function(rev,exps) {
  profit = rev - exps
  return(profit)
}

profitFunction(c(4,5,6), c(1,2,1))
## create posterior draws for outcomes of interest

## add to previous
outcomeDF = expand_grid(x_sim = drawsDF$x_sim,
                        orderQty = seq(50,100,by=10)) %>%
  mutate(revenue = revFunction(x_sim,orderQty),
         expenses = expenseFunction(orderQty),
         profit = revenue - expenses) %>%
  group_by(orderQty) %>%
  summarize(q05 = stats::quantile(profit,0.05),
            q50 = stats::quantile(profit,0.50),
            q95 = stats::quantile(profit,0.95),
            expense = first(expenses))

outcomeDF %>% mutate(under20 = ifelse(x_sim<orderQty,1,0)) %>%
  summarize(avgUnder20 = mean(under20))

### lets plot the three outcomes of interest for each decision
outcomeDF %>%
  ggplot(aes(x = orderQty)) +
  geom_linerange(aes(ymin = q05, ymax = q95),
                 size = 2, color = "cadetblue") +
  geom_point(aes(y = q50),
             size = 4, color = "navyblue") +
  labs(y = "profit")

##Question 3
drawsDF %>% mutate(under70 = ifelse(x_sim<70,1,0)) %>%
  summarize(avgUnder70 = mean(under70))

