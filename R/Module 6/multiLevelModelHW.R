library(causact)
library(greta)

graph2 = dag_create() %>%
  dag_node("Number of Signups","k",
           rhs = binomial(nTrials,theta),
           data = gymDF$nSigned) %>%
  dag_node("Signup Probability","theta",
           child = "k",
           rhs = 1 / (1+exp(-y))) %>%
  dag_node("Number of Trials","nTrials",
           child = "k",
           data = gymDF$nTrialCustomers) %>%
  dag_node("Linear Predictor","y",
           rhs = alpha + beta * x,
           child = "theta") %>%
  dag_node("Yoga Stretch Flag","x",
           data = gymDF$yogaStretch,
           child = "y") %>%
  dag_node("Gym Intercept","alpha",
           rhs = normal(mu_alpha,sd_alpha),
           child = "y") %>%
  dag_node("Gym Yoga Slope Coeff","beta",
           rhs = normal(mu_beta,sd_beta),
           child = "y") %>%
  dag_node("Avg Crossfit Intercept","mu_alpha",
           rhs = normal(-1,1.5),
           child = "alpha") %>%
  dag_node("Avg Crossfit Yoga Slope","mu_beta",
           rhs = normal(0,0.75),
           child = "beta") %>%
  dag_node("SD Crossfit Intercept","sd_alpha",
           rhs = uniform(0,3),
           child = "alpha") %>%
  dag_node("SD Crossfit Yoga Slope","sd_beta",
           rhs = uniform(0,1.5),
           child = "beta") %>%
  dag_plate("Gym","j",
            nodeLabels = c("alpha","beta"),
            data = gymDF$gymID,
            addDataNode = TRUE) %>%
  dag_plate("Observation","i",
            nodeLabels = c("k","x","j",
                           "nTrials","theta","y"))
graph2 %>% dag_render()

drawsDF = graph2 %>% dag_greta()
drawsDF %>% dagp_plot()

drawsDF %>% select(mu_alpha,mu_beta,sd_alpha,sd_beta) %>% dagp_plot()

## Pick random mu_alpha and mu_beta. In this case, mu_alpha = -1.5, 
# mu_beta = 0.5
# y = alpha[j] + beta[j] * x

y = -1.5 + 0.5*10

##Inverse logit function
#Signup Success Probablity - theta = 1/(1 + exp(-y))
theta = 1/(1 + exp(-y))

