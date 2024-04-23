
library(causact)
library(tidyverse)

## show graphical model
dag_create() %>%
  dag_node("Likes","x",
           obs = TRUE) %>%
  dag_node("Viral","y") %>%
  dag_edge("y","x") %>%
  dag_render()

## represent two models of the world, sunny and cloudy
## prior belief for each listed in that order
priorProb = c("viral" = 0.95,
              "non-viral" = 0.10)

## here is a likelihood given sales and conditional on
## whether it was sunny or not
likeFun = function(likes) {
  likelihood = c(  ## below functions are assumed given
    "viral" = prod(0.035 / 1000 * likes),
    "non-viral" =  prod(0.035 - 0.02 / 1000 * likes))
  return(likelihood)
}

# show same as video minute 7:14
likeFun(1000)
priorProb * likeFun(1000) ## numerators in video 7:43

# calculate p(data) as sum of prior*likelihoods for
# all of the models of the world (i.e. viral or non-viral)
pdata = function(likes) {
  pEvidence = sum(priorProb * likeFun(likes))
  return(pEvidence)
}

#confirm match number in video
pdata(1000)

# now do Bayes Rule for given data of 50
# Compute the posterior.
pModelGivendata = function(likes) {
  posterior = priorProb * likeFun(likes) / pdata(likes)
  return(posterior)
}

#comfirm match number in video
pModelGivendata(likes = 1000)

#make function for plotting as function of sales
getPlot = function(likes) {
  plotDF = tibble(twoModels = c("Viral","Non-Viral"),
                  prior = priorProb,
                  likelihood = likeFun(likes),
                  posterior = pModelGivendata(likes))
  
  #make tidy data to use facet grid by ProbType for plot
  tidyPlotDF = plotDF %>%
    gather("ProbType","Probability",-twoModels) %>%
    mutate(ProbType = fct_relevel(ProbType, c("prior","likelihood","posterior")))# order levels for plot
  
  
  #create named vector for facet labels
  labels = c(prior = "PRIOR\np(model)",likelihood = "LIKELIHOOD\np(data|model)",posterior="POSTERIOR\np(model|data)")
  
  #create plot
  plot = tidyPlotDF %>%
    ggplot(aes(x = twoModels,
               y = Probability,
               fill = ProbType)) +
    geom_col(width = 0.02,
             show.legend = FALSE) +
    facet_grid(rows = vars(ProbType),
               labeller = labeller(ProbType = labels),
               scales = "free_y") +
    geom_text(aes(y = Probability * 1.2,
                  label = signif(Probability,2))) +
    labs(title = paste0("Belief Evolution")) +
    theme_minimal(16)
  
  return(plot)
}  #end plot function definition

getPlot(likes = 1000)

### class question: what would be the posterior probability for cloudy if the prior was based on Buffalo weather where 90% of days are cloudy?


### class question: for the Buffalo example, what would happen if you observed sales from more than one store (assume all stores have identical likelihoods, but their sales figures are independent) and the sales of five stores are given as the following?
buffaloSales = c(50,150,170,140,160)
getPlot(obsSales = buffaloSales)
# show graphical model for last class question
#####
dag_create() %>%
  dag_node("Ice Cream Sales","x",
           obs = TRUE) %>%
  dag_node("Sunny Day","y") %>%
  dag_plate("Buffalo Store","i",
            nodeLabels = "x",
            data = buffaloSales) %>%
  dag_edge("y","x") %>%
  dag_render()





