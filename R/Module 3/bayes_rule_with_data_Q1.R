library("tidyverse")

thetaVals = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)

#CALCULATE PRIOR
# pTheta is the vector of prior probabilities on the theta values.
pTheta = c(0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125) # Makes a uniform belief distribution.

#SPECIFY OBSERVED DATA
# Specify the data. The follwoing are 3 successes and 9 failure
# and 6 success and 4 failures.
#Data = c(1,1,1,0,0,0,0,0,0,0,0,0)
#Data = c(rep(1,6), rep(0,4))
Data = c(1,1,1,0,0)  ## one store is a failure
nSuccess = sum( Data == 1 )    ##count # of successful stores
nFail = sum( Data == 0 )    ##count # of failure stores

#CALCULATE LIKELIHOOD
# Compute the likelihood of the data for each value of theta:
pDataGivenTheta = thetaVals^nSuccess * (1-thetaVals)^nFail

#CALCULATE POSTERIOR PROBABILITIES
# Compute the posterior:
pData = sum( pDataGivenTheta * pTheta )
pThetaGivenData = pDataGivenTheta * pTheta / pData   # This is Bayes' rule!

#make data frame for plotting
plotDF = data.frame(thetaVals, prior = pTheta,likelihood = pDataGivenTheta, posterior = pThetaGivenData)

#make tidy data to use facet grid by ProbType for plot
tidyPlotDF = plotDF %>%
  gather("ProbType","Probability",-thetaVals) %>%
  mutate(ProbType = fct_relevel(ProbType, c("prior","likelihood","posterior")))# oreder levels for plot


#create named vector for facet labels
labels = c(prior = "PRIOR\np(theta)",likelihood = "LIKELIHOOD\np(Data|theta)",posterior="POSTERIOR\np(theta|Data)")

#create plot
tidyPlotDF %>%
  ggplot(aes(x = thetaVals, y = Probability, fill = ProbType)) +
  geom_col(width = 0.02, show.legend = FALSE) +
  coord_cartesian(xlim = c(0,1)) + ## create coordinate system
  facet_grid(rows = vars(ProbType),
             labeller = labeller(ProbType = labels),
             scales = "free_y") +
  geom_text(aes(y = Probability * 1.2, label = signif(Probability,2))) +
  labs(title = paste0("Belief Evolution -- ",nFail," failure(s) -- ",nSuccess," success(es)")) +
  theme_minimal(16)

