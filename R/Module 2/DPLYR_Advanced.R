library(dplyr)
countyfactsDF = read.csv("/Users/drewmcknight1/Desktop/BUAD621/homework/data/county_facts.csv")
primaryresultsDF = read.csv("/Users/drewmcknight1/Desktop/BUAD621/homework/data/primary_results.csv")

##Question 1 & 2
countyfactsDF %>% left_join(primaryresultsDF) %>%
  select(stateAbbrev, candidate, party, votes, medianHouseholdIncome) %>%
  filter(party == 'Republican', stateAbbrev == 'IA',
         medianHouseholdIncome > 50000) %>%
  group_by(candidate) %>%
  summarize(sumVotes = sum(votes)) %>%
  mutate(pctOfVotes = sumVotes / sum(sumVotes)) %>%
  ungroup() %>%
  arrange(desc(pctOfVotes))

##Question 3
primaryresultsDF %>%
  group_by(countyID, candidate) %>%
  summarize(sumVotes = sum(votes)) %>%
  mutate(pctOfVotes = sumVotes / sum(sumVotes)) %>%
  filter(candidate == 'Ted Cruz') %>%
  ungroup() %>%
  arrange(desc(pctOfVotes))

##Question 4
countyfactsDF %>% left_join(primaryresultsDF) %>%
  select(stateAbbrev, candidate, party, votes, medianHouseholdIncome) %>%
  filter(party == 'Republican', stateAbbrev == 'NH',
         medianHouseholdIncome > 50000) %>%
  group_by(candidate) %>%
  summarize(sumVotes = sum(votes)) %>%
  mutate(pctOfVotes = sumVotes / sum(sumVotes)) %>%
  ungroup() %>%
  arrange(desc(pctOfVotes))
  
