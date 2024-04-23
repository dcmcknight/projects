library(dplyr)
countyfactsDF = read.csv("/Users/drewmcknight1/Desktop/BUAD621/homework/data/county_facts.csv")
primaryresultsDF = read.csv("/Users/drewmcknight1/Desktop/BUAD621/homework/data/primary_results.csv")

##Question 1
primaryresultsDF %>%
  filter(party == 'Democrat') %>%
  group_by(candidate) %>%
  summarise(sumVotes = sum(votes)) %>%
  ungroup() %>%
  arrange(desc(sumVotes))

##Question 2
countyfactsDF %>%
  select(stateAbbrev, medianHouseholdIncome, areaName) %>%
  filter(stateAbbrev == 'NE') %>%
  arrange(desc(medianHouseholdIncome))

##Question 3
countyfactsDF %>% left_join(primaryresultsDF) %>% 
  select(stateAbbrev, areaName, party, votes) %>%
  filter(!is.na(party), party == 'Democrat') %>%
  group_by(stateAbbrev, areaName) %>%
  summarise(sumVotes = sum(votes)) %>%
  ungroup() %>%
  arrange(desc(sumVotes))
  
##Question 4
countyfactsDF %>% left_join(primaryresultsDF) %>% 
  select(areaName, party, votes) %>%
  filter(party == 'Democrat') %>%
  group_by(areaName) %>%
  summarise(sumVotes = sum(votes)) %>%
  ungroup() %>%
  arrange(desc(sumVotes))

##Question 5
countyfactsDF %>%
  select(stateAbbrev, medianHouseholdIncome, areaName) %>%
  filter(stateAbbrev == 'IA') %>%
  arrange(desc(medianHouseholdIncome))

##Question 6
countyfactsDF %>% left_join(primaryresultsDF) %>% 
  select(stateAbbrev, areaName, party, votes) %>%
  filter(!is.na(party), party == 'Republican') %>%
  group_by(stateAbbrev, areaName) %>%
  summarise(sumVotes = sum(votes)) %>%
  ungroup() %>%
  arrange(desc(sumVotes))
  