library("dplyr")
library("hflights")
?hflights

hflights %>%
  mutate(delayFlag = ifelse(DepDelay > 0,1,0)) %>%
  group_by(DayOfWeek) %>%
  summarise(pctDelay = mean(delayFlag, na.rm = TRUE),count = n()) %>%
  filter(count > 30000) %>%
  arrange(desc(pctDelay))

hflights %>%
  mutate(delayFlag = ifelse(DepDelay > 0,1,0)) %>%
  group_by(Dest) %>%
  summarise(pctDelay = mean(delayFlag, na.rm = TRUE), count = n()) %>%
  filter(count > 50, pctDelay < 0.25) %>%
  arrange(desc(pctDelay))

# note: delayFlag is known as an indicator function
# the average or mean of an indicator function
# is the probabilty of the indicated event occurring
# hence, pctDelay represents percentage/probability
# of rows in which DepDelay > 0. 
# see https://youtu.be/f23Ppvw_Xvc for more info
# on indicator functions