library(dplyr)
rawDataDF = read.csv("/Users/drewmcknight1/Desktop/BUAD621/homework/data/Salaries.csv",stringsAsFactors = FALSE, colClasses = c("character","character","character","numeric","numeric","numeric","numeric","numeric","numeric","integer","character","character","character"), na.strings = c("NotProvided"))
set.seed(111)

##Get a random sample of rows to ensure that you are using R and the dplyr packages to do the homework
salaryDF = rawDataDF %>% sample_frac(size = 0.75) %>% as_tibble()

##Question 1
salaryDF %>%
  select(EmployeeName, OvertimePay, TotalPayBenefits, Year) %>% 
  filter(Year == 2014) %>%
  mutate(percent = OvertimePay / TotalPayBenefits) %>%
  arrange(desc(percent))

##Question 2
salaryDF %>%
  select(JobTitle, Year) %>% 
  filter(Year == 2014) %>%
  count(JobTitle, sort = TRUE, name = 'count')

##Question 3
salaryDF %>%
  select(EmployeeName, TotalPay, Year) %>% 
  filter(Year == 2014) %>%
  arrange(desc(TotalPay))

##Question 4
salaryDF %>%
  select(JobTitle, OvertimePay) %>%
  group_by(JobTitle) %>%
  summarize(avgOvertimePay = mean(OvertimePay, na.rm = TRUE)) %>%
  arrange(desc(avgOvertimePay))

##Question 5
salaryDF %>%
  select(JobTitle, BasePay) %>%
  group_by(JobTitle) %>%
  summarize(avgBasePay = mean(BasePay, na.rm = TRUE)) %>%
  arrange(desc(avgBasePay))

##Question 6
salaryDF %>%
  select(TotalPayBenefits, Year, JobTitle) %>% 
  filter(Year == 2014, JobTitle == 'Custodian') %>% 
  arrange(desc(round(TotalPayBenefits,2)))
  
##Question 7
salaryDF %>%
  select(EmployeeName, JobTitle, OvertimePay, TotalPayBenefits, Year) %>% 
  mutate(percent = OvertimePay / TotalPayBenefits) %>%
  filter(Year == 2014, percent >= 0.4) %>%
  count(JobTitle, sort = TRUE, name = 'count')

