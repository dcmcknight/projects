# R-SCRIPT OF CODE IN CLIENT DELIVAREABLE
# PROJECT DESCRIPTION

# USEFUL PACKAGES
library(ggplot2)
library(ggrepel)
library(dplyr)
library(tidyverse)
library(causact)
library(greta)

# LOAD DATA
carsDF = readr::read_csv("/Users/drewmcknight1/Desktop/BUAD621/homework/data/carsFixed.csv")
# VIEW QUICK SUMMARY OF DATA
carsDF %>%
  group_by(shopID) %>%
  summarize(numberOfObservations = n(),
            numberOfBossVisits = sum(boss))

carsDF %>%
  group_by(shopID) %>%
  summarize(numberOfObservations = n(),
            carsFixed = sum(carsFixed))
##This chart is for future reference, not for project - bar chart showing shops by number of cars fixed and number of boss visits as overlay line.
carsDF %>%
  group_by(shopID) %>%
  summarize(numberOfObservations = n(),
            numberOfBossVisits = sum(boss), carsFixed) %>%
  ggplot() +
  geom_col(aes(x = shopID, y = carsFixed), size = 1) +
  geom_line(aes(x = shopID, y = numberOfBossVisits*150, color = shopID), size = 1.5, color="red") + 
  scale_y_continuous(name = "Cars Fixed", sec.axis = sec_axis(trans=~./150, 
                                                              name = "Boss Visits"))

##Graph 1
bossx = factor(carsDF$boss == 1, labels = c("Not Present", "Present"))

ggplot(carsDF, aes(x = shopID, y = carsFixed)) +
  theme_minimal(14) +
  geom_jitter(aes(color = bossx), size = 2) + 
  theme(text = element_text(color = "gray20"),
        legend.title = element_blank(),
        legend.position = c("top"), # position the legend in the upper left
        legend.direction = "horizontal",
        legend.justification = 0.1, # anchor point for legend.position.
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1), # move title away from axis
        axis.title.y = element_text(vjust = 2), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank()
  ) +
  scale_color_manual(name = "",
                     values = c("red", "blue")) +
  scale_y_continuous(limits = c(0, 70),
                     breaks = seq(0, 70, by = 10)) +
  labs(title = "Cars Fixed by Shop Based on Boss Presence",
       y = "Cars Fixed",
       x = "Shop",
       caption = "Data from last 10 weeks") +
  annotate("text", x = c(1,2,3,4,5), y = c(28,45,55,68,68), 
           label = c("10 Visits", "5 Visits", "15 Visits", "5 Visits", 
                     "15 Visits") , color="darkgreen", 
           size=4 , angle=0, fontface="bold")

#ggsave("Final_Deliverable_Drew_McKnight_0.pdf")


# CREATE GRAPHICAL MODEL
graph = dag_create() %>%
  dag_node("Cars Fixed","K",
           data = carsDF$carsFixed,
           rhs = poisson(lambda)) %>%
  dag_node("Exp Cars Fixed - Shop Level","lambda",
           rhs = exp(alpha_shop + beta_shop * x),
           child = "K") %>%
  dag_node("Intercept - Shop Level","alpha_shop",
           rhs = normal(alpha,sigma_alpha),
           child = "lambda") %>%
  dag_node("Boss Effect - Shop Level","beta_shop",
           rhs = normal(beta,sigma_beta),
           child = "lambda") %>%
  dag_node("Intercept - Midas Level","alpha",
           rhs = normal(3,2),
           child = "alpha_shop") %>%
  dag_node("Std Dev - Midas Level","sigma_alpha",
           rhs = uniform(0,2),
           child = "alpha_shop") %>%
  dag_node("Exp Boss Effect - Midas Level","beta",
           rhs = normal(0,1),
           child = "beta_shop") %>%
  dag_node("Std Dev Boss Effect","sigma_beta",
           rhs = uniform(0,2),
           child = "beta_shop") %>%
  dag_node("Boss Present","x",
           data = carsDF$boss,
           child = "lambda") %>%
  dag_plate("Observation","i",
            nodeLabels = c("K","lambda","x")) %>%
  dag_plate("Shop","j",
            nodeLabels = c("beta_shop","alpha_shop"),
            data = carsDF$shopID,
            addDataNode = TRUE)

# DISPLAY GRAPHICAL MODEL
graph %>% dag_render()

# GET POSTERIOR DISTRIBUTION
drawsDF = graph %>% dag_greta()
drawsDF %>% dagp_plot()


#Shop 1
postDF = drawsDF %>%
  select(alpha_shop_1,beta_shop_1) %>%
  mutate(lambda_boss = exp(alpha_shop_1 + beta_shop_1 * 1)) %>%
  mutate(lambda_noBoss = exp(alpha_shop_1)) %>%
  mutate(z_2 = round(lambda_boss - lambda_noBoss))

postDF[-c(1, 2)] <- replace(postDF[-c(1, 2)], postDF[-c(1, 2)] < 0, 0)

moneyDF = postDF %>%
  mutate(ValueCreated = 50 * z_2)

summary(moneyDF$ValueCreated)
summary(moneyDF$z_2)

breaks = c(0,250,500,750,1000,1250)
labels = c("<$250","$250 - $500","$500 - $750",
           "$750-$1000","$1000+")
profitBins = cut(moneyDF$ValueCreated, 
           breaks, 
           include.lowest = T, 
           right=FALSE, 
           labels=labels)
moneyDF$profitBins = profitBins

breaks2 = c(0,5,10,15,20,25)
labels2 = c("<5","5-10","10-15","15-20","20+")
bins2 = cut(moneyDF$z_2, 
           breaks2, 
           include.lowest = T, 
           right=FALSE, 
           labels=labels2)
moneyDF$bins2 = bins2

plotDF = moneyDF %>%
  group_by(profitBins, bins2) %>%
  summarize(countInBin = n()) %>%
  mutate(pctInBin = countInBin / 4000) %>%
  mutate(label = paste0(round(100*pctInBin,2),"%"))


plotDF %>%
  ggplot(aes(x = profitBins, y = pctInBin), 
         color = "black", alpha = 0.7) +
  geom_col(aes(fill = bins2), color = "black", alpha = 0.7) +
  theme_minimal(14) +
  geom_text(aes(label=label), nudge_y = 0.035) +
  xlab("Profit Per Additional Car when Boss Visits Shop 1") +
  ylab("Probability of Outcome") +
  theme(text = element_text(color = "gray20"),
        legend.title=element_text(size=10, face = 'bold'),
        legend.position = c("top"), # position the legend in the upper left
        legend.direction = "horizontal",
        legend.justification = 0.1, # anchor point for legend.position.
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1), # move title away from axis
        axis.title.y = element_text(vjust = 2), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
      
  ) +
  ggtitle("Boss Presence for Shop 1") +
  coord_flip() +
  labs(fill = "Cars Fixed",
  caption = "Median Additional Cars Fixed: 6.1
       Average Additional Cars Fixed: 6.1
       Median Additional Profit: $304.10
       Average Additional Profit: $306
  Probability outcome represents chance of yielding specified profit given the number of additional cars fixed")

summary(moneyDF$ValueCreated)
summary(moneyDF$z_2)

#Shop 2
postDF2 = drawsDF %>%
  select(alpha_shop_2,beta_shop_2) %>%
  mutate(lambda_boss = exp(alpha_shop_2 + beta_shop_2 * 1)) %>%
  mutate(lambda_noBoss = exp(alpha_shop_2)) %>%
  mutate(z_2 = round(lambda_boss - lambda_noBoss))

postDF2[-c(1, 2)] <- replace(postDF2[-c(1, 2)], postDF2[-c(1, 2)] < 0, 0)

moneyDF2 = postDF2 %>%
  mutate(ValueCreated = 50 * z_2)


breaks = c(0,250,500,750,1000,1250)
labels = c("<$250","$250 - $500","$500 - $750",
           "$750-$1000","$1000+")
profitBins = cut(moneyDF2$ValueCreated, 
           breaks, 
           include.lowest = T, 
           right=FALSE, 
           labels=labels)
moneyDF2$profitBins = profitBins

breaks2 = c(0,5,10,15,20,25)
labels2 = c("<5","5-10","10-15","15-20","20+")
bins2 = cut(moneyDF2$z_2, 
            breaks2, 
            include.lowest = T, 
            right=FALSE, 
            labels=labels2)
moneyDF2$bins2 = bins2

plotDF2 = moneyDF2 %>%
  group_by(profitBins, bins2) %>%
  summarize(countInBin = n()) %>%
  mutate(pctInBin = countInBin / 4000) %>%
  mutate(label = paste0(round(100*pctInBin,2),"%"))


plotDF2 %>%
  ggplot(aes(x = profitBins, y = pctInBin), 
         color = "black", alpha = 0.7) +
  geom_col(aes(fill = bins2), color = "black", alpha = 0.7) +
  theme_minimal(14) +
  geom_text(aes(label=label), nudge_y = 0.035) +
  xlab("Profit Per Additional Car when Boss Visits Shop 2") +
  ylab("Probability of Outcome") +
  theme(text = element_text(color = "gray20"),
        legend.title=element_text(size=10, face = 'bold'),
        legend.position = c("top"), # position the legend in the upper left
        legend.direction = "horizontal",
        legend.justification = 0.1, # anchor point for legend.position.
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1), # move title away from axis
        axis.title.y = element_text(vjust = 2), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        
  ) +
  ggtitle("Boss Presence for Shop 2") +
  coord_flip() +
  labs(fill = "Cars Fixed",
       caption = "Median Additional Cars Fixed: 9.3
       Average Additional Cars Fixed: 9.4
       Median Additional Profit: $464.95
       Average Additional Profit: $469.19
  Probability outcome represents chance of yielding specified profit given the number of additional cars fixed")

summary(moneyDF2$ValueCreated)
summary(moneyDF2$z_2)

#Shop 3
postDF3 = drawsDF %>%
  select(alpha_shop_3,beta_shop_3) %>%
  mutate(lambda_boss = exp(alpha_shop_3 + beta_shop_3 * 1)) %>%
  mutate(lambda_noBoss = exp(alpha_shop_3)) %>%
  mutate(z_2 = round(lambda_boss - lambda_noBoss))

postDF3[-c(1, 2)] <- replace(postDF3[-c(1, 2)], postDF3[-c(1, 2)] < 0, 0)


moneyDF3 = postDF3 %>%
  mutate(ValueCreated = 50 * z_2)


breaks = c(0,250,500,750,1000,1250)
labels = c("<$250","$250 - $500","$500 - $750",
           "$750-$1000","$1000+")
profitBins = cut(moneyDF3$ValueCreated, 
           breaks, 
           include.lowest = T, 
           right=FALSE, 
           labels=labels)
moneyDF3$profitBins = profitBins

breaks2 = c(0,5,10,15,20,25)
labels2 = c("<5","5-10","10-15","15-20","20+")
bins2 = cut(moneyDF3$z_2, 
            breaks2, 
            include.lowest = T, 
            right=FALSE, 
            labels=labels2)
moneyDF3$bins2 = bins2

plotDF3 = moneyDF3 %>%
  group_by(profitBins, bins2) %>%
  summarize(countInBin = n()) %>%
  mutate(pctInBin = countInBin / 4000) %>%
  mutate(label = paste0(round(100*pctInBin,2),"%"))


plotDF3 %>%
  ggplot(aes(x = profitBins, y = pctInBin), 
         color = "black", alpha = 0.7) +
  geom_col(aes(fill = bins2), color = "black", alpha = 0.7) +
  theme_minimal(14) +
  geom_text(aes(label=label), nudge_y = 0.035) +
  xlab("Profit Per Additional Car when Boss Visits Shop 3") +
  ylab("Probability of Outcome") +
  theme(text = element_text(color = "gray20"),
        legend.title=element_text(size=10, face = 'bold'),
        legend.position = c("top"), # position the legend in the upper left
        legend.direction = "horizontal",
        legend.justification = 0.1, # anchor point for legend.position.
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1), # move title away from axis
        axis.title.y = element_text(vjust = 2), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        
  ) +
  ggtitle("Boss Presence for Shop 3") +
  coord_flip() +
  labs(fill = "Cars Fixed",
       caption = "Median Additional Cars Fixed: 4
       Average Additional Cars Fixed: 3.6
       Median Additional Profit: $200
       Average Additional Profit: $179.40
  Probability outcome represents chance of yielding specified profit given the number of additional cars fixed")

summary(moneyDF3$ValueCreated)
summary(moneyDF3$z_2)

#Shop 4
postDF4 = drawsDF %>%
  select(alpha_shop_4,beta_shop_4) %>%
  mutate(lambda_boss = exp(alpha_shop_4 + beta_shop_4 * 1)) %>%
  mutate(lambda_noBoss = exp(alpha_shop_4)) %>%
  mutate(z_2 = round(lambda_boss - lambda_noBoss))

postDF4[-c(1, 2)] <- replace(postDF4[-c(1, 2)], postDF4[-c(1, 2)] < 0, 0)


moneyDF4 = postDF4 %>%
  mutate(ValueCreated = 50 * z_2)


breaks = c(0,250,500,750,1000,1250)
labels = c("<$250","$250 - $500","$500 - $750",
           "$750-$1000","$1000+")
profitBins = cut(moneyDF4$ValueCreated, 
           breaks, 
           include.lowest = T, 
           right=FALSE, 
           labels=labels)
moneyDF4$profitBins = profitBins

breaks2 = c(0,5,10,15,20,25)
labels2 = c("<5","5-10","10-15","15-20","20+")
bins2 = cut(moneyDF4$z_2, 
            breaks2, 
            include.lowest = T, 
            right=FALSE, 
            labels=labels2)
moneyDF4$bins2 = bins2

plotDF4 = moneyDF4 %>%
  group_by(profitBins, bins2) %>%
  summarize(countInBin = n()) %>%
  mutate(pctInBin = countInBin / 4000) %>%
  mutate(label = paste0(round(100*pctInBin,2),"%"))


plotDF4 %>%
  ggplot(aes(x = profitBins, y = pctInBin), 
         color = "black", alpha = 0.7) +
  geom_col(aes(fill = bins2), color = "black", alpha = 0.7) +
  theme_minimal(14) +
  geom_text(aes(label=label), nudge_y = 0.035) +
  xlab("Profit Per Additional Car when Boss Visits Shop 4") +
  ylab("Probability of Outcome") +
  theme(text = element_text(color = "gray20"),
        legend.title=element_text(size=10, face = 'bold'),
        legend.position = c("top"), # position the legend in the upper left
        legend.direction = "horizontal",
        legend.justification = 0.1, # anchor point for legend.position.
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1), # move title away from axis
        axis.title.y = element_text(vjust = 2), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        
  ) +
  ggtitle("Boss Presence for Shop 4") +
  coord_flip() +
  labs(fill = "Cars Fixed",
       caption = "Median Additional Cars Fixed: 12.5
       Average Additional Cars Fixed: 12.5
       Median Additional Profit: $623.80
       Average Additional Profit: $626.90
  Probability outcome represents chance of yielding specified profit given the number of additional cars fixed")

summary(moneyDF4$ValueCreated)
summary(moneyDF4$z_2)

#Shop 5
postDF5 = drawsDF %>%
  select(alpha_shop_5,beta_shop_5) %>%
  mutate(lambda_boss = exp(alpha_shop_5 + beta_shop_5 * 1)) %>%
  mutate(lambda_noBoss = exp(alpha_shop_5)) %>%
  mutate(z_2 = round(lambda_boss - lambda_noBoss))

postDF5[-c(1, 2)] <- replace(postDF5[-c(1, 2)], postDF5[-c(1, 2)] < 0, 0)


moneyDF5 = postDF5 %>%
  mutate(ValueCreated = 50 * z_2)


breaks = c(0,250,500,750,1000,1250)
labels = c("<$250","$250 - $500","$500 - $750",
           "$750-$1000","$1000+")
profitBins = cut(moneyDF5$ValueCreated, 
           breaks, 
           include.lowest = T, 
           right=FALSE, 
           labels=labels)
moneyDF5$profitBins = profitBins

breaks2 = c(0,5,10,15,20,25)
labels2 = c("<5","5-10","10-15","15-20","20+")
bins2 = cut(moneyDF5$z_2, 
            breaks2, 
            include.lowest = T, 
            right=FALSE, 
            labels=labels2)
moneyDF5$bins2 = bins2

plotDF5 = moneyDF5 %>%
  group_by(profitBins, bins2) %>%
  summarize(countInBin = n()) %>%
  mutate(pctInBin = countInBin / 4000) %>%
  mutate(label = paste0(round(100*pctInBin,2),"%"))


plotDF5 %>%
  ggplot(aes(x = profitBins, y = pctInBin), 
         color = "black", alpha = 0.7) +
  geom_col(aes(fill = bins2), color = "black", alpha = 0.7) +
  theme_minimal(14) +
  geom_text(aes(label=label), nudge_y = 0.035) +
  xlab("Profit Per Additional Car when Boss Visits Shop 5") +
  ylab("Probability of Outcome") +
  theme(text = element_text(color = "gray20"),
        legend.title=element_text(size=10, face = 'bold'),
        legend.position = c("top"), # position the legend in the upper left
        legend.direction = "horizontal",
        legend.justification = 0.1, # anchor point for legend.position.
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1), # move title away from axis
        axis.title.y = element_text(vjust = 2), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        
  ) +
  ggtitle("Boss Presence for Shop 5") +
  coord_flip() +
  labs(fill = "Cars Fixed",
       caption = "Median Additional Cars Fixed: 8.7
       Average Additional Cars Fixed: 8.7
       Median Additional Profit: $433.30
       Average Additional Profit: $434.50
  Probability outcome represents chance of yielding specified profit given the number of additional cars fixed")

summary(moneyDF5$ValueCreated)
summary(moneyDF5$z_2)


#Graph 2
moneyDFsub = moneyDF %>% select(z_2, ValueCreated) %>% mutate(shop = 1)
moneyDF2sub = moneyDF2 %>% select(z_2, ValueCreated) %>% mutate(shop = 2)
moneyDF3sub = moneyDF3 %>% select(z_2, ValueCreated) %>% mutate(shop = 3)
moneyDF4sub = moneyDF4 %>% select(z_2, ValueCreated) %>% mutate(shop = 4)
moneyDF5sub = moneyDF5 %>% select(z_2, ValueCreated) %>% mutate(shop = 5)

moneyDFall = rbind(moneyDFsub, moneyDF2sub, moneyDF3sub, moneyDF4sub, moneyDF5sub)

breaks = c(0,250,500,750,1000,1250)
labels = c("<$250","$250 - $500","$500 - $750",
           "$750-$1000","$1000+")
profitBins = cut(moneyDFall$ValueCreated, 
                 breaks, 
                 include.lowest = T, 
                 right=FALSE, 
                 labels=labels)
moneyDFall$profitBins = profitBins

breaks2 = c(0,5,10,15,20,25)
labels2 = c("<5","5-10","10-15","15-20","20+")
bins2 = cut(moneyDFall$z_2, 
            breaks2, 
            include.lowest = T, 
            right=FALSE, 
            labels=labels2)
moneyDFall$bins2 = bins2

plotDFall = moneyDFall %>%
  group_by(shop, profitBins, bins2) %>%
  summarize(countInBin = n()) %>%
  mutate(pctInBin = countInBin / 4000) %>%
  mutate(label = paste0(round(100*pctInBin,2),"%"),
         facetLabel = paste0("Shop ", shop))

medianDF = moneyDFall %>%
  group_by(shop) %>%
  summarize(medianProfit = median(ValueCreated)) %>%
  mutate(facetLabel = paste0("Shop ",shop))


plotDFall %>%
  ggplot(aes(x = profitBins, y = pctInBin), 
         color = "black", alpha = 0.7) +
  geom_col(aes(fill = bins2), color = "black", alpha = 0.7) +
  facet_wrap(~facetLabel) +
  theme_minimal(14) +
  geom_text(aes(label=label), nudge_y = 0.055) +
  xlab("Profit Per Additional Car when Boss Visits") +
  ylab("Probability of Outcome") +
  theme(text = element_text(color = "gray20"),
        legend.title=element_text(size=10, face = 'bold'),
        legend.position = c("top"), # position the legend in the upper left
        legend.direction = "horizontal",
        legend.justification = 0.1, # anchor point for legend.position.
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1), # move title away from axis
        axis.title.y = element_text(vjust = 2), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(data = medianDF,
            aes(x = 5.2, y = 0.5,
                label = paste0("Median Profit: ",
                               scales::dollar(medianProfit)))) +
  ggtitle("Boss Presence") +
  coord_flip() +
  labs(fill = "Cars Fixed",
       caption = "Probability outcome represents chance of yielding specified profit given the number of additional cars fixed")

#ggsave("Final_Deliverable_Drew_McKnight.pdf", width = 16.5, height = 10, units = "in")
