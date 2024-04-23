# make the causact package available in this R session
library("causact")

# uncomment THE below line to show datasets that 
# are part of the causact package

data(package = "causact") # uncomment for dataset list

# load/unhide the dataset from the causact package
data("delivDF")
delivDF
# Uncomment this line to install the lubridate package
# install.packages("lubridate")
library("lubridate")

# Create new data frame to represent cleaned data
shipDF = delivDF
shipDF$plannedShipDate = mdy(shipDF$plannedShipDate)
shipDF$actualShipDate = mdy(shipDF$actualShipDate)

# Print updated tibble
shipDF


library("dplyr")

# create new data frame for just shipID date info
shipDateDF = shipDF %>%
  select(shipID,plannedShipDate,actualShipDate) %>%
  distinct()  ## get unique rows 
## to avoid double-counting

shipDateDF = shipDateDF %>%
  mutate(lateFlag = 
           ifelse(actualShipDate > plannedShipDate,
                  TRUE,
                  FALSE))

shipDateDF

data(prodLineDF)
prodLineDF

catDF = shipDF %>% 
  left_join(prodLineDF) %>%
  # NA prodCategory are for partID's that are 
  # not really parts.  Used for shipping or
  # service fees, so we can safely get rid of them
  filter(!is.na(prodCategory))

# find # of categories included on each shipID
numCatDF = catDF %>%
  select(shipID,
         plannedShipDate,
         actualShipDate,
         prodCategory) %>%
  distinct() %>% # unique rows only
  group_by(shipID) %>%
  summarize(numCategories = n())
# print out summary of numCategories column
numCatDF %>% 
  group_by(numCategories) %>%
  summarize(numShipID = n()) %>%
  mutate(percentOfShipments = 
           numShipID / sum(numShipID))

t = numCatDF %>%
  filter(numCategories == 2) 
  
