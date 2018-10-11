# find the mean of all the temps.
# subtract it from the actual temp
# Plot it on a graph
# find three overlapping sites in the west,east and south coast
# center around zero


# load libraries ----------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(ggplot2)


# load data ---------------------------------------------------------------

load("D:/honours/honours-project/data/SACTN_monthly_v4.2.Rdata")

temp <- SACTN_monthly_v4.2

temp <- temp %>%
  separate(index, c("site", "source"), "/")

unique(temp$site)




dat1 <- temp %>% 
  summarise(mn.temp = mean(temp, na.rm = TRUE),
            sd.temp = sd(temp, na.rm = TRUE))

dat2 <- temp %>% 
  summarise(mn.temp = mean(temp, na.rm = TRUE))

library(dplyr)

try1 <- temp %>%
  mutate_each(funs(.-mean(.)), matches('^col')) %>%
  select(dat2)
gc()

newnew <- mutate(temp, temp.mean = dat2)

