<<<<<<< HEAD
# analysing temperature data 
=======
# find the mean of all the temps.
# subtract it from the actual temp
# Plot it on a graph
# find three overlapping sites in the west,east and south coast
# center around zero
>>>>>>> 46fed79c4663b89324519f23bed69365cea9e34b


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


# desriptive statistics of temp data

na.omit(temp)

summary(temp)

#visualise
ggplot(temp, aes(x = site, y = temp)) +
  geom_boxplot(aes(colour = site)) +
  geom_point(data = dat1, aes(x = site, y = mn.temp), shape = 2) +
  theme_classic()

unique(SACTN_monthly_v4.2$index)

# create df of required sites 
sites <- c( "Yzerfontein", "Oudekraal","Kommetjie", "Miller's Point", "Kalk Bay", "Muizenberg")

# separate index column into site and source and include sites df
temp <- temp %>%
  separate(index, c("site", "source"), "/") %>% 
  filter(site %in% sites)

unique(temp$site)

# summarising temp
dat1 <- temp %>% 
  group_by(site) %>%
  summarise(mn.temp = mean(temp, na.rm = TRUE),
            sd.temp = sd(temp, na.rm = TRUE))

ggplot(dat1, aes(x = site, y = mn.temp)) +
  geom_point(data = dat1, aes(x = site, y = mn.temp), shape = 2) +
  geom_col(aes(fill = site)) + 
  geom_errorbar(aes(ymin = mn.temp - sd.temp,
                    ymax = mn.temp + sd.temp), size = 0.5)

################

# dont have to replicate sites, because morph isnt gona be combined with temp, 
# as temp is temp data to be merged with wave data, those sites should correspond.


###############




