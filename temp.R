# analysing temperature data 


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

# create site data frame
sites <- c( "Yzerfontein", "Oudekraal","Kommetjie", "Miller's Point", "Kalk Bay", "Muizenberg")

# separate index into site and source, and replace sites with the new sites df
temp1 <- temp %>% 
  separate(index, c("site", "source"), "/") %>% 
  filter(site %in% sites)

# summarising by site and date 
dat1 <- temp1 %>% 
  group_by(site, date) %>%
  summarise(mn.temp = mean(temp, na.rm = TRUE),
            sd.temp = sd(temp, na.rm = TRUE))

# summarise by unique site and source
mean <- temp1 %>% 
  group_by(site, source) %>% 
  summarise(mn.temp = mean(temp, na.rm = TRUE),
            sd.temp = sd(temp, na.rm = TRUE))

#visualise
ggplot(temp1, aes(x = site, y = temp)) +
  geom_boxplot(aes(colour = site)) +
  geom_point(data = mean, aes(x = site, y = mn.temp), shape = 2) +
  theme_classic()

ggplot(mean, aes(x = site, y = mn.temp)) +
  geom_col(aes(fill = site)) + 
  geom_errorbar(aes(ymin = mn.temp - sd.temp,
                    ymax = mn.temp + sd.temp), size = 0.2)

################

# dont have to replicate sites, because morph isnt gona be combined with temp, 
# as temp is temp data to be merged with wave data, those sites should correspond.


###############

# plot the monthly temperatures;
# create a facet for each site, and colour-code the src
plot1 <- ggplot(temp1, aes(x = date, y = temp)) +
  geom_line(aes(colour = site), size = 0.2) +
  facet_wrap(~site, ncol = 3) +
  xlab(NULL) + ylab("Temperature (°C)") + ggtitle("coastal water temperature") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot1


# map of sea surface temperature ------------------------------------------

# Load libraries
library(tidyverse)
library(ggpubr)
# Load data
load("data/south_africa_coast.RData")
load("data/sa_provinces.RData")
load("data/rast_annual.RData")
# Choose which SST product you would like to use
sst <- dat1

## The colour pallette we will use for ocean temperature
cols11 <- c("#004dcd", "#0068db", "#007ddb", "#008dcf", "#009bbc",
            "#00a7a9", "#1bb298", "#6cba8f", "#9ac290", "#bec99a")


site_list <- read.csv("D:/honours/honours-project/data/sites_updated.csv", sep=";")

# loca <- site_list %>% 
#   select(2,4:7,10:13)
#   
# a<- site_list[2,]
# b <- loca[3:6]
# c <- b[9:112]


# select the required sites, merge long and lat with temperature of those sites 
# plot on map of western cape 

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +   
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  #geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins),
            # colour = "white", size = 0.1) + # The coastal temperature values
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(17, 24), ylim = c(-36, -30), expand = 0)

# using too many datasets? 

