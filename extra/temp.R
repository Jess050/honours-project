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


temp <- SACTN_monthly_v4.2 %>%
  mutate(dec1 = date %% 1, # find fractional part of number
         year = ceiling(date), # find integer part of number
         int = round((dec1 * 12) + 1, 1), # convert fraction to integer presenting month
         month = month.abb[int]) # make month abbreviation

# desriptive statistics of temp data

na.omit(temp)

summary(temp)


# create site data frame

sites <-  c("Muizenberg", "Muizenberg", "Muizenberg", "Kalk Bay", "Miller's Point", "Miller's Point", "Miller's Point",
            "Bordjies", "Buffelsbaai", "Kommetjie", "Kommetjie", "Kommetjie","Kommetjie", "Oudekraal", "Oudekraal", "Yzerfontein")

# separate index into site and source, and replace sites with the new sites df
temp1 <- temp %>% 
  separate(index, c("site", "source"), "/") %>% 
  filter(site %in% sites)

temp2 <-temp1 %>% 
  select(-date, -dec1, -int)

# order of sites 
# "St_James_N"
# "St James "
# "St_James_S"
# "Kalk_Bay"
# "Miller`s_A"
# "Miller`s_B"
# "Miller`s_C"
# "Black_rocks"
# "Buffels"
# "Olifantsbos"
# "Soetwater"
# "Slangkop"
# "Kommetjie"
# "Oudekraal"
# "Bakoven"
# "Yzerfontein"


# summarising by site and date : mean of month for all years 
 dat1 <- temp2 %>% 
   group_by(site, month) %>%
   summarise(mn.temp = mean(temp, na.rm = TRUE),
             sd.temp = sd(temp, na.rm = TRUE))
 

# summarise by unique site and source, mean temp for all years for which there is data ?
mean <- temp2 %>% 
  group_by(site) %>% 
  summarise(mn.temp = mean(temp, na.rm = TRUE),
            sd.temp = sd(temp, na.rm = TRUE))

#visualise
ggplot(temp2, aes(x = site, y = temp)) +
  facet_wrap(~ month) +
  geom_boxplot(aes(colour = site)) +
  geom_point(data = mean, aes(x = site, y = mn.temp), shape = 2) +
  theme_classic()

# ggplot(mean, aes(x = site, y = mn.temp)) +
#   geom_col(aes(fill = site)) + 
#   geom_errorbar(aes(ymin = mn.temp - sd.temp,
#                     ymax = mn.temp + sd.temp), size = 0.2)

################

# dont have to replicate sites, because morph isnt gona be combined with temp, 
# as temp is temp data to be merged with wave data, those sites should correspond.


###############

# plot the monthly temperatures;
# create a facet for each site, and colour-code the src
# plot1 <- ggplot(temp1, aes(x = date, y = temp)) +
#   geom_line(aes(colour = site), size = 0.2) +
#   facet_wrap(~site, ncol = 3) +
#   xlab(NULL) + ylab("Temperature (°C)") + ggtitle("coastal water temperature") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# plot1
# not using this 

# map of sea surface temperature ------------------------------------------

# Load libraries
library(tidyverse)
library(ggpubr)
# Load data
load("data/south_africa_coast.RData")
load("data/sa_provinces.RData")
load("data/rast_annual.RData")
# Choose which SST product you would like to use

## The colour pallette we will use for ocean temperature
cols11 <- c("#004dcd", "#0068db", "#007ddb", "#008dcf", "#009bbc",
            "#00a7a9", "#1bb298", "#6cba8f", "#9ac290", "#bec99a")


site_list <- read.csv("D:/honours/honours-project/data/sites_updated.csv", sep=";")

# select the required sites, merge long and lat with temperature of those sites 

# remove sites and temperatures 
new <- mean %>% 
  select(site, mn.temp)

# replicate sites from temp data to nearest sites in site_list 
newer <- rbind(new, new[1:6, ])[-12,][-7,]
newest <- rbind(newer, new[2:4,])

sites.new <- as.data.frame(c("Muizenberg", "Oudekraal", "Oudekraal", "Yzerfontein", "Miller's Point","Miller's Point","Miller's Point", "Kommetjie", "Kommetjie", "Kommetjie", "Muizenberg", "Muizenberg", "Kalk Bay")) 


# # merge temp to site list 
# merge <- cbind(newest, site_list) # problem???
# 
# fuckj <-  as.data.frame(merge)

# plot on map of western cape 

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  # geom_raster(data = temp, aes(fill = temp)) +   
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +  
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins),
            colour = "white", size = 0.1) + # Monthly coastal temperature values
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(17, 24), ylim = c(-36, -30), expand = 0)

# using too many datasets? 

######## 
library(data.table)

site_list$site

#try <- setattr(site_list, "row.names", c("Muizenberg", "Oudekraal", "Oudekraal", "Yzerfontein", "Miller's Point","Miller's Point","Miller's Point", "Kommetjie", "Kommetjie", "Kommetjie", "Muizenberg", "Muizenberg", "Kalk Bay"))

sites.new <- as.data.frame(c("Muizenberg", "Oudekraal", "Oudekraal", "Yzerfontein", "Miller's Point","Miller's Point","Miller's Point", "Kommetjie", "Kommetjie", "Kommetjie", "Muizenberg", "Muizenberg", "Kalk Bay")) %>% 
  dplyr::rename(site_new = c("Muizenberg", "Oudekraal", "Oudekraal", "Yzerfontein", "Miller's Point","Miller's Point","Miller's Point", "Kommetjie", "Kommetjie", "Kommetjie", "Muizenberg", "Muizenberg", "Kalk Bay"))

merge <- merge(newest, site_list, by = "site")

new <- c("Muizenberg", "Oudekraal", "Oudekraal", "Yzerfontein", "Miller's Point","Miller's Point","Miller's Point", "Kommetjie", "Kommetjie", "Kommetjie", "Muizenberg", "Muizenberg", "Kalk Bay")

dplyr::arrange()