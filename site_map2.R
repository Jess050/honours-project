# Site map 
# Jesse 
# July 2018 
# spatial coordinates of sites 


# load libraries  ---------------------------------------------------------

library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggmap)
library(ggpubr)


# load data ---------------------------------------------------------------

site_list <- read.csv("D:/honours/honours-project/data/sites_fin.csv", sep=";")

# slice "selects rows" as select "selects columns"

false_bay_sites <- site_list %>% 
  slice(c(1,5:7,11:15))

west_coast_sites <- site_list %>% 
  slice(c(2:4,8:10,16))

# site_list %>% 
#   filter(region)

# western cape ------------------------------------------------------------
# 
#western_cape <- get_map(location = c(lon = 18.4731263, lat = -33.4358028), 
#                       zoom = 8, maptype = 'satellite')
# 
# ggmap(western_cape)

load("data/cape_point.RData")

ggmap(cape_point)

wc1 <- ggmap(cape_point) +
  geom_point(data = site_list, aes(x = long , y = lat ), 
             colour = "red", size =  2.5) +
  labs(x = "Latitude(°E)", y = "Longitude(°S)", title = "Site Map") 

wc2 <- wc1 +
  geom_text(data = false_bay_sites,
            aes(long, lat, label = site), 
            hjust = 0, vjust = 0.7, 
            size = 4, colour = "white") + 
  geom_text(data = west_coast_sites,
            aes(long, lat, label = site), 
            hjust = 1.05, vjust = 0.7, 
            size = 4, colour = "white") +
  annotate("text", label = "False Bay", 
                      x = 18.7 , y = -34.3,
                      size = 5, colour = "salmon")+
  annotate("text", label = "West Coast", 
           x = 18.1 , y = -34.3,
           size = 5, colour = "salmon")+
  theme_bw()+
  coord_cartesian()

wc2

# Yzerfontein -------------------------------------------------------------

# yzerfontein <- get_map(location = c(lon = 18.14726, lat = -33.35357),
#                         zoom = 10, maptype = 'satellite')
#  
# ggmap(yzerfontein)
#  
# yzer1 <- ggmap(yzerfontein) +
#   geom_point(data = site_list, aes(x = lon , y = lat ), 
#               colour = "red", size =  2.5) +
#    labs(x = "Latitude(°E)", y = "Longitude(°S)", title = "Yzerfontein") 
#  
#  yzer2 <- yzer1 +
#     geom_text(data = site_list,
#               aes(lon , lat , label = site),
#               hjust = 0.5, vjust = -1,
#               size = 4, colour = "white") +
#    annotate("text", label = "18.15°E, -33.35°S", 
#             x = 18.14726 , y = -33.35357, 
#             size = 3, colour = "salmon") +
#    theme_void() +
#    theme( panel.border = element_rect(colour = "white", fill=NA, size=5)) +
#    coord_cartesian()
#  
#  yzer2

