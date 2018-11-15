
library(tidyverse)
library(vegan)
library(RColorBrewer)
library(gridExtra)
library(lubridate)
library(GGally)
library(ggpubr)
library(ggrepel)

# Load map data
load("data/south_africa_coast.Rdata")
load("data/sa_provinces.Rdata")


# remove yzerfontein
kelp_sites <- read_delim("data/sites_fin.csv", ";", escape_double = FALSE, trim_ws = TRUE)
kelp_sites_map <- subset(kelp_sites, !site == "Yzerfontein")

# Subset into 4 regions
west__up_site <- subset(kelp_sites, site == "Oudekraal" | site == "Bakoven")

west__down_site <- subset(kelp_sites, site == "Soetwater" | site == "Olifantsbos" | site == "Slangkop" | site == "Kommetjie")

east_up_site <- subset(kelp_sites, site == "Kalk_Bay" | site == "St James" | site == "St_James_N" | site == "St_James_S") 

east_down_site <- subset(kelp_sites, site == "Millers`s_A" | site == "Miller`s_B" | site == "Miller`s_C" 
                         | site == "Black_rocks" | site == "Buffels")

# plot map
try <- ggplot(data = kelp_sites_map, aes(x = long, y = lat)) +
  geom_polygon(data = south_africa_coast, aes(group = group), fill = "grey70") +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_point(data = kelp_sites_map)  +
  coord_fixed(xlim = c(17.65, 19.5), ylim = c(-32.75, -34.6)) +
  geom_label_repel(data = west__up_site, aes(x = long, y = lat, label = site), 
                   size = 3, box.padding = 0.5, nudge_x = -0.5, nudge_y = 0.2, segment.alpha = 0.4) +
  geom_label_repel(data = west__down_site, aes(x = long, y = lat, label = site), 
                   size = 3, box.padding = 0.5, nudge_x = -0.5, nudge_y = -0.2, segment.alpha = 0.4) +
  geom_label_repel(data = east_up_site, aes(x = long, y = lat, label = site), 
                   size = 3, box.padding = 0.5, nudge_x = 0.5, nudge_y = 0.3, segment.alpha = 0.4) +
  geom_label_repel(data = east_down_site, aes(x = long, y = lat, label = site), 
                   size = 3, box.padding = 0.5, nudge_x = 0.35, nudge_y = -0.15, segment.alpha = 0.4) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
