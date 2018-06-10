# Site map 
# Jesse 
# 26 March 2018 

library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggmap)


site_list <- read.csv("site_list.csv", sep=";")

# slice "selects rows" as select "selects columns"

false_bay_sites <- site_list %>% 
  slice(c(1:6,9,11,15))

west_coast_sites <- site_list %>% 
  slice(c(8,10,12:14,16:18,21,22))


# cape point sites --------------------------------------------------------

cape_point1 <-  get_map(location = c(lon = 18.6, lat = -34.2),
          zoom = 10, maptype = 'satellite')

ggmap(cape_point1)
        
cp1 <- ggmap(cape_point1) +
  geom_point(data = site_list, aes(x = lon , y = lat ), 
             colour = "red", size =  2.5) +
  coord_equal(xlim = c(18.2, 19.0), ylim = c(-34.5, -33.8), expand = FALSE) +
  labs(y = "Latitude(°E)", x = "Longitude(°S)", title = "Site Map") 

cp1

cp2 <- cp1 +
  geom_text(data = false_bay_sites,
            aes(lon , lat , label = site), 
            hjust = 0, vjust = 0.7, 
            size = 4, colour = "salmon") + 
  geom_text(data = west_coast_sites,
            aes(lon , lat , label = site), 
            hjust = 1.05, vjust = 0.7, 
            size = 4, colour = "salmon") + 
  annotate("text", label = "St James", 
           x = 18.5, y = -34.11, 
           size = 4, colour = "salmon") +
  theme_bw()+
  coord_cartesian()

cp2

# western cape ------------------------------------------------------------


western_cape <- get_map(location = c(lon = 19.1, lat = -33.4),
                       zoom = 8, maptype = 'satellite')
ggmap(western_cape)


wc1 <- ggmap(western_cape) +
  geom_point(data = site_list, aes(x = lon , y = lat ), 
             colour = "red", size =  2.5) +
  labs(x = "Latitude(°E)", y = "Longitude(°S)", title = "Site Map") 

wc2 <- wc1 +
  geom_text(data = false_bay_sites,
            aes(lon , lat , label = site), 
            hjust = 0, vjust = 0.7, 
            size = 4, colour = "white") + 
  geom_text(data = west_coast_sites,
            aes(lon , lat , label = site), 
            hjust = 1.05, vjust = 0.7, 
            size = 4, colour = "white") + 
  theme_bw()+
  coord_cartesian()

wc2


# de hoop  ----------------------------------------------------------------

de_hoop <- get_map(location = c(lon = 20.55, lat = -34.46),
                        zoom = 10, maptype = 'satellite')


ggmap(de_hoop)

dh1 <- ggmap(de_hoop) +
  geom_point(data = site_list, aes(x = lon , y = lat ), 
             colour = "red", size =  2.5) +
  labs(x = "Latitude(°E)", y = "Longitude(°S)", title = "De Hoop") 

dh2 <- dh1 +
  # geom_text(data = site_list,
  #           aes(lon , lat , label = site), 
  #           hjust = 0, vjust = 0.7, 
  #           size = 4, colour = "white") +
  annotate("text", label = "20.55°E, -34.46°S", 
           x = 20.55, y = -34.46, 
           size = 3, colour = "salmon") +
  theme_void() +
  theme( panel.border = element_rect(colour = "white", fill=NA, size=5)) +
  coord_cartesian()

dh2

# St helena  --------------------------------------------------------------

st_helena <- get_map(location = c(lon = 17.98026, lat = -32.71841),
                   zoom = 10, maptype = 'satellite')

ggmap(st_helena)

sh1 <- ggmap(st_helena) +
  geom_point(data = site_list, aes(x = lon , y = lat ), 
             colour = "red", size =  2.5) +
  labs(x = "Latitude(°E)", y = "Longitude(°S)", title = "St Helena") 

sh2 <- sh1 +
  # geom_text(data = site_list,
  #           aes(lon , lat , label = site),
  #           hjust = 0.5, vjust = -1,
  #           size = 4, colour = "white") +
  annotate("text", label = "17.98°E, -32.72°S", 
           x = 17.9802 , y = -32.68, 
           size = 3, colour = "salmon") +
  theme_void() +
  theme( panel.border = element_rect(colour = "white", fill=NA, size=5)) +
  coord_cartesian()
 
sh2


# Yzerfontein -------------------------------------------------------------

yzerfontein <- get_map(location = c(lon = 18.14726, lat = -33.35357),
                     zoom = 10, maptype = 'satellite')

ggmap(yzerfontein)

yzer1 <- ggmap(yzerfontein) +
  geom_point(data = site_list, aes(x = lon , y = lat ), 
             colour = "red", size =  2.5) +
  labs(x = "Latitude(°E)", y = "Longitude(°S)", title = "Yzerfontein") 

yzer2 <- yzer1 +
  # geom_text(data = site_list,
  #           aes(lon , lat , label = site),
  #           hjust = 0.5, vjust = -1,
  #           size = 4, colour = "white") +
  annotate("text", label = "18.15°E, -33.35°S", 
           x = 18.14726 , y = -33.35357, 
           size = 3, colour = "salmon") +
  theme_void() +
  theme( panel.border = element_rect(colour = "white", fill=NA, size=5)) +
  coord_cartesian()

yzer2

# final site map  ---------------------------------------------------------

final <- cp2 + 
  annotation_custom(grob = ggplotGrob(sh2),
                  xmin = 18.4, xmax = 18.6,
                  ymin = -33.82, ymax = -34) +
  annotation_custom(grob = ggplotGrob(dh2),
                    xmin = 18.8, xmax = 19.0,
                    ymin = -33.82, ymax = -34) +
  annotation_custom(grob = ggplotGrob(yzer2),
                    xmin = 18.6, xmax = 18.8,
                    ymin = -33.82, ymax = -34)
final

ggsave(final, filename = "final_sitemap.png")


