#results 

# load libraries  ---------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(dplyr) #for filter and %>%  command
library(readr) #read csv
library(ggplot2)

# load data ---------------------------------------------------------------

#loading data about the morphometric properties of Ecklonia maxima 

morph <- read_csv("morph.csv")

unique(morph$site)

morph$ind = as.numeric(morph$ind) #needed? 

# facet wrap main--------------------------------------------------------------

ggplot(morph, aes(x = stipe_length, y = frond_length)) + 
  geom_point(aes(colour = site)) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey35") +
  facet_wrap(~site)+
  labs(x = "Stipe Length (cm)", y = "Frond Length (cm)")


# graph -------------------------------------------------------------------

ggplot(morph, aes(x = stipe_length, y = frond_length, colour = site, group = site)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Stipe Length (cm)", y = "Frond Length (cm)")

#SE overlap means not sig different from each other, still need to confirm by anova or t-test

# St James -------------------------------------------------------------

stjames <- morph %>% 
  filter(site == "St_James") 

stj <- ggplot(stjames, aes(x = stipe_length, y = frond_length)) +
  geom_line(aes(group = site), alpha = 0.5) +
  geom_point() +
  labs(x = "Stipe Length (cm)", y = "Frond Length (cm)",
title = "ST JAMES")

stj

# Oudekraal ---------------------------------------------------------------

oudekraal <- morph %>% 
  filter(site == "Oudekraal") 

oude <- ggplot(oudekraal, aes(x = stipe_length, y = frond_length)) +
  geom_line(aes(group = site), alpha = 0.5) +
  geom_point() +
  labs(x = "Stipe Length (cm)", y = "Frond Length (cm)",
       title = "OUDEKRAAL")
oude


# BAKOVEN -----------------------------------------------------------------

bakoven <- morph %>% 
  filter(site == "Bakoven") 

bak <- ggplot(bakoven, aes(x = stipe_length, y = frond_length)) +
  geom_line(aes(group = site), alpha = 0.5) +
  geom_point() +
  labs(x = "Stipe Length (cm)", y = "Frond Length (cm)",
       title = "BAKOVEN")

bak


# Yzerfontein -------------------------------------------------------------

Yzerfontein <- morph %>% 
  filter(site == "Yzerfontein")

yzer <- ggplot(Yzerfontein, aes(x = stipe_length, y = frond_length)) +
  geom_line(aes(group = site), alpha = 0.5) +
  geom_point() +
  labs(x = "Stipe Length (cm)", y = "Frond Length (cm)",
       title = "Yzerfontein")

yzer


# means -------------------------------------------------------------------

morph1 <- morph %>% 
  group_by(site) %>% 
  summarise(mn_st_len = mean(stipe_length),
            mn_fr_len = mean(frond_length),
            mn_fr_mass = mean(frond_mass),
            mn_st_mass = mean(stipe_mass))

morph1
  
ggplot(morph1, aes(y = mn_fr_len, x = mn_st_len)) +
  geom_col(aes(fill = site)) +
  geom_point(aes(colour = site), shape = 21, fill = "Khaki", size = 5)

ggplot(morph1, aes(y = mn_fr_len, x = mn_st_len)) +
  geom_point(aes(colour = site))

ggplot(morph1, aes(x = mn_st_mass, y = mn_st))

