# tidy script

# load libraries  ---------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(dplyr) #for filter and %>%  command
library(readr) #read csv
library(ggplot2)
library(vegan)

# load data ---------------------------------------------------------------

#loading data about the morphometric properties of Ecklonia maxima 

morph <- as.tibble(read_csv("D:/honours/honours-project/data/morph_update.csv")[, -14])


# standardise data --------------------------------------------------------

# create numerical df to be standardized 
morph.std <- morph %>% 
  select(-date, -site, -depth, -ind, -fertile) %>% 

morph.stand <- decostand(morph.std, method = "standardize")

# create df with non numerical cols
df_char <- morph %>% 
  select(date:fertile)

# combine standardized data with character df
df <- cbind(df_char, morph.stand) %>% 
  filter(depth == "deep/adult")

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# summarize morph 
sum.morph <- morph %>%
  group_by(as.factor(site)) %>%   
  summarise(mn_fr_mass = mean(frond_mass), 
            sd_fr_mass = sd(frond_mass),
            mn_pri_len = mean(primary_length),
            sd_pri_len = sd(primary_length), 
            mn_pri_wid = mean(primary_width),
            sd_pri_wid = sd(primary_width), 
            mn_fr_len = mean(frond_length),
            sd_fr_len = sd(frond_length),
            mn_st_mass = mean(stipe_mass),
            sd_st_mass = sd(stipe_mass), 
            mn_st_len = mean(stipe_length),
            sd_st_len = sd(stipe_length), 
            mn_st_circ = mean(stipe_circ), 
            sd_st_circ = sd(stipe_circ),
            mn_tufts = mean(tufts), 
            sd_tufts = sd(tufts),
            mn_total_len = mean(total_length), 
            sd_total_len = sd(total_length))

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# boxplots on standardized data 
# convert wide data to long data 
morph_long <- df %>% 
  gather(key = "variable", value = "value", -site, -ind, -date, -depth, -fertile)

# visualising data 
ggplot(data = morph_long, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  facet_wrap( ~ variable, scales = "free") +
  theme_classic()

# graphs 

# length
ggplot(df, aes(x = stipe_length, y = frond_length)) + 
  geom_point(aes(colour = site), show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey35") +
  facet_wrap(~site)+
  labs(x = "Stipe Length (cm)", y = "Frond Length (cm)")

# primary blade
ggplot(df, aes(x = primary_length, y = primary_width)) + 
  geom_point(aes(colour = site), show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey35") +
  facet_wrap(~site)+
  labs(x = "Primary Blade Length (cm)", y = "Primary Blade Width (cm)") +
  theme_classic()

# mass 
ggplot(df, aes(x = frond_mass, y = stipe_mass)) + 
  geom_point(aes(colour = site), show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey35") +
  facet_wrap(~site)+
  labs(x = "Frond Mass (kg)", y = "Stipe Mass (kg)") +
  theme_classic()

# with standard error (SE)
# SE overlap means not sig different from each other, still need to confirm by anova or t-test

ggplot(df, aes(x = stipe_length, y = frond_length, colour = site, group = site)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Stipe Length (cm)", y = "Frond Length (cm)")+
  theme_classic()

ggplot(df, aes(x = primary_length, y = primary_width, colour = site, group = site)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Primary Blade Length (cm)", y = "Primary Blade Width (cm)")+
  theme_classic()

ggplot(df, aes(x = frond_mass, y = stipe_mass, colour = site, group = site)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Frond Mass (kg)", y = "Stipe Mass (kg)") +
  theme_classic()


# comparisons  ------------------------------------------------------------

# depth classes 
# shallow/juvenile : juvenile E.maxima with stipe lengths of about 30cm, in very shallow water 
# shallow/adult : largest adult E.maxima individuals found in very shallow water 
# deep/adult : largest adult E.maxima individuals found at 1m depth 

compare <- cbind(df_char, morph.stand) %>% 
  filter(date == "12-09-2018")

comp_long <- compare %>%
  gather(key = "variable", value = "value", -site, -ind, -date, -depth, -fertile) 

# visualising data 
ggplot(data = comp_long, aes(x = site, y = value)) +
  geom_boxplot(aes(colour = depth)) +
  facet_wrap(~ variable, scales = "free") +
  theme_classic()

# generally juvenile kelp (30cm) show no significant difference among sites
# some variables have larger ranges than others. 
# adult kelp show morphological variation between sites, however the difference are not significant. 
# at what length do adult kelp become different in appearance between sites? 
# other sampling methods required. 
# use standardized data in order to reduce variation among sites and underlying differences 
# like slight differences in depth. 


# deep water comparison  --------------------------------------------------


# deep <- read_csv("data/eck_morph")
# unique(deep$site)
# 
# a <- deep[-12:-46,-2]
# b <-  a[-24:-73,-3]
# c <- b[-50:-75,-14]
# deep <- c[-63:-88,]
# 
# colnames(deep)
# colnames(df)
# 
# colnames(deep) <- c("date", "site", "depth", "ind", "fertile", "frond_mass", 
#                    "primary_length", "primary_width", "frond_length", "stipe_mass",    
#                    "stipe_length","stipe_circ","tufts", "total_length")

# quite complex for the formats of morph_updated and eck_morph to coincide

# load data with required sites 
deep <- read_csv("data/trial.csv")[,-15]

df.deep <- deep %>% 
  select(date:fertile)

deep.std <- deep %>% 
  select(-date, -site, -depth, -ind, -fertile)

# replacing NA values with 0 
deep.std[is.na(deep.std)] <- 0
  
deep.stand <- decostand(deep.std, method = "standardize")

combine <- cbind(deep.stand, df.deep)

# final.combine <- rbind(combine, df)

comb_long <- combine %>%
  group_by(site) %>% 
  gather(key = "variable", value = "value", -site, -ind, -date, -depth, -fertile) 

# unique(comb_long$site)
# 
# sites7 <- c( "Kalk_Bay", "Oudekraal","Kommetjie", "Miller's Point", "Kalk Bay", "Soetwater", "Miller`s_A", "Miller's_B", "Miller`s_C")

# remove Kalk Bay 
rm.kb <- combine[c(-11:-24, -140:-179),]

site_long <- rm.kb %>%
  group_by(site) %>% 
  gather(key = "variable", value = "value", -site, -ind, -date, -depth, -fertile) 

ggplot(data = site_long, aes(x = site, y = value)) +
  geom_boxplot(aes(colour = depth)) +
  facet_wrap(~ variable, scales = "free") +
  theme_classic()

# adult kelp at 1m and 7m depths are significantly different at Kommetjie and Soetwater.
# kelp are not significantly different at Miller's Point and Oudekraal, 
# except for variables stipe length, stipe mass, total length and tufts. 


# plot Kalk Bay at different depths 
# deepest adult = 7m 
# deep adult = 1m 
# shallow adult  
# shallow juvenile 

kb <- comb_long %>% 
  filter(site == "Kalk_Bay")

ggplot(data = kb, aes(x = site, y = value)) +
  geom_boxplot(aes(colour = depth)) +
  facet_wrap(~ variable, scales = "free") +
  theme_classic()

# kelp at 7m depth have a larger range, but not really significantly different from adult kelp in shallow water. 
# no significant difference between adult kelp at 1m, and juvenile kelp in shallow water.


# plot Miller's Point 

mp <-  comb_long[c(1:11, 76:114),]

ggplot(data = mp, aes(x = site, y = value)) +
  geom_boxplot(aes(colour = depth)) +
  facet_wrap(~ variable, scales = "free") +
  theme_classic()

# no significant difference between adult kelp at different depths. 
# kelps at 7m have a greater range than kelp at 1m.

### The END ###


# PCA ---------------------------------------------------------------------

# just for fun 

# Load the required packages
# (vegan must be loaded after ade4 to avoid some conflicts)
library(ade4)
library(vegan)
library(gclus)
library(ape)
library(FactoMineR)

# Load additionnal functions
# (files must be in the working directory)
source("sources/evplot.R")
source("sources/cleanplot.pca.R")
source("sources/PCA.newr.R")
source("sources/CA.newr.R")

# create df with non numerical cols
df_char1 <- morph %>% 
  select(site:ind)

# combine standardized data with character df
df.try <- cbind(df_char1, morph.stand) %>% 
  filter(depth == "deep/adult")


df.h <- decostand(data = df.try$frond_mass:total_length, method = "hellinger")
(df.h.pca <- rda(df.h))

# Plot eigenvalues and % of variance for each axis
ev <- df.h.pca$CA$eig
dev.new(title="PCA eigenvalues")
evplot(ev)

# PCA biplots
df.pca.sc1 <- scores(df.h.pca, display="sites", scaling=1)
df.pca.sc2 <- scores(df.h.pca, display="sites", scaling=2)

dev.new(title="PCA on sites", width=12, height=6)
par(mfrow=c(1,2))
cleanplot.pca(df.h.pca, scaling=1, mar.percent=0.06)
cleanplot.pca(df.h.pca, scaling=2, mar.percent=0.06)

# problem: showing sites 220+, 
# there should be only 169/247 sites due to removing shallow/juv and shallow/adult

