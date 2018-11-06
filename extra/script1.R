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
  select(-date, -site, -depth, -ind, -fertile) 

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
# 
# sites <- c("St_James_N","St James ","St_James_S","Kalk_Bay","Miller`s_A","Miller`s_B","Miller`s_C","Black_rocks",
#   "Buffels","Olifantsbos","Soetwater","Slangkop","Kommetjie","Oudekraal","Bakoven","Yzerfontein")


# boxplots on standardized data 
# convert wide data to long data 
morph_long <- df %>% 
  gather(key = "variable", value = "value", -site, -ind, -date, -depth, -fertile)  #%>% 
#  filter(site %in% sites)

# figure 1
# visualising data 
ggplot(data = morph_long, aes(x = "", y = value, fill = site)) +
  geom_boxplot() +
  facet_wrap( ~ variable, scales = "free") +
  theme_classic()

# graphs 
# 
# # length
# ggplot(df, aes(x = stipe_length, y = frond_length)) + 
#   geom_point(aes(colour = site), show.legend = FALSE) +
#   geom_smooth(method = "lm", se = FALSE, colour = "grey35") +
#   facet_wrap(~site)+
#   labs(x = "Stipe Length (cm)", y = "Frond Length (cm)")
# 
# # primary blade
# ggplot(df, aes(x = primary_length, y = primary_width)) + 
#   geom_point(aes(colour = site), show.legend = FALSE) +
#   geom_smooth(method = "lm", se = FALSE, colour = "grey35") +
#   facet_wrap(~site)+
#   labs(x = "Primary Blade Length (cm)", y = "Primary Blade Width (cm)") +
#   theme_classic()
# 
# # mass 
# ggplot(df, aes(x = frond_mass, y = stipe_mass)) + 
#   geom_point(aes(colour = site), show.legend = FALSE) +
#   geom_smooth(method = "lm", se = FALSE, colour = "grey35") +
#   facet_wrap(~site)+
#   labs(x = "Frond Mass (kg)", y = "Stipe Mass (kg)") +
#   theme_classic()
# 
# # with standard error (SE)
# # SE overlap means not sig different from each other, still need to confirm by anova or t-test
# 
# ggplot(df, aes(x = stipe_length, y = frond_length, colour = site, group = site)) + 
#   geom_point() +
#   geom_smooth(method = "lm", se = TRUE) +
#   labs(x = "Stipe Length (cm)", y = "Frond Length (cm)")+
#   theme_classic()
# 
# ggplot(df, aes(x = primary_length, y = primary_width, colour = site, group = site)) + 
#   geom_point() +
#   geom_smooth(method = "lm", se = TRUE) +
#   labs(x = "Primary Blade Length (cm)", y = "Primary Blade Width (cm)")+
#   theme_classic()
# 
# ggplot(df, aes(x = frond_mass, y = stipe_mass, colour = site, group = site)) + 
#   geom_point() +
#   geom_smooth(method = "lm", se = TRUE) +
#   labs(x = "Frond Mass (kg)", y = "Stipe Mass (kg)") +
#   theme_classic()
# 

# comparisons  ------------------------------------------------------------

# depth classes 
# shallow/juvenile : juvenile E.maxima with stipe lengths of about 30cm, in very shallow water 
# shallow/adult : largest adult E.maxima individuals found in very shallow water 
# deep/adult : largest adult E.maxima individuals found at 1m depth 

compare <- cbind(df_char, morph.stand) %>% 
  filter(date == "12-09-2018")

comp_long <- compare %>%
  gather(key = "variable", value = "value", -site, -ind, -date, -depth, -fertile) 

# figure 2 
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
deep <- read_csv("data/trial.csv")

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
rm.kb <- combine[c(-12:-25, -152:-192),]

# remove millers point 

rm.mp <- rm.kb[c(-1:-11, -73:-114 ),]


site_long <- rm.mp %>%
  group_by(site) %>% 
  gather(key = "variable", value = "value", -site, -ind, -date, -depth, -fertile)  

# figure 3
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

# figure 4 
ggplot(data = kb, aes(x = site, y = value)) +
  geom_boxplot(aes(colour = depth)) +
  facet_wrap(~ variable, scales = "free") +
  theme_classic()

# kelp at 7m depth have a larger range, but not really significantly different from adult kelp in shallow water.
# no significant difference between adult kelp at 1m, and juvenile kelp in shallow water.


# plot Miller's Point 

mp_deep <- comb_long %>% 
  filter(site == "Miller's Point")

mp_shallow <- comb_long %>% 
  filter(date == "04-05-2018")

mp <-  rbind(mp_deep, mp_shallow)

# figure 5 
ggplot(data = mp, aes(x = site, y = value)) +
  geom_boxplot(aes(colour = depth)) +
  facet_wrap(~ variable, scales = "free") +
  theme_classic()

# no significant difference between adult kelp at different depths. 
# kelps at 7m have a greater range than kelp at 1m.

### The END ###
