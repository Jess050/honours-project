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
  geom_boxplot(notch = TRUE) +
  facet_wrap( ~ variable, scales = "free") +
  theme_classic()

# Fig 1: Morphological variables of E.maxima collected at 1m depth.
# Frond length showed no specific pattern in geographic location among the sites.
# Both west coast and False Bay sites had variable frond lengths.
# Kalk Bay and Soetwater both had significantly shorter frond lengths than the other sites displayed.
# Olifantsbos, St James North, Yzerfontein and Oudekraal showed the largest variability in frond lengths when comparing the descriptive statistical boxplots.
# Frond mass however, displayed large variability at Oudekraal, St James and Yzerfontein and Kommetjie, with the former three sites having lager masses than that of Kommetjie.
# All other sites had relatively low variability and lighter frond mass. Sites located fairly close together such as Miller’s Point (including Miller’s A, B and C), St. James north and St. James South, and Black Rocks and Buffels, showed no significant difference in frond mass compared to each other.
# Stipe length displayed the most variability among sites, irrespective of the proximity of their locations. False Bay sites generally have longer stipes than sites along the west coast. Stipe mass is fairly similar across all sites.
# Stipe circumference is rather similar for all sites except Bakoven on the west coast and Kalk Bay in False Bay.The primary blades of E. maxima have high variability in their lengths, with longer primary blades found along the west coast, especially at olifantsbos, Oudekraal and Slangkop.


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

# Fig 2: A comparison of E. Maxima morphological variables collected from three site, at three different depths. (deep/adult collected at 1m depth, shallow/adult and shallow/juvenile collected at the shoreline)
# Both deep and shallow adult kelp show morphological variation between St James North and St James South for all variables, however the differences are not significant.
# However, all morphological variables of shallow kelp collected at Kalk Bay is significantly larger than that of adult kelp collected at 1m.  
# Juvenile kelp at all sites is significantly different in primary width, stipe circumference, stipe length, stipe mass and total length. 

# Question: at what length does adult kelp become different in appearance between sites? other sampling methods required


# deep water comparison  --------------------------------------------------


# load deep water data with required sites 
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

# Fig 3: morphological variables of E. Maxima collected at different depths ( deep/adult collected at 1m , deepest/adult collected at 7m)
# The general pattern observed from this figure, shows that kelp collected at 7m deep is larger for all variables than those collected at 1m deep. 
# Kommetjie and Soetwaterdisplay significantly different values between depths for all variables.
# Buffels bay specimens are only significant in stipe length, stipe mass, frond mass and number of tufts. 
# Oudekraal differs significantly only in stipe length and stipe mass.


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
  labs(title = "kalk bay" ) +
  theme_classic()

# Fig4: morphological variables of E. Maxima collected at Kalk Bay at different depths ( deep/adult collected at 1m , deepest/adult collected at 7m, shallow/adult and shallow/juvenile collected along the shoreline). 
# Kelp at 7m depth have a larger variability, but are not really significantly different from adult kelp in shallow water, except for stipe length, stipe mass, total length and number of tufts. 
# Adult kelp collected in 1m deep water is more similar in morphology to juvenile kelp collected at the shoreline. 

# Question: why are adult individuals growing in shallow water larger than adult individuals growing in 1m deep water? 
  

# plot Miller's Point 

mp_deep <- comb_long %>% 
  filter(site == "Miller's Point")

mp_shallow <- comb_long %>% 
  filter(date == "04-05-2018")

mp <-  rbind(mp_deep, mp_shallow)

# figure 5 
ggplot(data = mp, aes(x = site, y = value)) +
  geom_boxplot(aes(colour = depth), notch = TRUE) +
  facet_wrap(~ variable, scales = "free") +
  labs( title = "Fig5: morphological variables of E. Maxima collected at Miller’s Point at different depths (deep/adult collected at 1m, deepest/adult collected at 7m)") +
  theme_classic()

# Fig5: morphological variables of E. Maxima collected at Miller’s Point at different depths (deep/adult collected at 1m, deepest/adult collected at 7m). 
# Adult E,maxima growing at depths of 7m at Miller’s Point, differs only from 1m adults in frond length, stipe length, stipe mass, total length and number of tufts, with all these variables being larger than that of kelp collected at 1m. 
# Sites A, B and C show no significant differences in any of the morphological variables. 


### The END ###
