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
sites <- read_csv("data/sites_updated.csv")


# standardise data --------------------------------------------------------

# create numerical df to be standardized 
morph.std <- morph %>% 
  select(-date, -site, -depth, -ind, -fertile, -epi_length)

morph.stand <- decostand(morph.std, method = "standardize")

# create df with non numerical cols
df_char <- morph %>% 
  select(date:fertile)

# combine standardized data with character df
df <- cbind(df_char, morph.stand)

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# summarize morph 
sum.morph <- morph %>%
  group_by(as.factor(site)) %>%    #not working 
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
            mn_epi_len = mean(epi_length),
            sd_epi_len = sd(epi_length),
            mn_total_len = mean(total_length), 
            sd_total_len = sd(total_length))

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# boxplots on standardized data 
# convert wide data to long data 
morph_long <- morph %>% 
  gather(key = "variable", value = "value", -site, -ind, -date, -depth, -fertile, - epi_length)

try <-  data.frame(decostand(morph_long$value, method = "standardize"))

# visualising data 
ggplot(data = morph_long, aes(x = variable, y = try$decostand.morph_long.value..method....standardize.., fill = site)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap( ~ variable, scales = "free")


# boxplots on raw data 
# convert wide data to long data 
morph_long <- morph %>% 
  gather(key = "variable", value = "value", -site, -ind, -date, -depth, -fertile, -epi_length)

# visualising data 
ggplot(data = morph_long, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap( ~ variable, scales = "free") +
  theme_classic()


# visualising data 
ggplot(data = morph_long, aes(x = variable, y = value, fill = region)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap( ~ variable, scales = "free") +
  theme_classic()

# graphs 

# length
ggplot(morph, aes(x = stipe_length, y = frond_length)) + 
  geom_point(aes(colour = site), show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey35") +
  facet_wrap(~site)+
  labs(x = "Stipe Length (cm)", y = "Frond Length (cm)")

# primary blade
ggplot(morph, aes(x = primary_length, y = primary_width)) + 
  geom_point(aes(colour = site), show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey35") +
  facet_wrap(~site)+
  labs(x = "Primary Blade Length (cm)", y = "Primary Blade Width (cm)")



# with standard error (SE)
# SE overlap means not sig different from each other, still need to confirm by anova or t-test

ggplot(morph, aes(x = stipe_length, y = frond_length, colour = site, group = site)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Stipe Length (cm)", y = "Frond Length (cm)")

ggplot(morph, aes(x = primary_length, y = primary_width, colour = site, group = site)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Primary Blade Length (cm)", y = "Primary Blade Width (cm)")


# comparisons 

fb <- morph %>% 
  filter(date == "12-09-2018")


morph_long <- fb %>%
  gather(key = "variable", value = "value", -site, -ind, -date, -depth, -fertile, -epi_length) 


# visualising data 
ggplot(data = morph_long, aes(x = variable, y = value, fill = depth)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap(site ~ depth, scales = "free") +
  theme_classic()

# desriptive statistics of env data

na.omit(SACTN_monthly_v4.2)

summary(SACTN_monthly_v4.2)

ggplot(data = SACTN_monthly_v4.2, aes(x = date, y = temp)) +
  geom_boxplot()


# i.	Calculate an association matrix for the species data. Show the code used to produce the calculation. (1)
# ii.	Briefly describe the meaning of the data represented in the association matrix, make use of specific references to cells within the matrix in your explanation. (4)

# i)
.t <- t(SACTN_monthly_v4.2)
temp.t[1:20, 1:20]
temp.t.S7 <- vegdist(temp.t, binary = TRUE)
round(as.matrix(temp.t.S7)[1:15, 1:15], 2)
