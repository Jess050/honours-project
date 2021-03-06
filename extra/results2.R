#results

# load libraries  ---------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(vegan)

# load data ---------------------------------------------------------------

#loading data about the morphometric properties of Ecklonia maxima

morph <- read_csv("morph.csv")

# standardise data --------------------------------------------------------

morph <- as_tibble(morph)

<<<<<<< HEAD:results1.R
standard <- morph %>%
  group_by(site) %>%
  mutate(diff_morph = morph - mean(morph, na.rm = TRUE)) %>%
  mutate(standard_morph = diff_morph / sd(morph))
||||||| merged common ancestors
=======
sites <- data.frame(unique(morph$site))
>>>>>>> 51f00b8347192e883beb6aae9272d4aff96fdb43:extra/results2.R

false_bay_sites <- sites %>% 
  slice(c(1,6:8))

west_coast_sites <- sites %>% 
  slice(c(2:5))

# standardize data for statistical analysis
stand.morph <- morph %>%
  group_by(site) %>%     # not working 
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


m1 <- dplyr::select(stand.morph, mn_fr_mass, sd_fr_mass, mn_pri_len, sd_pri_len, mn_pri_wid, sd_pri_wid, 
                    mn_fr_len, sd_fr_len, mn_st_mass, sd_st_mass, mn_st_len, sd_st_len, 
                    mn_st_circ, sd_st_circ,mn_tufts, sd_tufts, mn_epi_len, sd_epi_len, mn_total_len, sd_total_len)

#calculate z-scores 
z_morph <- decostand(m1, method = "standardize")

#combine z scores with sites 
z_morph1 <- cbind(sites, z_morph)

# visualising data --------------------------------------------------------

<<<<<<< HEAD:results1.R
#morph$ind = as.numeric(morph$ind) #needed?
||||||| merged common ancestors
#morph$ind = as.numeric(morph$ind) #needed? 
=======
>>>>>>> 51f00b8347192e883beb6aae9272d4aff96fdb43:extra/results2.R

<<<<<<< HEAD:results1.R
# convert wide data to long data
morph_long <- morph %>%
  gather(key = "variable", value = "value", -site, -ind, -date, -depth, -fertile)
||||||| merged common ancestors
# convert wide data to long data 
morph_long <- morph %>% 
  gather(key = "variable", value = "value", -site, -ind, -date, -depth, -fertile)
=======
# convert wide data to long data 
morph_long <- z_morph1 %>% 
  gather(key = "variable", value = "value", - unique.morph.site.)

>>>>>>> 51f00b8347192e883beb6aae9272d4aff96fdb43:extra/results2.R

<<<<<<< HEAD:results1.R
# visualising data
ggplot(data = morph_long, aes(x = variable, y = value, fill = site)) +
||||||| merged common ancestors
# visualising data 
ggplot(data = morph_long, aes(x = variable, y = value, fill = site)) +
=======
# visualising data 
ggplot(data = morph_long, aes(x = variable, y = value)) + #fill = unique.morph.site. 
>>>>>>> 51f00b8347192e883beb6aae9272d4aff96fdb43:extra/results2.R
  geom_boxplot() +
  coord_flip()

# facet wrap main--------------------------------------------------------------

#length
<<<<<<< HEAD:results1.R
ggplot(morph, aes(x = stipe_length, y = frond_length)) +
  geom_point(aes(colour = site)) +
||||||| merged common ancestors
ggplot(morph, aes(x = stipe_length, y = frond_length)) + 
  geom_point(aes(colour = site)) +
=======
ggplot(z_morph1, aes(x = mn_st_len, y = mn_fr_len)) + 
  geom_point(aes(colour = unique.morph.site.)) +
>>>>>>> 51f00b8347192e883beb6aae9272d4aff96fdb43:extra/results2.R
  geom_smooth(method = "lm", se = FALSE, colour = "grey35") +
  #facet_wrap(~unique.morph.site.)+
  labs(x = "Mean Stipe Length (cm)", y = "Mean Frond Length (cm)")

#primary blade
<<<<<<< HEAD:results1.R
ggplot(morph, aes(x = primary_length, y = primary_width)) +
  geom_point(aes(colour = site)) +
||||||| merged common ancestors
ggplot(morph, aes(x = primary_length, y = primary_width)) + 
  geom_point(aes(colour = site)) +
=======
#ggplot(morph, aes(x = primary_length, y = primary_width)) + 
# geom_point(aes(colour = site)) +
#  geom_smooth(method = "lm", se = FALSE, colour = "grey35") +
# facet_wrap(~site)+
# labs(x = "Primary Blade Length (cm)", y = "Primary Blade Width (cm)")

ggplot(z_morph1, aes(x = mn_pri_len, y = mn_pri_wid)) + 
  geom_point(aes(colour = unique.morph.site.)) +
>>>>>>> 51f00b8347192e883beb6aae9272d4aff96fdb43:extra/results2.R
  geom_smooth(method = "lm", se = FALSE, colour = "grey35") +
<<<<<<< HEAD:results1.R
  facet_wrap(~site)+
  labs(x = "Primary Blade Length (cm)", y = "Primary Blade Width (cm)")
#remove yzer outlier/ set axis
||||||| merged common ancestors
  facet_wrap(~site)+
  labs(x = "Primary Blade Length (cm)", y = "Primary Blade Width (cm)")
#remove yzer outlier/ set axis 
=======
  #facet_wrap(~unique.morph.site.)+
  labs(x = "Mean Primary Blade Length (cm)", y = "Mean Primary Blade Width (cm)")
>>>>>>> 51f00b8347192e883beb6aae9272d4aff96fdb43:extra/results2.R

# graph -------------------------------------------------------------------

ggplot(morph, aes(x = stipe_length, y = frond_length, colour = site, group = site)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Stipe Length (cm)", y = "Frond Length (cm)")

ggplot(morph, aes(x = primary_length, y = primary_width, colour = site, group = site)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Primary Blade Length (cm)", y = "Primary Blade Width (cm)")

#SE overlap means not sig different from each other, still need to confirm by anova or t-test

# means -------------------------------------------------------------------

<<<<<<< HEAD:results1.R
morph1 <- morph %>%
  group_by(site) %>%
  summarise(mn_st_len = mean(stipe_length),
            mn_fr_len = mean(frond_length),
            mn_fr_mass = mean(frond_mass),
            mn_st_mass = mean(stipe_mass),
            mn_pr_len = mean(primary_length),
            mn_pr_width = mean(primary_width))

morph1

||||||| merged common ancestors
morph1 <- morph %>% 
  group_by(site) %>% 
  summarise(mn_st_len = mean(stipe_length),
            mn_fr_len = mean(frond_length),
            mn_fr_mass = mean(frond_mass),
            mn_st_mass = mean(stipe_mass),
            mn_pr_len = mean(primary_length),
            mn_pr_width = mean(primary_width))

morph1
  
=======
stand.morph
  
>>>>>>> 51f00b8347192e883beb6aae9272d4aff96fdb43:extra/results2.R
# lollipop graph

ggplot(stand.morph, aes(y = mn_fr_len, x = mn_st_len)) +
  geom_col(aes(fill = site)) +
  geom_point(aes(colour = site), shape = 21, fill = "Khaki", size = 5)

<<<<<<< HEAD:results1.R
# length
ggplot(morph1, aes(y = mn_fr_len, x = mn_st_len)) +
||||||| merged common ancestors
# length 
ggplot(morph1, aes(y = mn_fr_len, x = mn_st_len)) +
=======
# length 
ggplot(stand.morph, aes(y = mn_fr_len, x = mn_st_len)) +
>>>>>>> 51f00b8347192e883beb6aae9272d4aff96fdb43:extra/results2.R
  geom_point(aes(colour = site))

# mass
ggplot(stand.morph, aes(x = mn_st_mass, y = mn_fr_mass)) +
  geom_point(aes(colour = site))

# primary blade
ggplot(stand.morph, aes(x = mn_pri_len, y = mn_pri_wid)) +
  geom_point(aes(colour = site))

# t-test ------------------------------------------------------------------
# on two sites
# load data
morphy <- read_csv("morph.csv")


# convert wide data to long data
morphy_long <- morphy %>%
  gather(key = "variable", value = "value", -site, -ind, -date, -depth, -fertile)

# visualising data
ggplot(data = morphy_long, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()

# filter the data
morphy_sub <- morphy_long %>%
  filter(variable == "stipe_length") # & frond length

# then create a new figure
ggplot(data = morphy_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot(notch = TRUE) +
  coord_flip() +
  labs(y = "Stipe length (cm)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# checking assumptions
morphy_sub %>%
  group_by(site) %>%
  summarise(stipe_length_var = var(value)[1],
            stipe_length_norm = as.numeric(shapiro.test(value)[2]))

#running an analysis

# traditional output
# t.test(value ~ site, data = morphy_sub, var.equal = TRUE, alternative = "greater")
#(reduce to 2 levels)


# Heatmap  ----------------------------------------------------------------

library(reshape2)
library(corrplot)

#  test a hypothesis  -----------------------------------------------------
cor.test(morph$frond_length, morph$stipe_length)
# p-value:  chance data is random
# CI dont cross over 0 = not significant , cross over = significant
# report Pearson coeff =


# visualise data
ggplot(data = morph, aes(x = frond_length, y = stipe_length)) +
  geom_point()


# run multiple correlations at once  --------------------------------------

morph_sub <- morph %>%
  #select(-species, - site, - ID)
  select(frond_mass:total_length, -tufts, -epi_length)

# just produces cor coefficients for whole dataset
morph_cor <- cor(morph_sub)

morph_cor


# Spearman Rank test ------------------------------------------------------

# create ordinal data
morph$length <- as.numeric(cut((morph$stipe_length + morph$frond_length), breaks = 3))
# size classes (breaks = 3)
# not really neccessary

# run a Spearman test
cor.test(morph$total_length, morph$stipe_circ, method = "spearman")
<<<<<<< HEAD:results1.R
# rho value =
# p- value =   = (not)random, (no)sig diff
# warning = little samples and range of ranking
||||||| merged common ancestors
# rho value = 
# p- value =   = (not)random, (no)sig diff
# warning = little samples and range of ranking 
=======
# rho value = 0.3868971
# p- value = 0.0001276  = (not)random, (no)sig diff
# warning = little samples and range of ranking 
>>>>>>> 51f00b8347192e883beb6aae9272d4aff96fdb43:extra/results2.R


# Kendall rank  -----------------------------------------------------------
# not normal data

# test for normlity -------------------------------------------------------

morph_norm <- morph_sub %>%
  gather(key = "variable") %>%
  group_by(variable) %>%
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))

morph_norm
#primary length, frond length and total length are not normal

cor.test(morph$primary_length, morph$primary_width, method = "kendall")
<<<<<<< HEAD:results1.R
# z value =
# tau =
# p =
||||||| merged common ancestors
# z value = 
# tau = 
# p = 
=======
# z value = 0.32067
# tau = 0.02379201
# p = 0.7485
>>>>>>> 51f00b8347192e883beb6aae9272d4aff96fdb43:extra/results2.R


# Visualise all the things  -----------------------------------------------

library(corrplot)

morph_pearson <- cor(morph_sub)

corrplot(morph_pearson, method = "circle")
# try in ggplot
# positive or negative influence size of dots

library(reshape2)

melt.morph <- melt(morph_pearson)

ggplot(melt.morph, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "Indianred", name = "Pearson Correlation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))
