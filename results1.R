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


# standardise data --------------------------------------------------------

#still needs to be completed

morph <- as_tibble(morph)

standard <- morph %>%
  group_by(site) %>%
  mutate(diff_morph = morph - mean(morph, na.rm = TRUE)) %>%
  mutate(standard_morph = diff_morph / sd(morph))

#n1 - standardization ((x-mean)/sd)
#mutate(var = (var - mean(var))/sd(var))
#zVar <- (myVar - mean(myVar)) / sd(myVar)

zVar <- (morph - mean(morph)) / sd(morph)

library(standardize)
standardize(morph)


unique(morph$site)




#morph$ind = as.numeric(morph$ind) #needed?

# convert wide data to long data
morph_long <- morph %>%
  gather(key = "variable", value = "value", -site, -ind, -date, -depth, -fertile)

# visualising data
ggplot(data = morph_long, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()

# facet wrap main--------------------------------------------------------------

#length
ggplot(morph, aes(x = stipe_length, y = frond_length)) +
  geom_point(aes(colour = site)) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey35") +
  facet_wrap(~site)+
  labs(x = "Stipe Length (cm)", y = "Frond Length (cm)")

#primary blade
ggplot(morph, aes(x = primary_length, y = primary_width)) +
  geom_point(aes(colour = site)) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey35") +
  facet_wrap(~site)+
  labs(x = "Primary Blade Length (cm)", y = "Primary Blade Width (cm)")
#remove yzer outlier/ set axis

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

morph1 <- morph %>%
  group_by(site) %>%
  summarise(mn_st_len = mean(stipe_length),
            mn_fr_len = mean(frond_length),
            mn_fr_mass = mean(frond_mass),
            mn_st_mass = mean(stipe_mass),
            mn_pr_len = mean(primary_length),
            mn_pr_width = mean(primary_width))

morph1

# lollipop graph

ggplot(morph1, aes(y = mn_fr_len, x = mn_st_len)) +
  geom_col(aes(fill = site)) +
  geom_point(aes(colour = site), shape = 21, fill = "Khaki", size = 5)

# length
ggplot(morph1, aes(y = mn_fr_len, x = mn_st_len)) +
  geom_point(aes(colour = site))

# mass
ggplot(morph1, aes(x = mn_st_mass, y = mn_fr_mass)) +
  geom_point(aes(colour = site))

# primary blade
ggplot(morph1, aes(x = mn_pr_len, y = mn_pr_width)) +
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
  geom_boxplot() +
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
# rho value =
# p- value =   = (not)random, (no)sig diff
# warning = little samples and range of ranking


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
# z value =
# tau =
# p =


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
