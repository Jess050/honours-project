# analysing temperature data 


# load libraries ----------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(ggplot2)


# load data ---------------------------------------------------------------

load("D:/honours/honours-project/data/SACTN_monthly_v4.2.Rdata")


temp <- SACTN_monthly_v4.2 %>%
  mutate(dec1 = date %% 1, # find fractional part of number
         year = ceiling(date), # find integer part of number
         int = round((dec1 * 12) + 1, 1), # convert fraction to integer presenting month
         month = month.abb[int]) # make month abbreviation

# desriptive statistics of temp data

na.omit(temp)

summary(temp)

# monthly or annually? 
# cadv <- temp %>% 
#   filter(year == "2018")
# 
# unique(cadv$index)
# 
# unique(temp$year)

# create site data frame

sites <-  c("Muizenberg", "Muizenberg", "Muizenberg", "Kalk Bay", "Miller's Point", "Miller's Point", "Miller's Point",
            "Bordjies", "Buffelsbaai", "Kommetjie", "Kommetjie", "Kommetjie","Kommetjie", "Oudekraal", "Oudekraal", "Yzerfontein")

# separate index into site and source, and replace sites with the new sites df
temp1 <- temp %>% 
  separate(index, c("site", "source"), "/") %>% 
  filter(site %in% sites) %>% 
  select(-date, -dec1, -int)

na.omit(temp1)

summary(temp1)


sum.temp <- temp1 %>%
  group_by(site, month) %>%   
  summarise(min = min(temp, na.rm = TRUE),
            max = max(temp, na.rm = TRUE),
            med = median(temp, na.rm = TRUE),
            mean = mean(temp, na.rm = TRUE),
            sd = sd(temp,na.rm = TRUE))

sum_long <- sum.temp %>% 
  gather(key = "variable", value = "value", -site, -month)

ggplot(sum_long, aes(x = "", y = value)) +
  facet_wrap(~ variable, scales = "free") +
  geom_boxplot(aes(colour = site)) +
  theme_classic()


# temp sites replicated instead of interpolated,
# rename temp site names with wave sites name. in order for it to match to perform RDA 

tryrty <- temp1[rep(1:nrow(temp1), each = 4),]

# then remove unneccessary duplications 
sites_fin <- tryrty[-c(2:4, 6:11, 20:21, 27:31), ]

# rep mean data ? 
#tryrty <- mean[rep(1:nrow(mean), each = 4),]

# "St_James_N"
# "St James "
# "St_James_S"
# "Kalk_Bay"
# "Miller`s_A"
# "Miller`s_B"
# "Miller`s_C"
# "Black_rocks"
# "Buffels"
# "Olifantsbos"
# "Soetwater"
# "Slangkop"
# "Kommetjie"
# "Oudekraal"
# "Bakoven"
# "Yzerfontein"

# summarise by unique site and source, mean temp for all years for which there is data ?
mean <- temp1 %>% 
  group_by(site) %>% 
  summarise(mn.temp = mean(temp, na.rm = TRUE),
            sd.temp = sd(temp, na.rm = TRUE))

x_fac = factor(temp1$month, levels = month.name)

sort(x_fac)

temp2 <- temp1 %>% 
  filter(x_fac %in% month)

#visualise
ggplot(temp2, aes(x = site, y = temp)) +
  facet_wrap(~ x_fac) +
  geom_boxplot(aes(colour = site)) +
  geom_point(data = mean, aes(x = site, y = mn.temp), shape = 2) +
  theme_classic()

# order months correctly

# create matrix 
# bray -curtis // euclidean distances for RDA
# correlations between wave and morph, temp and morph, 