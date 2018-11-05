# Redundancy analysis (RDA)
# *************************

# Load the required packages
# (vegan must be loaded after ade4 to avoid some conflicts)
library(tidyverse)
library(ggpubr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(stringr)
library(broom)
library(purrr)
library(circular)
library(zoo) # format date 
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


# Wave Climatology  -------------------------------------------------------

# writing a function for extraction of wave data 
load_wave <- function(wave_files) {
  site_name <- sapply(strsplit(as.character(wave_files), "/"), "[[", 3)
  site_info <- sapply(strsplit(as.character(wave_files), "/"), "[[", 4)
  site_info <- sapply(strsplit(site_info, ".txt"), "[[", 1)
  site_info <- strsplit(site_info, "_")
  site_depth <- sapply(site_info, "[[", 1)
  site_num <- sapply(site_info, "[[", 2)
  
  wave <- read_table(wave_files, col_types = c("dddddd"),
                     col_names = c("date", "hs", "tp", "dir", "dirw", "spw")) %>%
    filter(tp != -999) %>%
    mutate(date = as.POSIXct(as.character(date), "%Y%m%d%H%M", tz = "Africa/Johannesburg")) %>%
    mutate(site = site_name,
           num = site_num,
           depth = site_depth) %>%
    select(site, num, depth, everything()) %>% 
    na.omit()
  return(wave)
}

#  column names: date, wave height, wave period, wave direction, wind direction, wind speed

# loading wave data of selected sites 
Bakoven <- map_dfr(dir("data/wave_data/Bakoven", 
                       full.names = TRUE, pattern = "*.txt"), load_wave)
Black_rocks <- map_dfr(dir("data/wave_data/Black_rocks", 
                           full.names = TRUE, pattern = "*.txt"), load_wave)
Buffels <- map_dfr(dir("data/wave_data/Buffels", 
                       full.names = TRUE, pattern = "*.txt"), load_wave)
Kalk_bay <- map_dfr(dir("data/wave_data/Kalk_Bay", 
                        full.names = TRUE, pattern = "*.txt"), load_wave)
Kommetjie <- map_dfr(dir("data/wave_data/Kommetjie", 
                         full.names = TRUE, pattern = "*.txt"), load_wave)
Millers_A <- map_dfr(dir("data/wave_data/Millers_A", 
                         full.names = TRUE, pattern = "*.txt"), load_wave)
Millers_B <- map_dfr(dir("data/wave_data/Millers_B", 
                         full.names = TRUE, pattern = "*.txt"), load_wave)
Millers_C <- map_dfr(dir("data/wave_data/Millers_C", 
                         full.names = TRUE, pattern = "*.txt"), load_wave)
Olifantsbos <- map_dfr(dir("data/wave_data/Olifantsbos", 
                           full.names = TRUE, pattern = "*.txt"), load_wave)
Oudekraal <- map_dfr(dir("data/wave_data/Oudekraal", 
                         full.names = TRUE, pattern = "*.txt"), load_wave)
Slangkop <- map_dfr(dir("data/wave_data/Slangkop", 
                        full.names = TRUE, pattern = "*.txt"), load_wave)
Soetwater <- map_dfr(dir("data/wave_data/Soetwater", 
                         full.names = TRUE, pattern = "*.txt"), load_wave)
St_James <- map_dfr(dir("data/wave_data/St_James", 
                        full.names = TRUE, pattern = "*.txt"), load_wave)
St_James_N <- map_dfr(dir("data/wave_data/St_James_N", 
                          full.names = TRUE, pattern = "*.txt"), load_wave)
St_James_S <- map_dfr(dir("data/wave_data/St_James_S", 
                          full.names = TRUE, pattern = "*.txt"), load_wave)
Yzerfontein <- map_dfr(dir("data/wave_data/Yzerfontein", 
                           full.names = TRUE, pattern = "*.txt"), load_wave)


# combine sites into one df
wave_data <- rbind(Bakoven, Black_rocks, Buffels, Kalk_bay, Kommetjie, Millers_A, Millers_B, Millers_C, 
                   Olifantsbos, Oudekraal, Slangkop, Soetwater, St_James, St_James_N, St_James_S, Yzerfontein) 


rm(Bakoven, Black_rocks, Buffels, Kalk_bay, Kommetjie, Millers_A, Millers_B, Millers_C, 
   Olifantsbos, Oudekraal, Slangkop, Soetwater, St_James, St_James_N, St_James_S, Yzerfontein)

wave <- wave_data[,-2:-4]

# Annual climatology for the wave data
wave_env <- wave %>% 
  group_by(site) %>% 
  summarise_all(funs(mean = mean, sd = sd, min = min, max = max), na.rm = T) %>%
  ungroup() %>% 
  group_by(site) %>% 
  select(site, everything())
# 
# range <- wave_env %>% 
#   group_by(site) %>% 
#   summarise_all(dir_range = dir_max - dir_min, 
#                 dirw_range = dirw_max - dirw_min,
#                 hs_range = hs_max - hs_min,
#                 tp_range = tp_max - tp_min,
#                 spw_range = spw_max - spw_min)



# managing site names --------------------------------------------------------


# temp sites replicated instead of interpolated, 
# rename temp site names with wave sites name. in order for it to match to perform RDA 

unique(temps$site)


# oudekraal rep for bakoven 
oude <- temp_monthly %>% 
  filter(site == "Oudekraal") 

oudekraal <- oude

oude$newcolumn<-"Bakoven"
colnames(oude)[colnames(oude)=="newcolumn"] <- "site"
bakoven <-  oude[,-1]


# Kommetjie for slangkop, Soetwater and Olifantsbos
kom <- temp_monthly %>% 
  filter(site == "Kommetjie") 

#kommetjie
Kom <- kom

#slangkop
kom$newcolumn<-"Slangkop"
colnames(kom)[colnames(kom)=="newcolumn"] <- "site"
slangkop <-  kom[,-1]

#soetwater 
kom$newcolumn <- "Soetwater"
colnames(kom)[colnames(kom)=="newcolumn"] <- "site"
soet<-  kom[,-1]
soetwater <- soet[,-7]

# Olifantsbos 
kom$newcolumn <- "Olifantsbos"
colnames(kom)[colnames(kom)=="newcolumn"] <- "site"
oli <- kom[,-8:-9]
Olifantsbos <- oli[,-1]

#bordjies for Black rocks 
bord <- temp_monthly %>% 
  filter(site == "Bordjies") 

bord$newcolumn<-"Black_rocks"
colnames(bord)[colnames(bord)=="newcolumn"] <- "site"
black_rocks <-  bord[,-1]

# Buffelsbaai rename to Buffels 
buff <- temp_monthly %>% 
  filter(site == "Buffelsbaai") 

buff$newcolumn<-"Buffels"
colnames(buff)[colnames(buff)=="newcolumn"] <- "site"
buffels <-  buff[,-1]


# Miller`s Point for millers A, B and C
millers <- temp_monthly %>% 
  filter(site == "Miller's Point") 

millers$newcolumn<-"Millers_A"
colnames(millers)[colnames(millers)=="newcolumn"] <- "site"
millers_A <-  millers[,-1]


# millers B
millers$newcolumn<-"Millers_B"
colnames(millers)[colnames(millers)=="newcolumn"] <- "site"
mill_B <-  millers[,-1]
millers_B <- mill_B[,-7]

# millers C
millers$newcolumn<-"Millers_C"
colnames(millers)[colnames(millers)=="newcolumn"] <- "site"
mill_C <-  millers[,-8:-9]
millers_C <- mill_C[,-1]


#Muizenberg for St James N 
muiz <- temp_monthly %>% 
  filter(site == "Muizenberg") 

muiz$newcolumn<-"St_James_N"
colnames(muiz)[colnames(muiz)=="newcolumn"] <- "site"
st_james_N <-  muiz[,-1]

# kalk bay for St James, and St James S
kalk <- temp_monthly %>% 
  filter(site == "Kalk Bay") 

# change kalk bay name 
kalk$newcolumn<-"Kalk_Bay"
colnames(kalk)[colnames(kalk)=="newcolumn"] <- "site"
Kalk_Bay <-  kalk[,-1]


# St JAmes 
kalk$newcolumn<-"St_James"
colnames(kalk)[colnames(kalk)=="newcolumn"] <- "site"
St_J <-  kalk[,-1]
St_James <- St_J[,-7]

# St JAmes S 
kalk$newcolumn<-"St_James_S"
colnames(kalk)[colnames(kalk)=="newcolumn"] <- "site"
St_J_S <-  kalk[,-8:-9]
St_James_S <- St_J_S[,-1]

# yzer 
yzer <- temp_monthly %>% 
  filter(site == "Yzerfontein")

monthly_temp <- rbind(bakoven, black_rocks, buffels, Kalk_Bay, Kom, millers_A, millers_B, millers_C, 
                      Olifantsbos, oudekraal, slangkop, soetwater, St_James, st_james_N, St_James_S, yzer) 

                
# temp annual 
ann_temp <-  monthly_temp %>% 
  group_by(site) %>% 
  gather(key = statistic, value = value, 
         mean_temp:sd_temp) %>% 
  mutate(clim_stat = paste(date, statistic, sep = "_")) %>% 
  select(-date, -statistic) %>% 
  spread(key = clim_stat, value = value)


# merge monthly temp and wave data to make env data for the RDA 
try_env <- merge(wave_env, ann_temp, by = c("site"))

# remove columns with values = 0
work <- try_env[, colSums(try_env != 0) > 0]


# load in morphology data 
morph <- as.tibble(read_csv("D:/honours/honours-project/data/morph_update.csv")[, -14])

# create morph df with  numerical cols
morph_spe <- morph %>% 
  filter(depth == "deep/adult") %>% 
  select(-date, -ind, -fertile)

# summarize morph 
sum.morph <- morph_spe %>%
  group_by(as.factor(site), depth) %>%   
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

spe <- sum.morph[,-1:-2]


# RDA  --------------------------------------------------------------------

# Hellinger-transform the species dataset
morph.hel <- as.tibble(decostand(spe, "hellinger"))
morph.hel

# env2 <- env[-1:-2]
(morph.rda <- rda(morph.hel ~ ., work))

#total inertia constrained axis (RDA)
sum(morph.rda$CCA$eig)

# total inertia unconstrained axis (PC)
sum(morph.rda$CA$eig)

summary(morph.rda)	# Scaling 2 (default)

# Canonical coefficients from the rda object
coef(morph.rda)
# Unadjusted R^2 retrieved from the rda object
(R2 <- RsquareAdj(morph.rda)$r.squared)
# Adjusted R^2 retrieved from the rda object
(R2adj <- RsquareAdj(morph.rda)$adj.r.squared) #report this value 

## Triplots of the rda results (wa scores)
## Site scores as weighted averages (vegan's default)
# Scaling 1: distance triplot
dev.new(title="RDA scaling 1 + wa")
plot(morph.rda, scaling=1, 
     main="Triplot RDA morph.hel ~ try.env - scaling 1 - wa scores")
spe.sc1 <- scores(morph.rda, choices=1:2, scaling=1, display="sp")
arrows(0, 0, spe.sc1[, 1]*0.92, spe.sc1[, 2]*0.92, length=0, lty=1, col="red")

# Scaling 2 (default): correlation triplot
dev.new(title="RDA scaling 2 + wa")
plot(morph.rda, main="Triplot RDA morph.hel ~ try.env - scaling 2 - wa scores")
spe.sc2 <- scores(morph.rda, choices=1:2, display="sp")
arrows(0, 0, spe.sc2[, 1]*0.92, spe.sc2[, 2]*0.92, length=0, lty=1, col="red")

## Triplots of the rda results (lc scores)
## Site scores as linear combinations of the environmental variables
# Scaling 1
dev.new(title="RDA scaling 1 + lc")
plot(morph.rda, scaling=1, display=c("sp", "lc", "cn"), 
     main="Triplot RDA morph.hel ~ try.env - scaling 1 - lc scores")
arrows(0, 0, spe.sc1[, 1]*0.92, spe.sc1[, 2]*0.92, length=0, lty=1, col="red")

# Scaling 2
dev.new(title="RDA scaling 2 + lc")
plot(morph.rda, display=c("sp", "lc", "cn"), 
     main="Triplot RDA morph.hel ~ try.env - scaling 2 - lc scores")
arrows(0, 0, spe.sc2[,1]*0.92, spe.sc2[,2]*0.92, length=0, lty=1, col="red")


# testing hypothesis
#  H0:the env is not having an effect
# H1: the env is having an effect

# Global test of the RDA result
anova(morph.rda, permutations=how(nperm=999))

# which axes are NB 
# Tests of all canonical axes
anova(morph.rda, by="axis", permutations=how(nperm=999))

#which env variables are responsible in influencing the landscape of spp? 
anova(morph.rda, by="term", permutations=how(nperm=999))


# Variance inflation factors (VIF) (extracts colinearity)
vif.cca(morph.rda)
# exclude certain variables then run again 
# no exact answer, dependent on ur argument 

# Apply Kaiser-Guttman criterion to residual axes
morph.rda$CA$eig[morph.rda$CA$eig > mean(morph.rda$CA$eig)]

