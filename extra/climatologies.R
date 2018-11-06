
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


# Temperature Climatology -------------------------------------------------

load("D:/honours/honours-project/data/SACTN_monthly_v4.2.Rdata")

# create site data frame

sites <-  c("Muizenberg", "Muizenberg", "Muizenberg", "Kalk Bay", "Miller's Point", "Miller's Point", "Miller's Point",
            "Bordjies", "Buffelsbaai", "Kommetjie", "Kommetjie", "Kommetjie","Kommetjie", "Oudekraal", "Oudekraal", "Yzerfontein")

# separate index into site and source, and replace sites with the new sites df
temps <- SACTN_monthly_v4.2 %>% 
  separate(index, c("site", "source"), "/") %>% 
  filter(site %in% sites)
  

  # Monthly climatology
  temp_monthly <- temps %>% 
    mutate(date = lubridate::month(date, label = TRUE)) %>% 
    group_by(site, date) %>% 
    summarise(mean_temp = mean(temp, na.rm = T),
              min_temp = min(temp, na.rm = T),
              max_temp = max(temp, na.rm = T),
              range_temp = range(temp, na.rm = T)[2]-range(temp, na.rm = T)[1],
              sd_temp = sd(temp, na.rm = T)) %>%
    filter(date %in% c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) %>% 
    ungroup()
  

  # Annual climatology
  annual_temperature <- temps %>%
    mutate(date = lubridate::month(date, label = T)) %>% 
    group_by(site, date) %>% 
    summarise(mean_temp = mean(temp, na.rm = T),
              min_temp = min(temp, na.rm = T),
              max_temp = max(temp, na.rm = T),
              range_temp = range(temp, na.rm = T)[2]-range(temp, na.rm = T)[1],
              sd_temp = sd(temp, na.rm = T)) %>% 
    ungroup() %>% 
    select(-date) %>% 
    group_by(site) %>% 
    summarise_all(funs(mean(., na.rm = T))) %>% 
    mutate(date = "Annual") %>% 
    select(site, date, everything())
  
 ann_temp <-  annual_temperature %>% 
    group_by(site) %>% 
    gather(key = statistic, value = value, 
           mean_temp:sd_temp) %>% 
    mutate(clim_stat = paste(date, statistic, sep = "_")) %>% 
    select(-date, -statistic) %>% 
    spread(key = clim_stat, value = value)
 
 # plot graphs 
 
 ggplot(temps, aes(x = month, y = temp )) + 
   facet_wrap(~ site) +
   geom_line(data = temp_monthly, aes(x = date, y = mean_temp, group = 1), colour = "green") +
   geom_line(data = temp_monthly, aes(x = date, y = min_temp, group = 1), colour = "blue") +
   geom_line(data = temp_monthly, aes(x = date, y = max_temp, group = 1), colour = "red") +
   labs(title = "Temperature ('C)") +
   theme_classic() 

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
 
  
  # Annual climatology for the wave data
  wave_annually <- wave_data %>% 
    mutate(date = lubridate::month(date, label = T)) %>% 
    group_by(site, date) %>% 
    summarise_all(funs(mean = mean, sd = sd), na.rm = T) %>%
    ungroup() %>% 
    select(-date) %>% 
    group_by(site) %>% 
    summarise_all(funs(mean), na.rm = T) %>% 
    mutate(date = "Annual") %>% 
    select(site, date, everything())
  
  # monthly climatology 
  wave_monthly <- wave_data %>% 
    mutate(date = lubridate::month(date, label = T)) %>% 
    group_by(site, date) %>% 
    summarise(dir_min = min(dir, na.rm = T),
              dir_max = max(dir, na.rm = T),
              dir_range = range(dir, na.rm = T)[2]-range(dir, na.rm = T)[1],
              dir_mean = mean(dir, na.rm = T), 
              dirw_min = min(dirw, na.rm = T),
              dirw_max = max(dirw, na.rm = T),
              dirw_range = range(dirw, na.rm = T)[2]-range(dirw, na.rm = T)[1],
              dirw_mean = mean(dirw, na.rm = T),
              hs_min = min(hs, na.rm = T),
              hs_max = max(hs, na.rm = T),
              hs_range = range(hs, na.rm = T)[2]-range(hs, na.rm = T)[1],
              hs_mean = mean(hs, na.rm = T),
              tp_min = min(tp, na.rm = T),
              tp_max = max(tp, na.rm = T),
              tp_range = range(tp, na.rm = T)[2]-range(tp, na.rm = T)[1],
              tp_mean = mean(tp, na.rm = T),
              spw_min = min(spw, na.rm = T),
              spw_max = max(spw, na.rm = T),
              spw_range = range(spw, na.rm = T)[2]-range(spw, na.rm = T)[1],
              spw_mean = mean(spw, na.rm = T)) %>%
    filter(date %in% c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) %>% 
    ungroup()
  
  # graphs 
  
  ggplot(wave_monthly, aes(x = month, y = hs)) + 
    facet_wrap(~ site) +
    geom_line(data = wave_monthly, aes(x = date, y = hs_mean, group = 1), colour = "green") +
    geom_line(data = wave_monthly, aes(x = date, y = hs_min, group = 1), colour = "blue") +
    geom_line(data = wave_monthly, aes(x = date, y = hs_max, group = 1), colour = "red") +
    labs(title = "Wave height (hs)")+
    theme_classic()
  
  
  ggplot(wave_monthly, aes(x = month, y = dir)) + 
    facet_wrap(~ site) +
    geom_line(data = wave_monthly, aes(x = date, y = dir_mean, group = 1), colour = "green") +
    geom_line(data = wave_monthly, aes(x = date, y = dir_min, group = 1), colour = "blue") +
    geom_line(data = wave_monthly, aes(x = date, y = dir_max, group = 1), colour = "red") +
    labs(title = "Wave direction (dir)")+
    theme_classic()
  
  ggplot(wave_monthly, aes(x = month, y = tp)) + 
    facet_wrap(~ site) +
    geom_line(data = wave_monthly, aes(x = date, y = tp_mean, group = 1), colour = "green") +
    geom_line(data = wave_monthly, aes(x = date, y = tp_min, group = 1), colour = "blue") +
    geom_line(data = wave_monthly, aes(x = date, y = tp_max, group = 1), colour = "red") +
    labs(title = "Wave period (tp)")+
    theme_classic()

  ggplot(wave_monthly, aes(x = month, y = dirw)) + 
    facet_wrap(~ site) +
    geom_line(data = wave_monthly, aes(x = date, y = dirw_mean, group = 1), colour = "green") +
    geom_line(data = wave_monthly, aes(x = date, y = dirw_min, group = 1), colour = "blue") +
    geom_line(data = wave_monthly, aes(x = date, y = dirw_max, group = 1), colour = "red") +
    labs(title = "Wind direction (dirw)")+
    theme_classic()
  
  ggplot(wave_monthly, aes(x = month, y = spw)) + 
    facet_wrap(~ site) +
    geom_line(data = wave_monthly, aes(x = date, y = spw_mean, group = 1), colour = "green") +
    geom_line(data = wave_monthly, aes(x = date, y = spw_min, group = 1), colour = "blue") +
    geom_line(data = wave_monthly, aes(x = date, y = spw_max, group = 1), colour = "red") +
    labs(title = "Wind speed (spw)") +
    theme_classic()
  
  

# RDA  --------------------------------------------------------------------

  
  
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




# Redundancy analysis (RDA)
# *************************

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


# load in morphology data 
morph <- as.tibble(read_csv("D:/honours/honours-project/data/morph12.csv")[, -14])



# merge monthly temp and wave data to make env data for the RDA 
env <- merge(wave_monthly, monthly_temp, by= c("site", "date"))


# create morph df with  numerical cols
morph_spe <- morph %>% 
  filter(depth == "deep/adult") %>% 
  select(-date:-fertile)

# Hellinger-transform the species dataset
morph.hel <- as.tibble(decostand(morph_spe, "hellinger"))
morph.hel

env2 <- env[-1:-2]

(morph.rda <- rda(morph.hel ~ ., env2))
# number of rows dont add up for morph and env 
# will need to remove a sample from each site 
# env is monthly data so only has 12 rows per site (192 rows)
# morph, 13 samples were collected per site (208 rows) <- <- <-  extra rows were removed 
   
  #total inertia constrained axis (RDA)
  sum(morph.rda$CCA$eig)
  
  # total inertia unconstrained axis (PC)
  sum(morph.rda$CA$eig)
  
  summary(morph.rda)	# Scaling 2 (default)
  
  
  # 1st RDA axis is comprised of Satr and Alal etc.. 
  
  # Canonical coefficients from the rda object
  coef(morph.rda)
  # Unadjusted R^2 retrieved from the rda object
  (R2 <- RsquareAdj(morph.rda)$r.squared)
  # Adjusted R^2 retrieved from the rda object
  (R2adj <- RsquareAdj(morph.rda)$adj.r.squared) #report this value 
  
  ## IMPORTANT NOTE: a bug in the scaling 1 biplot scores of explanatory variables
  ## has been corrected in vegan 2.4-2. The arrows of the explanatory variables 
  ## in scaling 1 biplots in the book are incorrect.
  
  ## Triplots of the rda results (wa scores)
  ## Site scores as weighted averages (vegan's default)
  # Scaling 1: distance triplot
  dev.new(title="RDA scaling 1 + wa")
  plot(morph.rda, scaling=1, 
       main="Triplot RDA morph.hel ~ env2 - scaling 1 - wa scores")
  spe.sc1 <- scores(morph.rda, choices=1:2, scaling=1, display="sp")
  arrows(0, 0, spe.sc1[, 1]*0.92, spe.sc1[, 2]*0.92, length=0, lty=1, col="red")
  
  # Scaling 2 (default): correlation triplot
  dev.new(title="RDA scaling 2 + wa")
  plot(morph.rda, main="Triplot RDA morph.hel ~ env2 - scaling 2 - wa scores")
  spe.sc2 <- scores(morph.rda, choices=1:2, display="sp")
  arrows(0, 0, spe.sc2[, 1]*0.92, spe.sc2[, 2]*0.92, length=0, lty=1, col="red")
  
  ## Triplots of the rda results (lc scores)
  ## Site scores as linear combinations of the environmental variables
  # Scaling 1
  dev.new(title="RDA scaling 1 + lc")
  plot(morph.rda, scaling=1, display=c("sp", "lc", "cn"), 
       main="Triplot RDA morph.hel ~ env2 - scaling 1 - lc scores")
  arrows(0, 0, spe.sc1[, 1]*0.92, spe.sc1[, 2]*0.92, length=0, lty=1, col="red")
  
  # Scaling 2
  dev.new(title="RDA scaling 2 + lc")
  plot(morph.rda, display=c("sp", "lc", "cn"), 
       main="Triplot RDA morph.hel ~ env2 - scaling 2 - lc scores")
  arrows(0, 0, spe.sc2[,1]*0.92, spe.sc2[,2]*0.92, length=0, lty=1, col="red")
  
  
  # testing hypothesis
  #  H0:the env is not having an effect
  # H1: the env is having an effect
  
  # Global test of the RDA result
  anova(morph.rda, permutations=how(nperm=999))
  # env variables is having an effect ( p = 0.012)
  
  # which axes are NB 
  # Tests of all canonical axes
  anova(morph.rda, by="axis", permutations=how(nperm=999))
  # RDA 1 is NB 
  
  #which env variables are responsible in influencing the landscape of spp? 
  anova(morph.rda, by="term", permutations=how(nperm=999))
  # dir_min and dir_median*
  # but ... look at the graph 
  # use graph for the direction of influence 
  
  
  # pho, amm and bod are correlated (colinear) therefore
  # Variance inflation factors (VIF) (extracts colinearity)
  vif.cca(morph.rda)
  # BOD is a consequence of amm or pho
  # exclude certain variables then run again 
  # no exact answer, dependent on ur argument 
  
  # Apply Kaiser-Guttman criterion to residual axes
  morph.rda$CA$eig[morph.rda$CA$eig > mean(morph.rda$CA$eig)]
  
  
  
  
  
  
  