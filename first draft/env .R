
# load libraries  
library(tidyverse)
library(ggpubr)
library(dplyr) #for filter and %>%  command
library(readr) #read csv
library(ggplot2)
library(vegan)
library(zoo)

# Complete lon/lats
kelp_sites <- read_csv("data/sites_fin.csv")

# Ecklonia
eck_data <- as.tibble(read_csv("data/morph_update.csv")[, -14])


# The SACTN monthly data
load("data/SACTN_monthly_v4.2.Rdata")

sites <-  c("Muizenberg", "Muizenberg", "Muizenberg", "Kalk Bay", "Miller's Point", "Miller's Point", "Miller's Point",
            "Bordjies", "Buffelsbaai", "Kommetjie", "Kommetjie", "Kommetjie","Kommetjie", "Oudekraal", "Oudekraal", "Yzerfontein")


temps <- SACTN_monthly_v4.2 %>% 
  separate(index, c("site", "source"), "/") %>% 
  filter(site %in% sites)

# oudekraal rep for bakoven 
oude <- temps %>% 
  filter(site == "Oudekraal") 

oudekraal <- oude

oude$newcolumn<-"Bakoven"
colnames(oude)[colnames(oude)=="newcolumn"] <- "site"
bakoven <-  oude[,-1]

# Kommetjie for slangkop, Soetwater and Olifantsbos
kom <- temps %>% 
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
soet<-  kom[,-5]
soetwater <- soet[,-1]

# Olifantsbos 
kom$newcolumn <- "Olifantsbos"
colnames(kom)[colnames(kom)=="newcolumn"] <- "site"
oli <- kom[,-5:-6]
Olifantsbos <- oli[,-1]

#bordjies for Black rocks 
bord <- temps %>% 
  filter(site == "Bordjies") 

bord$newcolumn<-"Black_rocks"
colnames(bord)[colnames(bord)=="newcolumn"] <- "site"
black_rocks <-  bord[,-1]

# Buffelsbaai rename to Buffels 
buff <- temps %>% 
  filter(site == "Buffelsbaai") 

buff$newcolumn<-"Buffels"
colnames(buff)[colnames(buff)=="newcolumn"] <- "site"
buffels <-  buff[,-1]

# Miller`s Point for millers A, B and C
millers <- temps %>% 
  filter(site == "Miller's Point") 

millers$newcolumn<-"Millers_A"
colnames(millers)[colnames(millers)=="newcolumn"] <- "site"
millers_A <-  millers[,-1]

# millers B
millers$newcolumn<-"Millers_B"
colnames(millers)[colnames(millers)=="newcolumn"] <- "site"
mill_B <-  millers[,-1]
millers_B <- mill_B[,-4]

# millers C
millers$newcolumn<-"Millers_C"
colnames(millers)[colnames(millers)=="newcolumn"] <- "site"
mill_C <-  millers[,-5:-6]
millers_C <- mill_C[,-1]

#Muizenberg for St James N 
muiz <- temps %>% 
  filter(site == "Muizenberg") 

muiz$newcolumn<-"St_James_N"
colnames(muiz)[colnames(muiz)=="newcolumn"] <- "site"
st_james_N <-  muiz[,-1]

# kalk bay for St James, and St James S
kalk <- temps %>% 
  filter(site == "Kalk Bay") 

# change kalk bay name 
kalk$newcolumn<-"Kalk_Bay"
colnames(kalk)[colnames(kalk)=="newcolumn"] <- "site"
Kalk_Bay <-  kalk[,-1]

# St JAmes 
kalk$newcolumn<-"St_James"
colnames(kalk)[colnames(kalk)=="newcolumn"] <- "site"
St_J <-  kalk[,-1]
St_James <- St_J[,-4]

# St JAmes S 
kalk$newcolumn<-"St_James_S"
colnames(kalk)[colnames(kalk)=="newcolumn"] <- "site"
St_J_S <-  kalk[,-5:-6]
St_James_S <- St_J_S[,-1]

# yzer 
yzer <- temps %>% 
  filter(site == "Yzerfontein")

# create new temp df with data for all sites 
temps_sites <- rbind(bakoven, black_rocks, buffels, Kalk_Bay, Kom, millers_A, millers_B, millers_C, 
                      Olifantsbos, oudekraal, slangkop, soetwater, St_James, st_james_N, St_James_S, yzer)


# Annual clims
temp_ann <- temps_sites %>% 
  mutate(date = lubridate::month(date, label = TRUE)) %>% 
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
  mutate(date = "Ann") %>% 
  select(site, date, everything())


# Monthly clims
temp_monthly <- temps_sites %>% 
  mutate(date = lubridate::month(date, label = TRUE)) %>% 
  group_by(site, date) %>% 
  summarise(mean_temp = mean(temp, na.rm = T),
            min_temp = min(temp, na.rm = T),
            max_temp = max(temp, na.rm = T),
            range_temp = range(temp, na.rm = T)[2]-range(temp, na.rm = T)[1],
            sd_temp = sd(temp, na.rm = T)) %>%
  filter(date %in% c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) %>% 
  ungroup()

# temperature climatology
temp_clim <- rbind(annual_temp, temp_monthly) %>% 
  group_by(site) %>% 
  gather(key = statistic, value = value, 
         mean_temp:sd_temp) %>% 
  mutate(clim_stat = paste(date, statistic, sep = "_")) %>% 
  select(-date, -statistic) %>% 
  spread(key = clim_stat, value = value)
# 
# # plot 
# ggplot(temps_sites, aes(x = date, y = temp )) + 
#   facet_wrap(~ site) +
#   geom_line(data = temp_monthly, aes(x = date, y = mean_temp, group = 1), colour = "green") +
#   geom_line(data = temp_monthly, aes(x = date, y = min_temp, group = 1), colour = "blue") +
#   geom_line(data = temp_monthly, aes(x = date, y = max_temp, group = 1), colour = "red") +
#   labs(title = "Temperature ('C)") +
#   theme_classic() 

env3 <- temp_clim %>% 
  dplyr::ungroup(site) %>% 
  dplyr::select(-site) %>% 
  vegdist(method = 'euclidian')
env3.mat <- as.matrix(env3)
env3.diag <- diag(env3.mat[-1, -nrow(env3.mat)])
env3.diag <- append(0, env3.diag, after = 1)

#  wave data  -------------------------------------------------------------


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
wave_annual <- wave_data[-2:-3] %>% 
  mutate(date = lubridate::month(date, label = T)) %>% 
  group_by(site, date) %>% 
  summarise_all(funs(mean = mean, sd = sd, min = min, max = max), na.rm = T) %>%
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
  summarise(hs_mean = mean(hs, na.rm = T),
            dir_min = min(dir, na.rm = T),
            dir_max = max(dir, na.rm = T),
            dir_mean = mean(dir, na.rm = T),
            dir_sd = sd(dir, na.rm = T),
            dirw_min = min(dirw, na.rm = T),
            dirw_max = max(dirw, na.rm = T),
            dirw_mean = mean(dirw, na.rm = T),
            dirw_sd = sd(dirw, na.rm = T),
            hs_min = min(hs, na.rm = T),
            hs_max = max(hs, na.rm = T),
            hs_sd = sd(hs, na.rm = T),
            tp_min = min(tp, na.rm = T),
            tp_max = max(tp, na.rm = T),
            tp_mean = mean(tp, na.rm = T),
            tp_sd = sd(tp, na.rm = T),
            spw_min = min(spw, na.rm = T),
            spw_mean = mean(spw, na.rm = T), 
            spw_sd = sd(spw, na.rm = T), 
            spw_max = max(spw, na.rm = T)) %>%
  filter(date %in% c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) %>% 
  ungroup()

# wave climatology

wave_clim <- rbind(wave_monthly, wave_annual) %>% 
  group_by(site) %>% 
  gather(key = statistic, value = value, 
         hs_mean:spw_max) %>% 
  mutate(clim_stat = paste(date, statistic, sep = "_")) %>% 
  select(-date, -statistic) %>% 
  spread(key = clim_stat, value = value) %>% 
  ungroup() 


# Ecklonia
eck_mean <- eck_data %>% 
  select(site, frond_mass:total_length) %>% 
  group_by(site) %>% 
  summarise_all(funs(mean(., na.rm = T)))

# Merge the biotic and abiotic variables
# Ecklonia
eck_all <- eck_mean %>% 
  left_join(temp_clim, by = "site") %>%
  left_join(wave_clim, by = "site")

# Hellinger-transform the morph dataset
eck_bio <- eck_all %>% 
  select(frond_mass:total_length) %>% 
  decostand(method = "hellinger")

eck_abio <- cbind(temp_clim, wave_clim) %>% 
  decostand(method = "standardize")

eck_abio <- merge(wave_clim, temp_clim, by = c("site")) %>% 
  select(-site) %>% 
  decostand(method = "standardize")

# standardization resulted in columns with NA values, removes these columns
abio <- eck_abio[,c(-145,-147,-245,-247,-255)]

# run RDA 
eck_RDA <- rda(eck_bio ~ ., data = abio)
eck_RDA 

# #total inertia constrained axis (RDA)
# sum(eck_RDA$CCA$eig)
# 
# # total inertia unconstrained axis (PC)
# sum(eck_RDA$CA$eig)

summary(eck_RDA)	# Scaling 2 (default)

# # Canonical coefficients from the rda object
# coef(eck_RDA)
# # env components displays as NA ???
# 
# # Unadjusted R^2 retrieved from the rda object
# (R2 <- RsquareAdj(eck_RDA)$r.squared)
# # Adjusted R^2 retrieved from the rda object
# (R2adj <- RsquareAdj(eck_RDA)$adj.r.squared) #report this value 

## Triplots of the rda results (wa scores)
## Site scores as weighted averages (vegan's default)
# Scaling 1: distance triplot
dev.new(title="RDA scaling 1 + wa")
plot(eck_RDA, scaling=1, 
     main="Triplot RDA morph.hel ~ try.env - scaling 1 - wa scores")
spe.sc1 <- scores(eck_RDA, choices=1:2, scaling=1, display="sp")
arrows(0, 0, spe.sc1[, 1]*0.92, spe.sc1[, 2]*0.92, length=0, lty=1, col="red")

# Scaling 2 (default): correlation triplot
dev.new(title="RDA scaling 2 + wa")
plot(eck_RDA, main="Triplot RDA morph.hel ~ try.env - scaling 2 - wa scores")
spe.sc2 <- scores(eck_RDA, choices=1:2, display="sp")
arrows(0, 0, spe.sc2[, 1]*0.92, spe.sc2[, 2]*0.92, length=0, lty=1, col="red")

## Triplots of the rda results (lc scores)
## Site scores as linear combinations of the environmental variables
# Scaling 1
dev.new(title="RDA scaling 1 + lc")
plot(eck_RDA, scaling=1, display=c("sp", "lc", "cn"), 
     main="Triplot RDA morph.hel ~ try.env - scaling 1 - lc scores")
arrows(0, 0, spe.sc1[, 1]*0.92, spe.sc1[, 2]*0.92, length=0, lty=1, col="red")

# Scaling 2
dev.new(title="RDA scaling 2 + lc")
plot(eck_RDA, display=c("sp", "lc", "cn"), 
     main="Triplot RDA morph.hel ~ try.env - scaling 2 - lc scores")
arrows(0, 0, spe.sc2[,1]*0.92, spe.sc2[,2]*0.92, length=0, lty=1, col="red")

# 
# # testing hypothesis
# #  H0:the env is not having an effect
# # H1: the env is having an effect
# 
# # Global test of the RDA result
# anova(eck_RDA, permutations=how(nperm=999))
# # no p-value
# 
# # which axes are NB 
# # Tests of all canonical axes
# anova(eck_RDA, by="axis", permutations=how(nperm=999))
# 
# #which env variables are responsible in influencing the landscape of spp? 
# anova(eck_RDA, by="term", permutations=how(nperm=999))
# 
# 
# # Variance inflation factors (VIF) (extracts colinearity)
# vif.cca(eck_RDA)
# # again env components not showing values 
# # exclude certain variables then run again 
# # no exact answer, dependent on ur argument 
# 
# # Apply Kaiser-Guttman criterion to residual axes
# eck_RDA$CA$eig[eck_RDA$CA$eig > mean(eck_RDA$CA$eig)]







