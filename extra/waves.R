

library(tidyverse)
library(ggpubr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(stringr)
library(broom)
library(purrr)
library(circular)

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

# loading sites 
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


# convert to daily data 
wave_daily <- wave_data %>%
  mutate(date = as.Date(date)) %>%
  separate(date, c("year", "month", "day"), "-") %>% 
  group_by(site, num, depth, date) %>%
  summarise(dir_circ = mean.circular(circular(dir, units = "degrees")),
            dirw_circ = mean.circular(circular(dirw, units = "degrees")),
            spw_circ = mean.circular(circular(spw, units = "degrees")),
            hs_circ = mean.circular(circular(hs, units = "degrees")),
            tp_circ = mean.circular(circular(tp, units = "degrees"))) 




# Annual clims
wave_ann <- wave_daily %>% 
  mutate(date = lubridate::month(date, label = TRUE)) %>% 
  group_by(site, date) %>% 
  summarise_all(funs(mean = mean, sd = sd), na.rm = T) %>%
  ungroup() %>% 
  select(-date) %>% 
  group_by(site) %>% 
  summarise_all(funs(mean), na.rm = T) %>% 
  mutate(date = "Annual") %>% 
  select(site, date, everything())

# 
# wave_long <- wave_ann %>% 
#   gather(key = "variable", value = "value", -site, -date, na.rm = TRUE)  
# 
# # visualising data 
# ggplot(data = wave_long, aes(x = value , y = "", group = 1)) +
#   geom_point(aes(colour = site)) +
#   facet_wrap( ~ variable, scales = "free") +
#   theme_classic()
# 






# Creating a function -----------------------------------------------------
load_wave <- function(wave_file){
  # tester
  # wave_file <- "wave/Ballito_dir_wave.csv"
  #
  wave_data <- read_csv(wave_file) %>% 
    mutate(date = lubridate::ymd(date),
           site = sapply(strsplit(sapply(strsplit(as.character(wave_file), "/"), 
                                         "[[", 2), "_"), "[[", 1)) #%>% 
  # group_by(site, date) %>% 
  return(wave_data)
}

# tab- project folder and finds- tab in "
# test <- map_dfr(dir("wave/", full.names = TRUE)[1], load_wave)

dir_wave <- map_dfr(dir("wave/", full.names = TRUE, pattern = "dir"), load_wave)
wavedir_temp <- left_join(dir_wave, select(year_clean, date, site, temp)) %>% 
  filter(date >= as.Date("2005-01-01"))

per_wave <- map_dfr(dir("wave/", full.names = TRUE, pattern = "per"), load_wave)
waveper_temp <- left_join(per_wave, select(year_clean, date, site, temp)) %>% 
  filter(date >= as.Date("2005-01-01"))

swh_wave <- map_dfr(dir("wave/", full.names = TRUE, pattern = "swh"), load_wave)
waveswh_temp <- left_join(swh_wave, select(year_clean, date, site, temp)) %>% 
  filter(date >= as.Date("2005-01-01"))

wind_wave <- map_dfr(dir("wave/", full.names = TRUE, pattern = "wind"), load_wave)
wavewind_temp <- left_join(wind_wave, select(year_clean, date, site, temp)) %>% 
  filter(date >= as.Date("2005-01-01"))

final_wave <- left_join(wavedir_temp,waveper_temp,waveswh_temp,wavewind_temp, 
                        select(year_clean, date, site, temp))




#Seasons:Monthly
dirs_temps_monthly <- dir_wave %>% 
  mutate(date = lubridate::month(date, label = TRUE)) %>% 
  group_by(site, date) %>% 
  summarise(dir_min = min(dir, na.rm = TRUE),
            dir_max = max(dir, na.rm = TRUE),
            dir_range = range(dir, na.rm = TRUE)[2]-range(dir, na.rm = TRUE)[1],
            dir_median = median(dir, na.rm = TRUE), 
            dirw_min = min(dirw, na.rm = TRUE),
            dirw_max = max(dirw, na.rm = TRUE),
            dirw_range = range(dirw, na.rm = TRUE)[2]-range(dirw, na.rm = TRUE)[1],
            dirw_median = median(dirw, na.rm = TRUE)) %>%
  filter(date %in% c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) %>% 
  ungroup()

#Seasons:Annually

dirs_annual <- dir_wave %>% 
  mutate(date = lubridate::month(date, label = TRUE)) %>% 
  group_by(site, date) %>% 
  summarise(dir_min = min(dir, na.rm = T),
            dir_max = max(dir, na.rm = T),
            dir_range = range(dir, na.rm = T)[2]-range(dir, na.rm = T)[1],
            dir_median = median(dir, na.rm = T), 
            dirw_min = min(dirw, na.rm = T),
            dirw_max = max(dirw, na.rm = T),
            dirw_range = range(dirw, na.rm = T)[2]-range(dirw, na.rm = T)[1],
            dirw_median = median(dirw, na.rm = T)) %>% 
  ungroup() %>% 
  select(-date) %>% 
  group_by(site) %>% 
  summarise_all(funs(mean(., na.rm = T))) %>% 
  mutate(date = "Annually") %>% 
  select(site, date, everything())
