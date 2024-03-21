# process water level data
library(readxl)
library(tidyverse)
library(lubridate)

###############################################################################
# rotoehu
rotoehu <- read_excel('./data/raw_data/EDS-686238-HL143688-Entire Record.xlsx', skip = 5)

rotoehu <- na.omit(rotoehu)
colnames(rotoehu) <- c('date', 'end', 'avg_level_m')

rotoehu <- rotoehu %>% 
  mutate(avg_level_m = as.numeric(avg_level_m),
         date = as.POSIXct(date),
         year = year(date),
         month = month(date)) %>% 
  select(-end) %>% 
  group_by(year, month) %>% 
  mutate(monthly_avg_level_m = mean(avg_level_m, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(-month)
# note: we don't want to summarize because we need to match up a single date in the
# WQ data with the monthly averages

rotoehu$lake <- 'Rotoehu'

###############################################################################
# rotorua
rotorua <- read.csv('./data/raw_data/rotorua_water_level.csv', skip = 5)

rotorua <- na.omit(rotorua)
colnames(rotorua) <- c('date', 'avg_level_m')

rotorua <- rotorua %>% 
  mutate(avg_level_m = as.numeric(avg_level_m),
         date = as.POSIXct(date),
         year = year(date),
         month = month(date)) %>% 
  group_by(year, month) %>% 
  mutate(monthly_avg_level_m = mean(avg_level_m, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(-month)

rotorua$lake <- 'Rotorua'

###############################################################################
# tarawera
tarawera <- read.csv('data/raw_data/tarawera_water_level.csv', skip = 5)

tarawera <- na.omit(tarawera)
colnames(tarawera) <- c('date', 'avg_level_m')

tarawera <- tarawera %>% 
  mutate(avg_level_m = as.numeric(avg_level_m),
         date = as.POSIXct(date),
         year = year(date),
         month = month(date)) %>% 
  group_by(year, month) %>% 
  mutate(monthly_avg_level_m = mean(avg_level_m, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(-month)

tarawera$lake <- 'Tarawera'
################################################################################
# water level for okaro?
# okareka has water level, that could be an alternative

################################################################################
# combine all data into one dataframe
lvl <- rbind(rotoehu, rotorua, tarawera)

lvl <- lvl %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  distinct(month, year, lake, .keep_all = TRUE) %>% 
  select(date, year, month, lake, monthly_avg_level_m) 
  
ggplot(lvl, aes(x = as.Date(date), y = monthly_avg_level_m, color = lake)) +
  geom_point() 

ggplot(lvl, aes(x = as.Date(date), y = monthly_avg_level_m, color = lake)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lake, scales = 'free')

write.csv(lvl, './data/moving_window_analysis_cross_lake/water_level_all_lakes.csv')
