# combine all data into one
library(tidyverse)

lakes <- c('Okaro', 'Tarawera', 'Rotorua', 'Rotoehu')

################################################################################
# water quality monitoring data
wq <- read.csv('./data/moving_window_analysis_cross_lake/BoP_WQ_formatted.csv')

unique(paste0(wq$lake, wq$site))
# should we use rotorua site 2 or 5? go with 5?

wq <- wq %>% 
  filter(lake %in% lakes,
         site %in% c(1,3,5))


ggplot(wq, aes(x = as.Date(date), y = DRP_mgm3_bottom, color = site)) +
  geom_point() +
  facet_wrap(~lake, scales = 'free')

#select the variables I want to keep for the moving window analysis
wq <- wq %>% 
  select(date, lake, DRP_mgm3_bottom, NH4_mgm3_bottom, NNN_mgm3_bottom, 
         pH_bottom, pH_top,
         secchi_m, TN_mgm3_top, TP_mgm3_top, chla_mgm3_top) ## keep the TLI variables

ggplot(wq, aes(x = as.Date(date), y = pH_bottom - pH_top)) +
  geom_point() +
  facet_wrap(~lake, scales = 'free')

wq$date <- as.Date(wq$date)

################################################################################
# ctd data
ctd <- read.csv('./data/moving_window_analysis_cross_lake/BoP_ctd_2003_2022.csv')

ctd <- ctd %>% 
  filter(lake %in% lakes,
         site %in% c(1,3,5)) %>% 
  select(lake, date, depth_m, DO_sat, PAR_umolm2s, spcond_uScm, temp_C)

# calculate top and bottom values
ctd_long <- ctd %>% 
  pivot_longer(DO_sat:temp_C, names_to = 'variable', values_to = 'value') %>% 
  group_by(lake, date) %>% 
  mutate(max_depth = max(depth_m)) %>% 
  group_by(lake, date, variable) %>% 
  mutate(bottom = value[which.max(depth_m)],
         top = value[which.min(depth_m)]) %>% 
  distinct(lake, date, variable, .keep_all = TRUE)

ctd_tb <- ctd_long %>% 
  select(-depth_m, -value, -max_depth) %>% 
  pivot_wider(names_from = 'variable', values_from = c('bottom', 'top'),
              names_glue = "{variable}_{.value}")
ctd_tb$date <- as.Date(ctd_tb$date)

## combine ctd and wq
df <- left_join(wq, ctd_tb, by = c('lake', 'date'))

################################################################################
## add in met data #######

##### NOTE ########
# using only rotoehu met data as a filler for now, waiting to get ERA5 for all lakes
met <- read.csv('./data/processed_data/Rotoehu_met_1980_2022.csv')
met$date <- as.Date(met$date)

##### REMOVE LAKE NAME FOR NOW?
met <- met %>% select(-lake)

# combine met with other data
df2 <- left_join(df, met, by = 'date')

################################################################################
## add in alum data #######
alum <- read.csv('./data/processed_data/alum_rotoehu_okaro.csv')
alum$date <- as.Date(alum$date)

ggplot(alum, aes(x = as.Date(date), y = sum_alum)) +
  geom_point() +
  facet_wrap(~lake, scales = 'free')

# combine
df3 <- left_join(df2, alum, by = c('date', 'lake'))

ggplot(df3, aes(x = as.Date(date), y = sum_alum)) +
  geom_point() +
  facet_wrap(~lake, scales = 'free')

################################################################################
## add in alum data #######
lvl <- read.csv('./data/moving_window_analysis_cross_lake/water_level_all_lakes.csv')

# create new col which we will keep for water level which will be monthly average
# for all lakes, except okaro which is the observation on the day of sampling 
# (or closest to it)
lvl <- lvl %>% 
  mutate(water_level = ifelse(is.na(monthly_avg_level_m), daily_level_m, monthly_avg_level_m)) %>% 
  select(lake, year, month, water_level)

# combine with other data
df3 <- df3 %>% 
  mutate(year = year(date),
         month = month(date))

df4 <- left_join(df3, lvl, by = c('lake', 'year', 'month')) %>% 
  select(-month, -year)

################################################################################
# remove NA's and interpolate?????
df4 %>% 
  select(-note) %>% 
  pivot_longer(DRP_mgm3_bottom:water_level, names_to = 'variable', values_to = 'value') %>% 
  ggplot(aes(x = as.Date(date), y = value, color = lake)) +
  geom_point() +
  facet_wrap(~variable, scales = 'free')
