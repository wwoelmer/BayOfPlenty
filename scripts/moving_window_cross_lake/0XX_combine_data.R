# combine all data into one
library(tidyverse)
library(zoo)


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
df3 <- df3 %>% 
  mutate(sum_alum = ifelse(is.na(sum_alum), 0, sum_alum))


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

df4 %>% 
  select(-note) %>% 
  pivot_longer(DRP_mgm3_bottom:water_level, names_to = 'variable', values_to = 'value') %>% 
  ggplot(aes(x = as.Date(date), y = value, color = lake)) +
  geom_point() +
  facet_wrap(~variable, scales = 'free')

#################################################################################
## add in mixing metrics
mix <- read.csv('./data/moving_window_analysis_cross_lake/BoP_mix_index_2003_2022.csv')
mix$date <- as.Date(mix$date)
mix <- mix %>% 
  select(-windspeed, -depth_m, -temp_C)

df5 <- left_join(df4, mix, by = c('date', 'lake'))
################################################################################
# remove NA's and interpolate?????
# interpolate missing data
df5 <- df5[order(df5$date),]

df5 <- df5 %>% 
  group_by(lake) %>% 
  mutate(DRP_mgm3_bottom = na.approx(DRP_mgm3_bottom, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(NH4_mgm3_bottom = na.approx(NH4_mgm3_bottom, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(NNN_mgm3_bottom = na.approx(NNN_mgm3_bottom, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(pH_bottom = na.approx(pH_bottom, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(pH_top = na.approx(pH_top, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(secchi_m = na.approx(secchi_m, na.rm = FALSE, rule = 2, maxgap = 15)) %>%   
  mutate(TN_mgm3_top = na.approx(TN_mgm3_top, na.rm = FALSE, rule = 2, maxgap = 15)) %>%   
  mutate(TP_mgm3_top = na.approx(TP_mgm3_top, na.rm = FALSE, rule = 2, maxgap = 15))   %>% 
  mutate(chla_mgm3_top = na.approx(chla_mgm3_top, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(DO_sat_bottom = na.approx(DO_sat_bottom, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(PAR_umolm2s_bottom = na.approx(PAR_umolm2s_bottom, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(spcond_uScm_bottom = na.approx(spcond_uScm_bottom, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(temp_C_bottom = na.approx(temp_C_bottom, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(DO_sat_top = na.approx(DO_sat_top, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(PAR_umolm2s_top = na.approx(PAR_umolm2s_top, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(spcond_uScm_top = na.approx(spcond_uScm_top, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(temp_C_top = na.approx(temp_C_top, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(schmidt_stability = na.approx(schmidt_stability, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(thermo_depth = na.approx(thermo_depth, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(epi_temp = na.approx(epi_temp, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(epi_dens = na.approx(epi_dens, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(hypo_temp = na.approx(hypo_temp, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(hypo_dens = na.approx(hypo_dens, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(meta_top = na.approx(meta_top, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(meta_bot = na.approx(meta_bot, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(uStar = na.approx(uStar, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(lake_num = na.approx(lake_num, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(strat = na.approx(strat, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(water_level = na.approx(water_level, na.rm = FALSE, rule = 2, maxgap = 15)) 
  
#################################################################################
### Calculate the TLI
# calculate monthly TLI
source('./scripts/R/tli_fx.R')

df6 <- df5 %>%
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(month, year, lake) %>%
  mutate(tli_monthly = tli_fx(chl = chla_mgm3_top, TN = TN_mgm3_top, TP = TP_mgm3_top, secchi = secchi_m)) %>% 
  group_by(year, lake) %>%
  mutate(tli_annual = tli_fx(chl = chla_mgm3_top, TN = TN_mgm3_top, TP = TP_mgm3_top, secchi = secchi_m))

hist(df6$tli_monthly)
tli <- ggplot(df6, aes(x = as.Date(date), y = tli_monthly)) +
  geom_point(linewidth = 1.2) +
  facet_wrap(~lake) +
  geom_line(aes(x = as.Date(date), y = tli_annual), size = 2) +
  theme_bw()
tli

df6 %>% 
  distinct(year, tli_annual, .keep_all = TRUE) %>% 
  ggplot(aes(x = as.Date(date), y = tli_annual)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lake) +
  ylim(2, 6) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('Date') +
  ylab('Annual TLI')

#################################################################################
write.csv(df6, './data/moving_window_analysis_cross_lake/all_lakes_TLI_drivers.csv', row.names = FALSE)
