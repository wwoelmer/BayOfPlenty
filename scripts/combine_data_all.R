# combine multiple data sources together
library(tidyverse)
library(plotly)
library(zoo)
library(readxl)

##########################
## LAWA water quality 
wq <- read.csv('./data/processed_data/BoP_wq_2007_2021.csv')
wq$date <- as.Date(wq$date)
wq <- wq %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  select(-depth_m)
wq$category <- NA

###########################
# ctd data
ctd <- read.csv('./data/processed_data/BoP_ctd_2003_2022.csv')
ctd$date <- as.Date(ctd$date)
ctd <- ctd %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(depth_m %in% c(1, 8)) %>% # keep surface and bottom data
  distinct(lake, year, month, depth_m, site, .keep_all = TRUE) 

ggplot(ctd, aes(x = date, y = DO_gm3, color = as.factor(depth_m))) +
  geom_line() +
  facet_wrap(~lake)

ggplot(ctd, aes(x = date, y = chla_ugL, color = as.factor(depth_m))) +
  geom_line() +
  facet_wrap(~lake, scales = 'free')


ctd_wide <- ctd %>% 
  pivot_wider(names_from = depth_m, values_from = chla_ugL:temp_C)

ggplot(ctd_wide, aes(x = chla_ugL_1, y = chla_ugL_8, color = lake)) +
  geom_point() 

df <- left_join(wq, ctd_wide, by = c('lake', 'year', 'month', 'site'))
df <- df %>% 
  rename(date = date.x) %>% 
  select(lake, site, laketype, date, year, month, chla_ugL_INT, NH4_mgL, TN_mgm3, TP_mgm3,
         pH, secchi_m, PAR_umolm2s_1, turbidity_ntu_1, turbidity_ntu_8, DO_sat_1, DO_sat_8,
         SpC_uScm_1, SpC_uScm_8, temp_C_1, temp_C_8, chla_ugL_8)

###############################################################################################
# interpolate missing data
df <- df[order(df$date),]

df <- df %>% 
  group_by(lake, site) %>% 
  mutate(chla_ugL_8 = na.approx(chla_ugL_8, na.rm = FALSE, rule = 2, maxgap = 15),
         DO_sat_1 = na.approx(DO_sat_1, na.rm = FALSE, rule = 2, maxgap = 15),
         DO_sat_8 = na.approx(DO_sat_8, na.rm = FALSE, rule = 2, maxgap = 15),
         PAR_umolm2s_1 = na.approx(PAR_umolm2s_1, na.rm = FALSE, rule = 2, maxgap = 15),
         SpC_uScm_1 = na.approx(SpC_uScm_1, na.rm = FALSE, rule = 2, maxgap = 15),
         SpC_uScm_8 = na.approx(SpC_uScm_8, na.rm = FALSE, rule = 2, maxgap = 15),
         turbidity_ntu_1 = na.approx(turbidity_ntu_1, na.rm = FALSE, rule = 2, maxgap = 15),
         turbidity_ntu_8 = na.approx(turbidity_ntu_8, na.rm = FALSE, rule = 2, maxgap = 15),
         temp_C_1 = na.approx(temp_C_1, na.rm = FALSE, rule = 2, maxgap = 15),
         temp_C_8 = na.approx(temp_C_8, na.rm = FALSE, rule = 2, maxgap = 15))

###########################################################################################
######################################
### thermal mixing metrics
mix <- read.csv('./data/processed_data/BoP_mix_index_2004_2019.csv')
mix <- mix %>% 
  select(lake, site, date, everything()) %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  distinct(lake, year, month, site, .keep_all = TRUE)


df <- left_join(df, mix, by = c('lake', 'year', 'month', 'site'))
df <- df %>% 
  rename(date = date.x) %>% 
  select(-date.y)

#####################################
# lake geomorphology
gm <- read.csv('./data/processed_data/geomorphic_characteristics.csv', sep = ';')
gm <- gm %>% select(-trophic_state)

df <- left_join(df, gm, by = 'lake')
#####################################
### met data
## get met data for all lakes??

#####################################
## lake level
## only starts in 2021??

# possible to get lake level for other lakes? rotoma lake level????

######################################
## inflow data (discharge and loads)

# possible to get inflow data for other lakes?
######################################
## land cover data

# need lc data for other lakes
#################################################

## write the dataset as master file
write.csv(df, './data/master_all_lakes.csv', row.names = FALSE)
