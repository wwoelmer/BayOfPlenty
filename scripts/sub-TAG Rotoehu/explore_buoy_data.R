# calculate thermocline depth, schmidt stability, anoxia in hypolimnion, etc from buoy data

library(tidyverse)
library(rLakeAnalyzer)

buoy <- read.csv('./data/processed_data/buoy_data/Rotoehu_202101-202409_profiles.csv')

buoy <- buoy %>% 
  mutate(date = as.Date(DateTime))

# read in bathy estimates
bty <- read_excel('./data/raw_data/Rotlakes_bathymetry.xls', skip = 1)
colnames(bty) <- c('lake', 'depth_m', 'vol_to_bottom_m3', 'vol_at_countour_m3', 'planar_sa_m2', 
                   'model_sd_m2')

bty$depth_m <- abs(bty$depth_m)
bty <- bty %>% 
  filter(lake=='Rotoehu')

# area and depth of bathymetry
bthA <- bty$model_sd_m2
bthD <- bty$depth_m

# take daily mean
buoy_daily <- buoy %>% 
  mutate(depth_rnd = floor(DptSns)) %>% 
  group_by(date, depth_rnd) %>% 
  summarise(temp_C = mean(TmpWtr,  na.rm = TRUE),
            DO_mgL = mean(DOconc, na.rm = TRUE),
            DO_sat = mean(DOpsat, na.rm = TRUE),
            chl_RFU = mean(FlChlr, na.rm = TRUE))
  
buoy_daily <- na.omit(buoy_daily)  

buoy_daily %>% 
  mutate(doy = yday(date),
         year = year(date)) %>% 
  rename(depth_m = depth_rnd) %>% 
  ggplot(aes(x = doy, y = DO_mgL, color = depth_m)) +
  geom_point() +
  facet_wrap(~year) +
  theme_bw()

buoy_daily %>% 
  mutate(doy = yday(date),
         year = year(date)) %>% 
  rename(depth_m = depth_rnd) %>% 
  ggplot(aes(x = doy, y = chl_RFU, color = depth_m)) +
  geom_point() +
  facet_wrap(~year) +
  theme_bw()
  
mix <- buoy_daily %>%   
  summarise(thermo_depth = thermo.depth(temp_C, depth_rnd, seasonal = TRUE),
            thermo_depth = ifelse(is.na(thermo_depth), 0, thermo_depth),
            schmidt_stability = schmidt.stability(temp_C, depth_rnd, bthA = bty$model_sd_m2, bthD = bty$depth_m))

colnames(mix) <- c("date", "thermo_depth", "schmidt_stability")

mix %>% 
  mutate(doy = yday(date),
         year = year(date)) %>% 
  ggplot(aes(x = doy, y = thermo_depth, color = as.factor(year))) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~year)

mix %>% 
  mutate(doy = yday(date),
         year = year(date)) %>% 
  ggplot(aes(x = doy, y = schmidt_stability, color = as.factor(year))) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~year)

ggplot(mix, aes(x = as.Date(date), y = schmidt_stability)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  xlab('Date')

mix <- mix %>% 
  mutate(year = year(date),
         month = month(date))

ggplot(mix, aes(x = month, y = schmidt_stability, fill =  as.factor(month))) +
  geom_boxplot() +
  theme_bw()

ggplot(mix, aes(x = as.factor(year), y = schmidt_stability, fill =  as.character(year))) +
  geom_boxplot() +
  theme_bw()

ggplot(mix, aes(x = as.factor(month), y = schmidt_stability, fill =  as.factor(year))) +
  geom_boxplot(position = position_dodge(0.8)) +
  theme_bw() +
  labs(fill = 'Year') +
  xlab('Month') +
  ylab('Schmidt stability')

ggplot(mix, aes(x = as.factor(month), y = schmidt_stability, fill =  as.factor(year))) +
  geom_boxplot(position = position_dodge(0.8)) +
  theme_bw() +
  facet_wrap(~month, scales = 'free') +
  labs(fill = 'Year') +
  xlab('Month') +
  ylab('Schmidt stability')


ggplot(mix, aes(x = as.factor(month), y = thermo_depth, fill =  as.factor(year))) +
  geom_boxplot(position = position_dodge(0.8)) +
  theme_bw() +
  facet_wrap(~month, scales = 'free') +
  labs(fill = 'Year') +
  xlab('Month') +
  ylab('Thermocline Depth')

do <- ggplot(buoy_daily, aes(x = as.Date(date), y = DO_mgL, color = depth_rnd)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(color = 'Depth') +
  xlab('Date') +
  ylab('DO (mg/L)')

chl <- ggplot(buoy_daily, aes(x = as.Date(date), y = chl_RFU, color = depth_rnd)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(color = 'Depth') +
  xlab('Date') +
  ylab('Chl (RFU)')

ggarrange(do, chl, ncol = 1)

buoy_daily %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  ggplot(aes(x = as.factor(month), y = chl_RFU, fill =  as.factor(year))) +
  geom_boxplot(position = position_dodge(0.8)) +
  theme_bw() +
  labs(fill = 'Year') +
  xlab('Month') 

######################################################################################################
# calculate volumetrically weighted DO


hypo_o2 <- buoy_daily %>% 
  select(date, depth_rnd, temp_C, DO_mgL) %>% 
  rename(depth_m = depth_rnd) %>% 
  distinct(date, depth_m, .keep_all = TRUE) %>% 
  group_by(date) %>% 
  mutate(hypo_depth = meta.depths(temp_C, depth_m, seasonal = FALSE, mixed.cutoff = 0)[2]) %>% 
  group_by(date) %>% 
  filter(depth_m > hypo_depth) %>% 
  left_join(bty_interp, by = 'depth_m') %>% 
  mutate(DO_vol = DO_mgL*vol_at_contour_m3)

hypo_VW <- hypo_o2 %>% 
  group_by(date) %>% 
  summarise(DO_sum = sum(DO_vol)) %>% 
  mutate(doy = yday(date),
         year = year(date))


ggplot(hypo_VW, aes(x = as.Date(date), y = DO_sum)) +
  geom_point() +
  geom_line() +
  geom_smooth()

ggplot(hypo_VW, aes(x = doy, y = DO_sum, color = as.factor(year))) +
  geom_point() +
  geom_line() +
  facet_wrap(~year)

ggplot(hypo_VW, aes(x = as.factor(year), y = DO_sum, fill = as.factor(year))) +
  geom_boxplot() +
  theme_bw() +
  ylab('Hypolimnetic DO') +
  xlab('Year') +
  theme(legend.position = 'none')
