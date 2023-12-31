library(tidyverse)
library(readxl)
library(rLakeAnalyzer)

ysi <- read_excel('./data/raw_data/Lake_Rotoehu_temp_DO_1990.xlsx', 
                  sheet = 'Profile')
ysi <- ysi %>% 
  select(-StationName, -LakeName) %>% 
  rename(date = Date,
         depth_m = 'Depth (m)',
         temp_C = 'Temperature (degC)',
         DO_psat = 'DO%',
         DO_mgl = 'DO (mg/l)')


ggplot(ysi, aes(x = as.Date(date), y = temp_C, color = as.factor(depth_m))) +
  geom_line() +
  geom_point()

ggplot(ysi, aes(x = as.Date(date), y = DO_mgl, color = as.factor(depth_m))) +
  geom_line() +
  geom_point()

# extract surface and bottom temps for analysis
temps <- ysi %>% 
  filter(depth_m==0|depth_m==8) %>% 
  select(date, depth_m, temp_C) %>% 
  distinct(date, depth_m, .keep_all = TRUE) %>% 
  pivot_wider(names_from = depth_m, values_from = temp_C, 
              names_prefix = 'temp_')

#################################################################
# calculate thermocline depth and schmidt stability

# read in bathy estimates
bty <- read_excel('./data/raw_data/Rotlakes_bathymetry.xls', skip = 1) 
colnames(bty) <- c('lake', 'depth_m', 'vol_to_bottom_m3', 'vol_at_countour_m3', 
                   'planar_sa_m2', 'model_sd_m2')

bty$depth_m <- abs(bty$depth_m)
bty <- bty %>% 
  filter(lake=='Rotoehu')


# area and depth of bathymetry
bthA <- bty$model_sd_m2
bthD <- bty$depth_m


t_metrics <- ysi %>% 
  select(date, depth_m, temp_C) %>% 
  distinct(date, depth_m, .keep_all = TRUE) %>% 
  group_by(date) %>% 
  mutate(thermo_depth = thermo.depth(temp_C, depth_m, seasonal = TRUE),
         thermo_depth = ifelse(is.na(thermo_depth), 0, thermo_depth),
         schmidt_stability = schmidt.stability(temp_C, 
                                               depth_m, 
                                               bthA = bty$model_sd_m2, 
                                               bthD = bty$depth_m),
         strat = ifelse(thermo_depth > 0, 1, 0)) %>% 
  select(date, everything()) %>% 
  distinct(date, .keep_all = TRUE)

ggplot(t_metrics, aes(x = as.Date(date), y = thermo_depth, 
                      color = as.factor(year(date)))) +
  geom_point() +
  geom_line()

ggplot(t_metrics, aes(x = as.Date(date), y = schmidt_stability, 
                      color = as.factor(year(date)))) +
  geom_point() +
  geom_line()

#######################################################################
# combine and write file
t_dat <- full_join(t_metrics, temps) 
t_dat <- t_dat %>% 
  select(date, thermo_depth, schmidt_stability, temp_0, temp_8)
write.csv(t_dat, './data/processed_data/90s_data/temp_metrics_rotoehu_90s.csv')
