# calculate mixing metrics

library(rLakeAnalyzer)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(plotly)
library(readxl)
library(patchwork)


ctd <- read.csv('./data/processed_data/BoP_ctd_2003_2022.csv')
ctd$date <- as.Date(ctd$date)

# remove any dups
ctd <- ctd %>% 
  distinct(lake, site, date, depth_m, .keep_all = TRUE) %>% 
  filter(!is.na(temp_C))


# read in bathy estimates
bty <- read_excel('./data/raw_data/Rotlakes_bathymetry.xls', skip = 1)
colnames(bty) <- c('lake', 'depth_m', 'vol_to_bottom_m3', 'vol_at_countour_m3', 'planar_sa_m2', 
                   'model_sd_m2')

bty$depth_m <- abs(bty$depth_m)

# area and depth of bathymetry
bthA <- bty$model_sd_m2
bthD <- bty$depth_m

# and wind
met_okaro <- read.csv('./data/moving_window_analysis_cross_lake/met_data/okaro_met_1980_2023.csv')
met_rotoehu <- read.csv('./data/moving_window_analysis_cross_lake/met_data/rotoehu_met_1980_2023.csv')
met_rotorua <- read.csv('./data/moving_window_analysis_cross_lake/met_data/rotorua_met_1980_2023.csv')
met_tarawera <- read.csv('./data/moving_window_analysis_cross_lake/met_data/tarawera_met_1980_2023.csv')

met <- rbind(met_okaro, met_rotoehu, met_rotorua, met_tarawera)
met$date <- as.Date(met$date)

# merge wind into ctd dataframe
ctd <- left_join(ctd, met, by = c('date', 'lake'))

# calculate a bunch of mixing metrics
t_metrics <- ctd %>% 
  select(lake, site, date, depth_m, temp_C, windspeed) %>% 
  group_by(lake, site, date) %>% 
  mutate(thermo_depth = thermo.depth(temp_C, depth_m, seasonal = TRUE),
         thermo_depth = ifelse(is.na(thermo_depth), 0, thermo_depth),
         schmidt_stability = schmidt.stability(temp_C, depth_m, bthA = bty$model_sd_m2, bthD = bty$depth_m),
         epi_temp = epi.temperature(wtr = temp_C, depths = depth_m, bthA = bthA, bthD = bthD),
         epi_dens = water.density(epi_temp),
         hypo_temp = hypo.temperature(wtr = temp_C, depths = depth_m, bthA = bthA, bthD = bthD),
         hypo_dens = water.density(hypo_temp),
         meta_top = meta.depths(temp_C, depth_m)[1],
         meta_bot = meta.depths(temp_C, depth_m)[2],
         uStar = uStar(wndSpeed = windspeed, wndHeight = 10, averageEpiDense = epi_dens),
         lake_num = lake.number(bthA = bthA, bthD = bthD, uStar = uStar, St = schmidt_stability,
                                metaT = meta_top, metaB = meta_bot, averageHypoDense = hypo_dens),
         strat = ifelse(thermo_depth > 0, 1, 0)) %>% 
  select(lake, site, date, everything()) %>% 
  distinct(lake, site, date, .keep_all = TRUE)


colnames(t_metrics)

ggplot(t_metrics, aes(x = as.Date(date), y = thermo_depth, color = lake)) +
  geom_line() +
  facet_wrap(~lake, scales = 'free')

write.csv(t_metrics, './data/moving_window_analysis_cross_lake/BoP_mix_index_2003_2022.csv', row.names = FALSE)

