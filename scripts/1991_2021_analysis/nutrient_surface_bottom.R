# calculate difference between surface and bottom nutrients back to 1991
library(tidyverse)
library(readxl)

#################################################################################
# bottom nutrients from Lake_Rotoehu
nuts <- read_excel('./data/raw_data/Rotoehu_1990_1999_PaulScholes.xlsx')
nuts$Date <- as.POSIXct(nuts$Date)
nuts <- nuts %>% 
  rename(date = Date,
         depth_m = DepthFrom) %>% 
  select(date, depth_m, Unit, Results) %>% 
  mutate(depth_m = round(depth_m))
table(nuts$depth_m)

nuts <- nuts %>% 
 # filter(depth_m %in% depths) %>% 
  mutate(location = ifelse(depth_m < 5, 'top', 'bottom'))
  
ggplot(nuts, aes(x = as.Date(date), y = Results, color = as.factor(depth_m))) +
  geom_point() +
  facet_wrap(Unit~location, scales ='free') +
  theme_bw()

nuts_f <- nuts %>% 
  distinct(date, location, Unit, .keep_all = TRUE)

nuts_wide <- nuts_f %>% 
  dplyr::select(-depth_m) %>% 
  pivot_wider(names_from = c('Unit', 'location'), values_from = 'Results') %>% 
  select(date, `DRP (mg/m3)_top`, `DRP (mg/m3)_bottom`, `NH4-N (mg/m3)_top`, `NH4-N (mg/m3)_bottom`) %>% 
  rename(DRP_mgm3_top = `DRP (mg/m3)_top`,
         DRP_mgm3_bottom = `DRP (mg/m3)_bottom`,
         NH4_mgm3_top = `NH4-N (mg/m3)_top`,
         NH4_mgm3_bottom = `NH4-N (mg/m3)_bottom`) %>% 
  mutate(month = month(date)) 



##############################################################################
## combine with 2001 and later data
dat <- read.csv("./data/processed_data/rotoehu_waterquality_2000_2021.csv")
dat$date <- as.Date(dat$date)

dat <- dat %>% 
  select(date, top_DRP_ugL, bottom_DRP_ugL, top_NH4_ugL,  bottom_NH4_ugL) %>% 
  rename(DRP_mgm3_top = top_DRP_ugL,
         DRP_mgm3_bottom = bottom_DRP_ugL,
         NH4_mgm3_top = top_NH4_ugL,
         NH4_mgm3_bottom = bottom_NH4_ugL)

nuts_all <- full_join(nuts_wide, dat, by = c('date', 'DRP_mgm3_top', 'DRP_mgm3_bottom', 'NH4_mgm3_top', 'NH4_mgm3_bottom'))

nuts_all <- nuts_all %>% 
  mutate(diff_DRP = DRP_mgm3_top - DRP_mgm3_bottom,
         diff_NH4 = NH4_mgm3_top - NH4_mgm3_bottom)
######
nuts_all %>% 
  pivot_longer(diff_DRP:diff_NH4, names_to = 'variable', values_to = 'value') %>% 
  ggplot(aes(x = as.Date(date), y = value, color = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = 'free_y') +
  theme_bw() +
  xlab('Date') +
  ylab('Top - Bottom Concentration (ug/L)') +
  geom_vline(aes(xintercept = as.Date('2000-01-01')))




