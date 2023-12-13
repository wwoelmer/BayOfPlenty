# run random forest for multiple variables
library(randomForest)
library(tidyverse)
library(ggpubr)
library(tidymodels)

# read in data
dat <- read.csv('./data/master_rotoehu.csv')

# reorganize data
dat <- dat %>% 
  select(chla_ugL_INT, everything(), -lake, -site, -date, -method, -top_turbidity_NTU, -bottom_turbidity_NTU, -turbidity_ntu_1) 

# remove bottom nutrients?
dat <- dat %>% 
  select(-bot)

# keep only land cover percentages, not area
dat <- dat %>% 
  select(-c("area_ha_aq_vegetation", "area_ha_decid_hardwood", "area_ha_exotic_forest",
            "area_ha_forest_harvested", "area_ha_gorse", "area_ha_hp_exotic_grassland" ,
            "area_ha_manuka",  "area_ha_native_forest", "area_ha_native_hardwood",
            "area_ha_settlement", "area_ha_water"))

# separate df for TN and secchi which have more NAs
# remove cols with lots of zeroes
dat <- dat %>% 
  select(-c(lake_num, epi_temp, epi_dens, hypo_temp, hypo_dens, meta_top, meta_bot, uStar))

colnames(dat)
table(is.na(dat$TN_TP))

dat1 <- na.omit(dat)
dat1 <- dat1[!is.infinite(rowSums(dat1)),]
source('./scripts/R/randomforest_diagnostics.R')

# example
#data = dat1
#target = "chla_ugL_INT"
#lake_name = paste0('Rotoehu', Sys.Date())
#train = .75
#mtry = ncol(data)/3
#ntree = 5000
set.seed(22)

dat_chl <- dat1 %>%  select(-secchi_m, -bottom_TN_ugL)
rf_diagnostic(data = dat_chl, target = "chla_ugL_INT", ntree = 5000, lake_name = paste0('Rotoehu', Sys.Date()))

dat_secchi <- dat1 
rf_diagnostic(data = dat_secchi, 
              target = "secchi_m", ntree = 5000, lake_name = paste0('Rotoehu', Sys.Date()))

dat_TN <- dat1 %>% 
  select(-bottom_TN_ugL, -bottom_NH4_ugL, - top_NH4_ugL)

rf_diagnostic(data = dat1, target = "top_TP_ugL", ntree = 5000, lake_name = paste0('Rotoehu', Sys.Date()))
rf_diagnostic(data = dat1, target = "bottom_NH4_ugL", ntree = 5000, lake_name = paste0('Rotoehu', Sys.Date()))
rf_diagnostic(data = dat_TN, 
              target = "top_TN_ugL", ntree = 5000, lake_name = paste0('Rotoehu', Sys.Date()))

########################
# read in again to run without TNTP
# read in data
dat <- read.csv('./data/master_rotoehu.csv')

# reorganize data
dat <- dat %>% 
  select(chla_ugL_INT, everything(), -lake, -site, -laketype, -date) 

# keep only land cover percentages, not area
dat <- dat %>% 
  select(-c("area_ha_aq_vegetation", "area_ha_decid_hardwood", "area_ha_exotic_forest",
            "area_ha_forest_harvested", "area_ha_gorse", "area_ha_hp_exotic_grassland" ,
            "area_ha_manuka",  "area_ha_native_forest", "area_ha_native_hardwood",
            "area_ha_settlement", "area_ha_water"))

# separate df for TN and secchi which have more NAs
# remove cols with lots of zeroes
dat <- dat %>% 
  select(-c(lake_num, epi_temp, epi_dens, hypo_temp, hypo_dens, meta_top, meta_bot, uStar))

dat1 <- dat %>% 
  select(-secchi_m, -TN_mgm3)

dat1 <- na.omit(dat1)
dat2 <- na.omit(dat)

rf_diagnostic(data = dat1, target = "TP_mgm3", ntree = 5000, lake_name = 'Rotoehu, with TNTP')
rf_diagnostic(data = dat1, target = "NH4_mgL", ntree = 5000, lake_name = 'Rotoehu, with TNTP')
rf_diagnostic(data = dat1, target = "chla_ugL_INT", ntree = 5000, lake_name = 'Rotoehu, with TNTP')
rf_diagnostic(data = dat2, target = "secchi_m", ntree = 5000, lake_name = 'Rotoehu, with TNTP')
rf_diagnostic(data = dat2, target = "TN_mgm3", ntree = 5000, lake_name = 'Rotoehu, with TNTP')

