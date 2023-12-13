# run random forest for multiple variables
library(randomForest)
library(tidyverse)
library(ggpubr)
library(tidymodels)

# read in data
dat <- read.csv('./data/master_all_lakes.csv')

dat <- dat %>% 
  filter(lake=='Rotoiti',
         site==3) 

# reorganize data
dat <- dat %>% 
  select(chla_ugL_INT, everything(), -lake, -site, -laketype, -date) 


# separate df for TN and secchi which have more NAs
# remove cols with lots of zeroes
dat <- dat %>% 
  select(-c(lake_num, epi_temp, epi_dens, hypo_temp, hypo_dens, meta_top, meta_bot, uStar))

table(is.na(dat$TN_TP))

dat1 <- dat %>% 
  select(-secchi_m, -TN_mgm3)

dat1 <- na.omit(dat1)
dat2 <- na.omit(dat)


source('./scripts/R/randomforest_diagnostics.R')

set.seed(242)
rf_diagnostic(data = dat1, target = "chla_ugL_INT", ntree = 5000, lake_name = 'Rotiti')
rf_diagnostic(data = dat2, target = "secchi_m", ntree = 5000, lake_name = 'Rotoiti')

########################
# read in again to run without TNTP
# read in data
dat <- read.csv('./data/master.csv')

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

