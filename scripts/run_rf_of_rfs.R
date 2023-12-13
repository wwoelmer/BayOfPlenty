
library(randomForest)
library(tidyverse)
library(ggpubr)
library(tidymodels)

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
  select(-lake_num)

dat1 <- dat %>% 
  select(-secchi_m, -TN_mgm3)

dat1 <- na.omit(dat1)
dat2 <- na.omit(dat)

source('./scripts/R/var_sel_regression_rf.R')

target = 'NH4_mgL'
data = dat2

all_vars <- var_sel_regression_rf(target = target, data = dat2)
all_vars_plot <- varsel_plot(all_vars)
all_vars_plot
all_vars_select <- unlist(all_vars$vars[19])
all_vars_select
