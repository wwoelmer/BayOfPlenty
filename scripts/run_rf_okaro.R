library(randomForest)
library(tidyverse)
library(ggpubr)
library(tidymodels)

# read in data
dat <- read.csv('./data/master_all_lakes.csv')

dat <- dat %>% 
  filter(lake=='Okaro') 

# reorganize data
dat <- dat %>% 
  select(chla_ugL_INT, everything(), -lake, -site, -laketype, -date, -volume_km2, -ratio_km2, -depth_m, -catchment_km2) 


# separate df for TN and secchi which have more NAs
# remove cols with lots of zeroes
dat <- dat %>% 
  select(-c(lake_num, epi_temp, epi_dens, hypo_temp, hypo_dens, meta_top, meta_bot, uStar))

dat1 <- dat %>% 
  select(-secchi_m, -TN_mgm3)

dat1 <- na.omit(dat1)
dat2 <- na.omit(dat)


source('./scripts/R/randomforest_diagnostics.R')

set.seed(242)
rf_diagnostic(data = dat1, target = "chla_ugL_INT", ntree = 5000, lake_name = 'Okaro')
rf_diagnostic(data = dat2, target = "secchi_m", ntree = 5000, lake_name = 'Okaro')

ggplot(dat, aes(x = month, y = chla_ugL_INT)) +
  geom_point()
