# run random forest for multiple variables
library(randomForest)
library(tidyverse)
library(ggpubr)
library(tidymodels)

# read in data
dat <- read.csv('./data/master_rotoehu.csv')

# calculate monthly TLI
tli_fx <- function(chl, TN, TP, secchi){
  tli_c <- 2.22+2.54*log10(chl) 
  tli_n <-  -3.61+3.01*log10(TN)
  tli_p <-  0.218+2.92*log10(TP) 
  tli_secchi <-  5.56+2.6*log10(1/secchi - 1/40) 
  
  tli_all <- (mean(tli_c, na.rm = TRUE) + mean(tli_n, na.rm = TRUE) + 
                mean(tli_p, na.rm = TRUE) + mean(tli_secchi, na.rm = TRUE))/4
  
  return(tli_all)
}


dat <- dat %>% 
  group_by(month, year, lake, site) %>%
  mutate(tli_monthly = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m)) %>% 
  group_by(year, lake, site) %>%
  mutate(tli_annual = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m))

ggplot(dat, aes(x = as.Date(date), y = tli_monthly, linetype = 'monthly')) +
  geom_point(aes(color = as.factor(month)), size = 3) +
  geom_line() +
  geom_line(aes(x = as.Date(date), y = tli_annual, linetype = 'annual')) +
  ylab('Monthly TLI') +
  xlab('Year') +
  labs(linetype = '') +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  guides(color = FALSE)

dat %>% 
  distinct(year, .keep_all = TRUE) %>% 
  filter(year > 2009) %>% 
ggplot(aes(x = as.Date(date), y = tli_annual)) +
  geom_line() +
  geom_point(size = 2) +
  geom_hline(yintercept = 3.9) +
  ylim(0, 6) 

ggplot(dat, aes(x = as.Date(date), y = DO_sat_8)) +
  geom_line()

ggplot(dat, aes(x = tli_monthly, y = DO_sat_8)) +
  geom_point() +
  geom_smooth()

ggplot(dat, aes(x = tli_monthly, y = air_temp_max)) +
  geom_point() +
  geom_smooth()

# calculate proportion of times that the monthly TLI exceeded annual for each month
mo <- dat %>% 
  group_by(month) %>% 
  mutate(exceed = ifelse(tli_monthly > tli_annual, 1, 0))

mo_out <- plyr::ddply(mo, "month", function(x){
  pct <- sum(x$exceed==1)/nrow(x)*100
  return(data.frame(pct = pct))
})
mo_out

ggplot(mo_out, aes(x = as.factor(month), y = pct, color = as.factor(month))) +
  geom_point(size = 4) +
  ylab('Percent of times exceeding annual TLI') +
  geom_hline(yintercept = 50) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(color = 'Month') +
  xlab('Month')
  
# categorize points according to season
summer <- c(1:5, 12)

dat <- dat %>% 
  mutate(season = ifelse(month %in% summer, 1, 0))


# reorganize data and remove the four components that go into the TLI
dat <- dat %>% 
  ungroup() %>% 
  select(tli_monthly, everything(), 
         -lake, -site, -date, -method, -bottom_turbidity_NTU, -turbidity_ntu_1, - top_turbidity_NTU,
         -tli_annual,
         -c(chla_ugL_INT, top_TN_ugL, top_TP_ugL, secchi_m)) 

# keep only land cover percentages, not area
dat <- dat %>% 
  select(-c("area_ha_aq_vegetation", "area_ha_decid_hardwood", "area_ha_exotic_forest",
            "area_ha_forest_harvested", "area_ha_gorse", "area_ha_hp_exotic_grassland" ,
            "area_ha_manuka",  "area_ha_native_forest", "area_ha_native_hardwood",
            "area_ha_settlement", "area_ha_water"),
         -"bottom_pH")

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

rf_diagnostic(data = dat1, target = "tli_monthly", ntree = 5000, lake_name = paste0('Rotoehu', Sys.Date()))

dat_nonuts <- dat1 %>% 
  select(-c(bottom_TN_ugL, bottom_TP_ugL, top_NH4_ugL, top_pH,
            top_DRP_ugL, bottom_DRP_ugL))
rf_diagnostic(data = dat_nonuts, target = "tli_monthly", ntree = 5000, lake_name = paste0('Rotoehu no nutrients ', Sys.Date()))

# seasonal but with bottom nutrients
dat_summer <- dat1 %>% 
  filter(season==1) %>% 
  select(-season)
rf_diagnostic(data = dat_summer, target = "tli_monthly", ntree = 5000, lake_name = paste0('Rotoehu Summer ', Sys.Date()))

dat_winter <- dat1 %>% 
  filter(season==0) %>% 
  select(-season)
rf_diagnostic(data = dat_winter, target = "tli_monthly", ntree = 5000, lake_name = paste0('Rotoehu Winter ', Sys.Date()))

# seasonal without bottom TN and TP
dat_summer <- dat_nonuts %>% 
  filter(season==1) %>% 
  select(-season)
rf_diagnostic(data = dat_summer, target = "tli_monthly", ntree = 5000, lake_name = paste0('Rotoehu Summer no nutrients', Sys.Date()))

dat_winter <- dat_nonuts %>% 
  filter(season==0) %>% 
  select(-season)
rf_diagnostic(data = dat_winter, target = "tli_monthly", ntree = 5000, lake_name = paste0('Rotoehu Winter no nutrients', Sys.Date()))
