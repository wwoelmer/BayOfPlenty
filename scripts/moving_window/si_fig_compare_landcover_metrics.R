# run simple AR model for TLI + one covariate from each group
# with groups defined as climatic, (air temp, rainfall, windspeed)
#                       anthropogenic, (land use change, alum dosing)
#                       climatic*anthropogenic interaction, (discharge, nutrient load)
#                       internal variability (e.g., obs complexity, anoxic factor?)

library(tidyverse)
library(MuMIn)
library(tidymodels)
library(plotly)
library(RColorBrewer)
library(patchwork)


# read in data
#dat <- read.csv('./data/processed_data/BoP_wq_2007_2021.csv')
dat <- read.csv('./data/master_rotoehu.csv')

#calculate hydro year
dat$hydroyear <- as.POSIXct(dat$date) + (184*60*60*24)
dat$hydroyear <- format(dat$hydroyear,"%Y")
dat$hydroyear <- as.numeric(dat$hydroyear)
dat$hydroyear_label <- paste(dat$hydroyear-1, dat$hydroyear, sep = "-")


# calculate monthly TLI
source('./scripts/R/tli_fx.R')

dat <- dat %>% 
  group_by(month, year, lake, site) %>%
  mutate(tli_monthly = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m)) %>% 
  group_by(year, lake, site) %>%
  mutate(tli_annual = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m))

hist(dat$tli_monthly)
tli <- ggplot(dat, aes(x = as.Date(date), y = tli_monthly)) +
  geom_point(size = 1.2) +
  geom_line(aes(x = as.Date(date), y = tli_annual), size = 2) +
  theme_bw()
tli

dat %>% 
  distinct(year, tli_annual, .keep_all = TRUE) %>% 
  ggplot(aes(x = as.Date(date), y = tli_annual)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 3.9) +
  ylim(0, 6) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('Date') +
  ylab('Annual TLI')


# plot all land use vars to see which have changed
dat %>% 
  select(c("date", "area_pct_aq_vegetation", "area_pct_decid_hardwood","area_pct_exotic_forest",
           "area_pct_forest_harvested", "area_pct_gorse", "area_pct_hp_exotic_grassland",
           "area_pct_manuka", "area_pct_native_forest", "area_pct_native_hardwood",    
           "area_pct_settlement", "area_pct_water")) %>% 
  pivot_longer(area_pct_aq_vegetation:area_pct_water, names_to = 'variable', values_to = 'value') %>% 
  ggplot(aes(x = as.Date(date), y = value, color = variable, fill = variable)) +
  geom_area() +
  xlab('Date') +
  ylab('Percent of Catchment') +
  theme_bw()

#######################################################
# run the ar model simulation
source('./scripts/R/run_ar.R')

# select variables to test
test_vars <- c("air_temp_mean", "windspeed_max", "rain_mean","avg_level_m", 
               "temp_C_8", "thermo_depth", "sum_alum",  "area_pct_exotic_forest",
               "DO_sat_8", "bottom_DRP_ugL", "bottom_NH4_ugL", 
               "bottom_NO3_ugL", "none")

# this set of variables comes from the decadal analysis (90s, 2000s, 2010s)
test_vars <- c("air_temp_mean", "windspeed_min", 
               "avg_level_m", 
               "bottom_DRP_ugL", "bottom_NH4_ugL",
               "temp_C_8", #"de_trended_temp_anomaly", 
               "area_pct_aq_vegetation", "area_pct_decid_hardwood","area_pct_exotic_forest",
               "area_pct_forest_harvested", "area_pct_gorse", "area_pct_hp_exotic_grassland",
               "area_pct_manuka", "area_pct_native_forest", "area_pct_native_hardwood",    
               "area_pct_settlement", "area_pct_water",
               "sum_alum",
               "none")

id_var <- "tli_monthly"
window_length <- 100
n_iter <- seq(1, nrow(dat) - window_length)

out <- data.frame()

for(i in 1:length(test_vars)){
  if(test_vars[i]=='none'){
    dat_ar <- dat %>% 
      ungroup() %>% 
      select(date, id_var)  
  }else{
    dat_ar <- dat %>% 
      ungroup() %>% 
      select(date, id_var, test_vars[i])  
  }
  
  
  for(j in 1:length(n_iter)){
    
    # subset to the 100 observations in the iteration
    start <- j
    end <- j + window_length
    dat_sub <- dat_ar[start:end,]
    
    # run the model
    d <- run_ar(data = dat_sub, 
                id_var = id_var, 
                id_covar = test_vars[i], 
                window_length = window_length)
    d$iter_start <- start
    d$iter_end <- end
    d$start_date <- min(dat_sub$date)
    d$end_date <- max(dat_sub$date)
    d$n <- nrow(dat_sub)
    out <- rbind(out, d)
    
  }
}



## define color palettes for the right number of variables
col_no <- length(unique(out$id_covar))
col_pal <- colorRampPalette(brewer.pal(9, "Set1"))(col_no)



ggplotly(ggplot(out, aes(x = iter_start, y = r2, color = id_covar)) +
           geom_point() +
           scale_color_manual(values = col_pal) +
           geom_line() +
           theme_bw())

# look at just land cover vars
si_landuse <- out %>% 
  filter(id_covar %in% c(  "area_pct_aq_vegetation", "area_pct_decid_hardwood","area_pct_exotic_forest",
                           "area_pct_forest_harvested", "area_pct_gorse", "area_pct_hp_exotic_grassland",
                           "area_pct_manuka", "area_pct_native_forest", "area_pct_native_hardwood",    
                           "area_pct_settlement", "area_pct_water")) %>% 
  ggplot(aes(x = iter_start, y = r2, color = id_covar)) +
  geom_point() +
  scale_color_manual(values = col_pal) +
  geom_line() +
  theme_bw()

ggsave('./figures/moving_window/si_fig_landuse_vars_r2.png', si_landuse, dpi = 300, units = 'mm', height = 300, width = 500, scale = 0.4)