# run model without the moving window (whole time series)

library(tidyverse)
library(plotly)
library(RColorBrewer)
library(patchwork)
library(ggpubr)
library(statcomp)
'%notin%' <- Negate('%in%')

# read in data
#dat <- read.csv('./data/processed_data/BoP_wq_2007_2021.csv')
dat <- read.csv('./data/moving_window_analysis_cross_lake/all_lakes_TLI_normalized_drivers.csv')

# subset to when the CTD data starts
dat <- dat %>% 
  filter(date > as.Date('2003-03-17'))

#######################################################
# run the ar model simulation
source('./scripts/R/run_ar.R')

# this set of variables comes from the decadal analysis (90s, 2000s, 2010s) plus land cover, alum, and 'none'
test_vars <-  c( "DRP_mgm3_bottom", "NH4_mgm3_bottom", "NNN_mgm3_bottom", 
                 "DO_sat_bottom", "PAR_umolm2s_top",
                 #"temp_C_top", 
                 "temp_C_bottom",
                 "schmidt_stability", "epi_temp",
                 "none") # "lake_num"

lakes <- unique(dat$lake)
#lakes <- 'Rotorua'
id_var <- "tli_monthly"

obs_length <- dat %>% 
  count(lake)

out <- data.frame()

for(i in 1:length(test_vars)){
  for(k in 1:length(lakes)){
    # subset to driver variable
    if(test_vars[i]=='none'){
      dat_ar <- dat %>% 
        ungroup() %>% 
        select(date, lake, id_var)  
    }else{
      dat_ar <- dat %>% 
        ungroup() %>% 
        select(date, lake, id_var, test_vars[i])  
    }
    print(test_vars[i])
    # subset to the right lake 
    
    dat_ar <- dat_ar %>% 
      dplyr::filter(lake==lakes[k])
    print(lakes[k])
    
      
      # run the model
      d <- run_ar(data = dat_ar, 
                  id_var = id_var, 
                  id_covar = test_vars[i], 
                  window_length = window_length)
      d$start_date <- min(dat_ar$date)
      d$end_date <- max(dat_ar$date)
      d$n <- nrow(dat_ar)
      d$lake <- lakes[k]
      out <- rbind(out, d)
      print(paste0(test_vars[i], " ", lakes[k]))
    }  
  }
  


col_no_all <- length(unique(out$id_covar))
col_pal_all <- colorRampPalette(brewer.pal(9, "Set1"))(col_no_all)

out %>% 
  distinct(id_covar, lake, .keep_all = TRUE) %>% 
  ggplot(aes(x = id_covar, y = r2, color = as.factor(id_covar))) +
  geom_point(size = 4) +
  scale_color_manual(values = col_pal_all) +
  facet_wrap(~lake) +
  theme_bw() +
  ylab('R2') +
  labs(color = 'Covariate') +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  xlab('Covariate')


out %>% 
  distinct(id_covar, lake, .keep_all = TRUE) %>% 
  group_by(lake) %>% 
  mutate(r2_none = r2[id_covar=='none'],
         diff_from_none = r2 - r2_none) %>% 
  ggplot(aes(x = id_covar, y = diff_from_none, color = as.factor(id_covar))) +
  geom_point(size = 4) +
  scale_color_manual(values = col_pal_all) +
  facet_wrap(~lake) +
  theme_bw() +
  ylab('Difference from AR only (R2)') +
  labs(color = 'Covariate') +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  xlab('Covariate')


params <- out %>% 
  filter(covar %in% test_vars) %>% 
  mutate(mixing_state = ifelse(lake %in% c('Rotorua', 'Rotoehu', 'Rerewhakaaitu'), 'polymictic', 'monomictic'))


params$id_covar <- factor(params$id_covar, levels = 
                            c('epi_temp', 'Kd', 'PAR_umolm2s_top',
                              'NH4_mgm3_bottom', 'DRP_mgm3_bottom',
                              'NNN_mgm3_bottom', 'temp_C_bottom', 'DO_sat_bottom',
                              'schmidt_stability'))

# set order of lake factor
params$lake <- factor(params$lake, levels = c('Rotorua', 
                                              'Rotoehu', 
                                              'Rerewhakaaitu',
                                              'Okaro', 
                                              'Okareka',
                                              'Tarawera'))

ggplot(params, aes(x = lake, y = value, color = lake, fill = lake)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0) +
  #facet_grid(id_covar~mixing_state, scales = 'free') +
  facet_wrap(~id_covar, scales = 'free_y') +
  theme_bw() +
  scale_color_manual(values = c( '#B22222', '#D05A22', '#ED9121', '#C9F2C7', 'seagreen','royalblue')) +
  xlab('Start of Iteration') +
  ylab('Parameter Value') +
  theme(text=element_text(size=15),
        axis.text.x = element_text(angle = 65, vjust = 0.50))
