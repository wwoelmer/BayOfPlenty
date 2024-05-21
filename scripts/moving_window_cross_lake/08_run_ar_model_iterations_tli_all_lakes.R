# run simple AR model for TLI + one covariate from set of potential driver variables


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
test_vars <-  c( "DRP_mgm3_bottom", "NH4_mgm3_bottom", "NNN_mgm3_bottom", "pH_bottom",
                 "pH_top", "DO_sat_bottom", "PAR_umolm2s_bottom", "spcond_uScm_bottom", 
                 "DO_sat_top", "PAR_umolm2s_top", "spcond_uScm_top", 
                 #"temp_C_top", 
                 "temp_C_bottom",
                 "air_temp_mean", "rain_sum", "windspeed_mean",
                 #"air_temp_max", "air_temp_min",
                 #"windspeed_min",  "windspeed_max", #"sum_alum", 
                 "water_level", "thermo_depth",
                 "schmidt_stability", "epi_temp", "hypo_temp",
                 "meta_top", "meta_bot", "none") # "lake_num"

lakes <- unique(dat$lake)
#lakes <- 'Rotorua'
id_var <- "tli_monthly"
window_length <- 100

obs_length <- dat %>% 
  count(lake)

n_iter <- seq(1, min(obs_length$n) - window_length)  

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
    
    for(j in 1:length(n_iter)){
      print(n_iter[j])
      # subset to the 100 observations in the iteration
      start <- j
      end <- j + window_length
      dat_sub <- dat_ar[start:end,]
      dat_sub <- na.omit(dat_sub)
      opd <-  weighted_ordinal_pattern_distribution(x = dat_sub$tli_monthly, ndemb = 4)
      pe <- permutation_entropy(opd) 
      print('before ar fx')
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
      d$pe <- pe
      d$lake <- lakes[k]
      out <- rbind(out, d)
      print(paste0(test_vars[i], " ", lakes[k], " ", n_iter[j]))
    }  
  }

}

#write.csv(out, './data/moving_window_analysis_cross_lake/model_output_all_lakes.csv', row.names = FALSE)

################################################################################
# assign an order to the lakes factor
out$lake <- factor(out$lake, levels = c('Rotorua', 'Rotoehu', 'Okaro', 'Tarawera'))

# and get rid of some variables we won't analyze
out <- out %>% 
  filter(id_covar %notin% c('meta_bot', 'meta_top', 'hypo_temp'))

################################################################################
# look at PE results
ggplotly(out %>% 
  filter(id_covar!='meta_bot',
         id_covar!='meta_top',
         id_covar!='hypo_temp') %>% 
ggplot(aes(x = pe, y = r2)) +
  geom_point() +
  geom_smooth(method = 'lm') )

################################################################################
# look at R2 results
out %>% 
  filter(id_covar %notin% c('sum_alum', 'air_pressure', 'rain', 
                            'windspeed', 'shortwave', 'longwave', 'air_temp',
                            'epi_dens', 'hypo_dens', 
                            'temp_C_top', 'meta_bot', 'meta_top', 'hypo_temp')) %>%  # get rid of met variables for now
ggplot(aes(x = as.Date(start_date), y = r2, color = id_covar)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~lake) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('Start date of iteration (+100 obs)') +
  ylab(bquote(~R^2)) +
  labs(color = 'Driver')

ggplotly(ggplot(out, aes(x = as.Date(start_date), y = r2, color = id_covar)) +
  geom_line() +
  geom_point(size = 2) +
  theme_bw() +
    facet_wrap(~lake) +
  theme(text=element_text(size=18)) +
  xlab('Start date of iteration (+100 obs)') +
  ylab('R2') +
  labs(color = 'Driver'))

################################################################################
# calculate the difference across variables
out_prop <- out %>% 
  filter(id_covar %notin% c('sum_alum', 'air_pressure', 'rain', 
                            'windspeed', 'shortwave', 'longwave', 'air_temp',
                            'epi_dens', 'hypo_dens', 'temp_C_bottom',
                            'temp_C_top', 'strat')) %>%  # get rid of met variables for now
  distinct(lake, id_covar, iter_start, .keep_all = TRUE) %>% 
  select(lake, id_covar:iter_end, start_date, end_date, r2) %>% 
  group_by(iter_start, lake) %>% 
  mutate(diff_from_best = max(r2) - r2,
         rank = dense_rank(desc(r2)),
         r2_none = r2[id_covar=='none'],
         diff_from_none = r2 - r2_none)

diff_r2 <- ggplot(out_prop, aes(x = as.Date(start_date), y = diff_from_best, color = id_covar)) +
  geom_point(size = 2) +
  theme_bw() +
  facet_wrap(~lake) +
  ylab('Difference from Best Performing Model') +
  xlab('Start of Iteration') +
  labs(color = 'Driver') +
  theme(text=element_text(size=18))
diff_r2
ggplotly(diff_r2)

##################################################################################################
# for manuscript but not report
#### look at difference from none model
# positive values indicate that model was better than the none model (better than autoregression alone)
ggplotly(ggplot(out_prop, aes(x = as.Date(start_date), y = diff_from_none, color = id_covar)) +
  geom_point(size = 2) +
  theme_bw() +
  facet_wrap(~lake) +
  ylab('Difference from AR only (none) model') +
  xlab('Start of Iteration') +
  labs(color = 'Covariate') +
  theme(text=element_text(size=18)))

ggplot(out_prop, aes(x = as.Date(start_date), y = diff_from_none, color = lake)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~id_covar) +
  geom_hline(yintercept = 0) +
  ylab('Difference from "none" model') +
  xlab('Start of Iteration') +
  labs(color = 'Lake') +
  theme(text=element_text(size=18))

################################################################################
# rank variables based on differences

out_prop <- out_prop %>% 
  select(lake, id_covar:rank) %>% 
  filter(id_covar %notin% c('sum_alum', 'air_pressure', 'rain', 
                            'windspeed', 'shortwave', 'longwave', 'air_temp',
                            'epi_dens', 'hypo_dens', 'temp_C_bottom',
                            'temp_C_top', 'strat'))   # get rid of met variables for now
  
out_rank <- plyr::ddply(out_prop, c("id_covar", "rank", "lake"), \(x) {
  n <- nrow(x)
  pct <- round(n/length(unique(out_prop$iter_start))*100)
  return(data.frame(pct = pct))
})

# define colors for the right number of ranks
## define color palettes for the right number of variables
num_ranks <- length(unique(out_rank$rank))
rank_pal <- colorRampPalette(brewer.pal(9, "YlGnBu"))(num_ranks)

out_rank <- out_rank %>% 
  group_by(rank, lake) %>% 
  arrange(pct) %>% 
  group_by(id_covar, lake) %>% 
  mutate(sum = sum(pct*rank)) %>% group_by(lake)

rank <- ggplot(out_rank, aes(y = reorder(id_covar, sum), x = pct, fill = fct_rev(as.factor(rank)))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = rank_pal) +
  facet_wrap(~lake, scales = 'free_y') +
  theme_bw() +
  ylab('Percent of time') +
  xlab('Covariate') +
  labs(fill = 'Rank') +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.55)) 
rank


####################################################
### now by category
weather <- c("air_temp_min", "windspeed_min", "air_temp_mean", "windspeed_mean",
             "air_temp_max", "windspeed_max", "rain_sum", "water_level")
water_quality_bottom <- c("pH_bottom", "DO_sat_bottom",  "PAR_umolm2s_bottom",
                          "spcond_uScm_bottom", "temp_C_bottom", "temp_C_bottom")
water_quality_top <- c( "pH_top", "DO_sat_top", 
                        "PAR_umolm2s_top", "spcond_uScm_top", "temp_C_top",
                        "DO_sat_top", "PAR_umolm2s_top", "spcond_uScm_top", "temp_C_top")
nutrients <- c("DRP_mgm3_bottom", "NH4_mgm3_bottom", "NNN_mgm3_bottom")
stratification <- c("thermo_depth", "schmidt_stability", "epi_temp", "epi_dens",
                    "hypo_temp", "hypo_dens", "meta_top", "meta_bot", "uStar",
                    "lake_num", "strat" )



out_rank <- out_rank %>% 
  mutate(category = ifelse(id_covar %in% weather, 'weather', 
                           ifelse(id_covar %in% water_quality_bottom, 'water_quality_bottom',
                                  ifelse(id_covar %in% water_quality_top, 'water_quality_top',
                                         ifelse(id_covar %in% stratification, 'stratification',
                                                ifelse(id_covar %in% nutrients, 'nutrients',
                                                       NA)))))) 

out_rank %>% 
  filter(category!='NA') %>% 
ggplot(aes(y = reorder(id_covar, sum), x = pct, fill = fct_rev(as.factor(rank)))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = rank_pal) +
  facet_grid(category~lake, scales = 'free_y') +
  theme_bw() +
  ylab('Percent of time') +
  xlab('Covariate') +
  labs(fill = 'Rank') +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.55)) 

####################################################################################
out_rank <- out_rank %>% 
  group_by(category, lake) %>% 
  mutate(n = length(unique(id_covar)),
         category_sum = sum(sum)/n)

ggplot(out_rank, aes(x = id_covar, y = -sum, fill = lake)) +
  geom_col(position = 'dodge') +
  facet_wrap(~category)

ggplot(out_rank, aes(x = category, y = category_sum, fill = lake)) +
  geom_col(position = 'dodge') 

out_rank %>% 
  filter(category!='NA') %>% 
  ggplot(aes(y = reorder(id_covar, sum), x = sum, fill = lake)) +
  geom_bar(stat = 'identity') +
  facet_grid(category~lake, scales = 'free_y') +
  theme_bw() +
  ylab('"Score" for each variable') +
  xlab('Covariate') +
  labs(fill = 'Rank') +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.55)) 

#######################
# do separately for each lake
rotoehu <- out_rank %>% 
  filter(lake=='Rotoehu',
         id_covar %in% vars_keep$vars[vars_keep$lake=='Rotoehu']) %>% 
  ggplot(aes(y = reorder(id_covar, sum), x = pct, fill = fct_rev(as.factor(rank)))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = rank_pal) +
  facet_wrap(~lake, scales = 'free_y') +
  theme_bw() +
  xlab('Percent of time') +
  ylab('Covariate') +
  labs(fill = 'Rank') +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.55)) 

rotorua <- out_rank %>% 
  filter(lake=='Rotorua',
         id_covar %in% vars_keep$vars[vars_keep$lake=='Rotorua']) %>% 
  ggplot(aes(y = reorder(id_covar, sum), x = pct, fill = fct_rev(as.factor(rank)))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = rank_pal) +
  facet_wrap(~lake, scales = 'free_y') +
  theme_bw() +
  xlab('Percent of time') +
  ylab('Covariate') +
  labs(fill = 'Rank') +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.55)) 

okaro <- out_rank %>% 
  filter(lake=='Okaro',
         id_covar %in% vars_keep$vars[vars_keep$lake=='Okaro']) %>% 
  ggplot(aes(y = reorder(id_covar, sum), x = pct, fill = fct_rev(as.factor(rank)))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = rank_pal) +
  facet_wrap(~lake, scales = 'free_y') +
  theme_bw() +
  xlab('Percent of time') +
  ylab('Covariate') +
  labs(fill = 'Rank') +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.55)) 


tarawera <- out_rank %>% 
  filter(lake=='Tarawera',
         id_covar %in% vars_keep$vars[vars_keep$lake=='Tarawera']) %>% 
  ggplot(aes(y = reorder(id_covar, sum), x = pct, fill = fct_rev(as.factor(rank)))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = rank_pal) +
  facet_wrap(~lake, scales = 'free_y') +
  theme_bw() +
  xlab('Percent of time') +
  ylab('Covariate') +
  labs(fill = 'Rank') +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.55)) 

ggarrange(rotoehu, rotorua, okaro, tarawera)

############
ggplot(out_rank, aes(x = lake, y = pct, fill = fct_rev(as.factor(rank)))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = rank_pal) +
  facet_wrap(~id_covar) +
  theme_bw() +
  ylab('Percent of time') +
  xlab('Covariate') +
  labs(fill = 'Rank') +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.55)) 

reorder(id_covar, sum)
################################################################################
# look at parameter values
params <- out %>% 
  filter(covar %in% test_vars) %>% 
  filter(id_covar %notin% c('sum_alum', 'air_pressure', 'rain', 'windspeed', 
                            'shortwave', 'longwave', 'air_temp', 'uStar', 'lake_num',
                            'hypo_dens', 'epi_dens',
                            'temp_C_bottom', 'temp_C_top')) %>% # get rid of met variables for nowg
  ggplot(aes(x = as.Date(start_date), y = value, color = lake)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  facet_wrap(~id_covar, scales = 'free_y') +
  theme_bw() +
  scale_color_manual(values = c('#B22222', '#ED9121', 'seagreen','royalblue')) +
  xlab('Start of Iteration') +
  ylab('Parameter Value') +
  labs(color = 'Covariate') +
  theme(text=element_text(size=15))

params

out %>% 
  filter(covar %in% test_vars) %>% 
  filter(id_covar %notin% c('sum_alum', 'air_pressure', 'rain', 'windspeed', 
                            'shortwave', 'longwave', 'air_temp', 'uStar', 'lake_num',
                            'hypo_dens', 'epi_dens',
                            'temp_C_bottom', 'temp_C_top')) %>% # get rid of met variables for nowg
  ggplot(aes(x = lake, y = value, color = lake, fill = lake)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  facet_wrap(~id_covar, scales = 'free_y') +
  theme_bw() +
  scale_color_manual(values = c('#B22222', '#ED9121', 'seagreen','royalblue')) +
  scale_fill_manual(values = c('#B22222', '#ED9121', 'seagreen','royalblue')) +
  xlab('Start of Iteration') +
  ylab('Parameter Value') +
  theme(text=element_text(size=15))



################################################################################
## plot R2 on x and parameter value on y with color by window, facet by variable
out %>% 
  filter(covar %in% test_vars) %>% 
  filter(id_covar %notin% c('sum_alum', 'air_pressure', 'rain', 'windspeed', 
                            'shortwave', 'longwave', 'air_temp', 'uStar', 'lake_num',
                            'hypo_dens', 'epi_dens',
                            'temp_C_bottom', 'temp_C_top'),
         id_covar %in% vars_keep$vars) %>% # get rid of met variables for nowg
  ggplot(aes(x = r2, y = value, color = lake)) +
  scale_color_manual(values = c('#B22222', '#ED9121', 'seagreen','royalblue')) +
  geom_hline(yintercept = 0) +
  geom_point(size = 3) +
  facet_wrap(~covar, scales = 'free') +
  theme_bw()

out %>% 
  filter(covar %in% test_vars,
         lake=='Rotoehu') %>% 
  filter(id_covar %notin% c('sum_alum', 'air_pressure', 'rain', 'windspeed', 
                            'shortwave', 'longwave', 'air_temp', 'uStar', 'lake_num',
                            'hypo_dens', 'epi_dens',
                            'temp_C_bottom', 'temp_C_top'),
         id_covar %in% vars_keep$vars[vars_keep$lake=='Rotoehu']) %>% # get rid of met variables for nowg
  ggplot(aes(x = r2, y = value, color = iter_start, shape = lake)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  geom_line() +
  facet_wrap(~covar, scales = 'free') +
  theme_bw()

out %>% 
  filter(covar %in% test_vars,
         lake=='Rotorua') %>% 
  filter(id_covar %notin% c('sum_alum', 'air_pressure', 'rain', 'windspeed', 
                            'shortwave', 'longwave', 'air_temp', 'uStar', 'lake_num',
                            'hypo_dens', 'epi_dens',
                            'temp_C_bottom', 'temp_C_top')) %>% # get rid of met variables for nowg
  ggplot(aes(x = r2, y = value, color = iter_start, shape = lake)) +
  geom_point(size = 3) +
  geom_line() +
  facet_wrap(~covar, scales = 'free') +
  theme_bw()


