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
dat <- read.csv('./data/moving_window_analysis_cross_lake/all_lakes_TLI_drivers.csv')

#######################################################
# run the ar model simulation
source('./scripts/R/run_ar.R')

# this set of variables comes from the decadal analysis (90s, 2000s, 2010s) plus land cover, alum, and 'none'
test_vars <- c( "DRP_mgm3_bottom", "NH4_mgm3_bottom", "NNN_mgm3_bottom", "pH_bottom",
                "pH_top", "DO_sat_bottom", "PAR_umolm2s_bottom", "spcond_uScm_bottom", 
                "temp_C_bottom", "DO_sat_top", "PAR_umolm2s_top", "spcond_uScm_top", 
                "temp_C_top", "air_temp", "rain", "air_pressure", "shortwave", 
                "longwave", "windspeed", "sum_alum", "water_level", "none")

lakes <- unique(dat$lake)
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

write.csv(out, './data/moving_window_analysis_cross_lake/model_output_all_lakes.csv', row.names = FALSE)

################################################################################
# look at R2 results
out %>% 
  filter(id_covar %notin% c('sum_alum', 'air_pressure', 'rain', 'windspeed', 'shortwave', 'longwave', 'air_temp')) %>%  # get rid of met variables for now
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

ggplotly(diff_r2)

diff_r2_panels <- ggplot(out_prop, aes(x = as.Date(start_date), y = as.factor(rank), color = as.factor(lake))) +
  geom_point() +
  facet_wrap(~id_covar) +
  theme_bw() +
  ylab('Rank') +
  xlab('Start of Iteration') +
  theme(text=element_text(size=18)) +
  labs(color = 'Lake')
ggplotly(diff_r2_panels)

##################################################################################################
# for manuscript but not report
#### look at difference from none model
# positive values indicate that model was better than the none model (better than autoregression alone)
ggplotly(ggplot(out_prop, aes(x = as.Date(start_date), y = diff_from_none, color = id_covar)) +
  geom_point(size = 2) +
  theme_bw() +
  facet_wrap(~lake) +
  ylab('Difference from "none" model') +
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
  filter(id_covar %notin% c('sum_alum', 'air_pressure', 'rain', 'windspeed', 'shortwave', 'longwave', 'air_temp')) # get rid of met variables for now

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
  mutate(sum = sum(pct*rank))

rank <- ggplot(out_rank, aes(x = reorder(id_covar, sum), y = pct, fill = fct_rev(as.factor(rank)))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = rank_pal) +
  facet_wrap(~lake) +
  theme_bw() +
  ylab('Percent of time') +
  xlab('Covariate') +
  labs(fill = 'Rank') +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.55)) 
rank

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
  filter(id_covar %notin% c('sum_alum', 'air_pressure', 'rain', 'windspeed', 'shortwave', 'longwave', 'air_temp')) %>% # get rid of met variables for nowg
  ggplot(aes(x = as.Date(start_date), y = value, color = lake)) +
  geom_point() +
  facet_wrap(~id_covar, scales = 'free_y') +
  theme_bw() +
  xlab('Start of Iteration') +
  ylab('Parameter Value') +
  labs(color = 'Covariate') +
  theme(text=element_text(size=15))

ggplotly(params)
ggsave('./figures/moving_window/parameter_time_series.png', params, dpi = 300, units = 'mm', 
       height = 300, width = 550, scale = 0.4)

## select a single driving covariate and compare across model parameters
out %>% 
  filter(id_covar=='air_temp_mean') %>% 
  ggplot(aes(x = as.Date(start_date), y = value, color = covar)) +
  geom_point() +
  facet_wrap(~covar, scales = 'free_y') +
  theme_bw()

out %>% 
  filter(id_covar=='avg_level_m') %>% 
  ggplot(aes(x = as.Date(start_date), y = value, color = covar)) +
  geom_point() +
  facet_wrap(~covar, scales = 'free_y') +
  theme_bw()

out %>% 
  filter(id_covar=='none') %>% 
  ggplot(aes(x = as.Date(start_date), y = value, color = covar)) +
  geom_point() +
  facet_wrap(~covar, scales = 'free_y') +
  theme_bw()

## look at p-values
out %>% 
  filter(id_covar=='none',
         p_value < 0.05) %>% 
  ggplot(aes(x = as.Date(start_date), y = p_value, color = covar)) +
  geom_point() +
  facet_wrap(~covar, scales = 'free_y') +
  theme_bw()

###############################################################################################
# run the simulation on the entire dataset, without subsetting to time periods

out_all_ts <- data.frame()

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
    # run the model
    d <- run_ar(data = dat_ar, id_var = id_var, id_covar = test_vars[i])
    d$iter_start <- start
    d$iter_end <- end
    d$start_date <- min(dat_sub$date)
    d$end_date <- max(dat_sub$date)
    d$n <- nrow(dat_sub)
    out_all_ts <- rbind(out_all_ts, d)
    
  
}


col_no_all <- length(unique(out_all_ts$id_covar))
col_pal_all <- colorRampPalette(brewer.pal(9, "Set1"))(col_no_all)

out_all_ts %>% 
  distinct(id_covar, .keep_all = TRUE) %>% 
ggplot(aes(x = id_covar, y = r2, color = as.factor(id_covar))) +
  geom_point(size = 4) +
  scale_color_manual(values = col_pal_all) +
  theme_bw() +
  ylab('R2') +
  labs(color = 'Covariate') +
  ylim(0.2, 0.7) +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  xlab('Covariate')


length(unique(dat$year))

############################################
# plot time series of driver variables with highlighted areas where that driver was of a high rank
source('./scripts/R/plot_date_range_rank.R')

# check a few out
plot_date_range_rank(variable = 'rain_mean', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Rain')
plot_date_range_rank(variable = 'rain_mean', rank_plot = 2, df_rank = out_prop, df_driver = dat, ylab = 'Rain')
plot_date_range_rank(variable = 'temp_C_8', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Bottom Water Temperature (C)')
plot_date_range_rank(variable = 'air_temp_mean', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Mean Air Temperature (C)')
plot_date_range_rank(variable = 'bottom_DRP_ugL', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Bottom Water DRP (ug/L)')
plot_date_range_rank(variable = 'TN_TP', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'TN:TP')
plot_date_range_rank(variable = 'thermo_depth', rank_plot = 4, df_rank = out_prop, df_driver = dat, ylab = 'thermocline depth')

dat <- dat %>% 
  mutate(diff_P = top_DRP_ugL - bottom_DRP_ugL,
         diff_NH4 = top_NH4_ugL - bottom_NH4_ugL,
         diff_NO3 = top_NO3_ugL - bottom_NO3_ugL) 
  
plot_date_range_rank(variable = 'bottom_DRP_ugL', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'diff btw surface and bottom DRP')

# some examples to save
exforest <- plot_date_range_rank(variable = 'area_pct_exotic_forest', rank_plot = 2, df_rank = out_prop, df_driver = dat, ylab = '% Exotic Forest in Watershed', title = FALSE, shading = FALSE)
lvl <- plot_date_range_rank(variable = 'avg_level_m', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Average Water Level (m)', title = FALSE, shading = FALSE)
drp <- plot_date_range_rank(variable = 'bottom_DRP_ugL', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Bottom Water DRP (ug/L)', title = FALSE, shading = FALSE)
btemp <- plot_date_range_rank(variable = 'temp_C_8', rank_plot = 1, df_rank = out_prop, df_driver = dat, ylab = 'Bottom Water Temperature (C)', title = FALSE, shading = FALSE)

(exforest + lvl)/ (drp + btemp)
drp
btemp

sel_vars <- c('area_pct_exotic_forest', 'avg_level_m')
ggplot(out_prop[out_prop$id_covar %in% sel_vars,], aes(x = as.Date(start_date), y = as.factor(rank), color = as.factor(id_covar))) +
  geom_point() +
  facet_wrap(~id_covar) +
  theme_bw() +
  ylab('Rank') +
  xlab('Start of Iteration') +
  theme(text=element_text(size=12)) +
  scale_color_manual(values = col_pal) +
  labs(color = 'Covariate')

