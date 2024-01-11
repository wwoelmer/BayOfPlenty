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
               "avg_level_m", "monthly_avg_level_m",
               "bottom_DRP_ugL", "bottom_NH4_ugL",
               "temp_C_8", #"de_trended_temp_anomaly", 
               "area_pct_hp_exotic_grassland",
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


###############################################################################
### compare aic and r2
ggplot(out, aes(x = iter_start, y = r2, color = id_covar)) +
  geom_line() +
  geom_point(size = 2) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('Start date of iteration (+100 obs)') +
  ylab('R2') +
  scale_color_manual(values = col_pal) +
  labs(color = 'Covariate')

aicc <- ggplot(out, aes(x = iter_start, y = aic, color = id_covar)) +
  geom_line() +
  geom_point(size = 2) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('Start date of iteration (+100 obs)') +
  ylab('AICc') +
  scale_color_manual(values = col_pal) +
  labs(color = 'Covariate')

aic_r2_compare <- ggplot(out, aes(x = aic, y = r2, color = id_covar)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('AICc') +
  ylab('R2') +
  scale_color_manual(values = col_pal) +
  labs(color = 'Covariate')

combined_fig <- ggarrange(aicc, aic_r2_compare, common.legend = TRUE, labels = 'AUTO')
ggsave('./figures/moving_window/aic_r2_comparison.png', combined_fig,
       dpi = 300, units = 'mm', height = 300, width = 700, scale = 0.5)

################################################################################
# look at R2 results
r2_results <- ggplot(out, aes(x = as.Date(start_date), y = r2, color = id_covar)) +
  geom_line() +
  geom_point(size = 2) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('Start date of iteration (+100 obs)') +
  ylab('R2') +
  scale_color_manual(values = col_pal) +
  labs(color = 'Covariate')
r2_results

ggsave('./figures/moving_window/r2_timeseries.png', r2_results,
       dpi = 300, units = 'mm', height = 300, width = 600, scale = 0.5)

tli + r2_results

################################################################################
# calculate the difference across variables
out_prop <- out %>% 
  distinct(id_covar, iter_start, .keep_all = TRUE) %>% 
  select(id_covar:iter_end, start_date, end_date, r2) %>% 
  group_by(iter_start) %>% 
  mutate(diff_from_best_r2 = max(r2) - r2,
         rank_r2 = dense_rank(desc(r2)),
         diff_from_best_aic = min(aic) - aic,
         rank_aic = dense_rank((aic)))

ggplotly(ggplot(out_prop, aes(x = iter_start, y = diff_from_best_r2, color = id_covar)) +
           geom_point() +
           scale_color_manual(values = col_pal) +
           theme_bw())


#### area exotic forest doesn't follow expectations for r2 and aic
## this article might be helpful: https://stats.stackexchange.com/questions/140965/when-aic-and-adjusted-r2-lead-to-different-conclusions

ggplot(out_prop, aes(x = as.Date(start_date), y = diff_from_best_r2, color = id_covar)) +
  geom_point() +
  scale_color_manual(values = col_pal) +
  theme_bw() +
  ylab('Difference from Best Performing Model') +
  xlab('Start of Iteration') +
  labs(color = 'Covariate') +
  theme(text=element_text(size=18))

ggplot(out_prop, aes(x = as.Date(start_date), y = diff_from_best_r2, color = id_covar)) +
  geom_point() +
  scale_color_manual(values = col_pal) +
  theme_bw() +
  facet_wrap(~id_covar) +
  geom_hline(yintercept = 0) +
  ylab('Difference from Best Performing Model') +
  xlab('Start of Iteration') +
  labs(color = 'Covariate') +
  theme(text=element_text(size=18))

ggplot(out_prop, aes(x = as.Date(start_date), y = as.factor(rank_r2), color = as.factor(id_covar))) +
  geom_point() +
  facet_wrap(~id_covar) +
  theme_bw() +
  ylab('Rank') +
  xlab('Start of Iteration') +
  theme(text=element_text(size=12)) +
  scale_color_manual(values = col_pal) +
  labs(color = 'Covariate')

ggplot(out_prop, aes(x = as.Date(start_date), y = as.factor(rank_aic), color = as.factor(id_covar))) +
  geom_point() +
  facet_wrap(~id_covar) +
  theme_bw() +
  ylab('Rank') +
  xlab('Start of Iteration') +
  theme(text=element_text(size=12)) +
  scale_color_manual(values = col_pal) +
  labs(color = 'Covariate')

################################################################################
# rank variables based on differences

out_rank <- plyr::ddply(out_prop, c("id_covar", "rank_r2", "rank_aic"), \(x) {
  print(unique(x$id_covar))
  #  print(unique(x$rank))
  n <- nrow(x)
  pct <- round(n/length(unique(out_prop$iter_start))*100)
  return(data.frame(pct = pct))
})


# define colors for the right number of ranks
## define color palettes for the right number of variables
num_ranks <- length(unique(out_rank$rank))
rank_pal <- colorRampPalette(brewer.pal(9, "YlGnBu"))(num_ranks)

out_rank <- out_rank %>% 
  group_by(rank) %>% 
  arrange(pct)

rank <- ggplot(out_rank, aes(x = reorder(id_covar, rank), y = pct, fill = fct_rev(as.factor(rank)))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = rank_pal) +
  theme_bw() +
  ylab('Percent of time') +
  xlab('Covariate') +
  labs(fill = 'Rank') +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) 
rank
ggsave('./figures/rank_barplot.png', rank, dpi = 300, units = 'mm', height = 400, width = 600, scale = 0.4)


################################################################################
# look at parameter values
ggplot(out, aes(x = as.Date(start_date), y = value, color = id_covar)) +
  geom_point() +
  scale_color_manual(values = col_pal) +
  facet_wrap(~covar, scales = 'free_y')

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
  filter(id_covar=='air_temp_mean',
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

