
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(Hmisc)

'%notin%' <- Negate('%in%')

# read in data
#dat <- read.csv('./data/processed_data/BoP_wq_2007_2021.csv')
dat <- read.csv('./data/moving_window_analysis_cross_lake/all_lakes_TLI_drivers.csv')

# subset to when the CTD data starts
dat <- dat %>% 
  filter(date > as.Date('2003-03-17'))

lakes <- unique(dat$lake)

out <- NULL
for(i in 1:length(lakes)){
  cor_df <- dat %>% 
    filter(lake==lakes[i]) %>% 
    select(-date, -lake, -note, -tli_annual, -sum_alum, -month, -year, -strat,
           -secchi_m, -TN_mgm3_top, -TP_mgm3_top, -chla_mgm3_top,
           -temp_C_bottom, -temp_C_top, -epi_dens,-hypo_dens,
           -air_temp_min, -windspeed_min, -air_temp_mean,
           -windspeed_mean, -air_temp_max, -windspeed_max,
           -rain_sum, -water_level, -uStar, -lake_num) %>% 
    select(tli_monthly, everything())
  
  cor_df <- na.omit(cor_df)
  cor_out <- rcorr(as.matrix(cor_df)) # default is pearson
  try(corrplot::corrplot(cor_out$r, type = 'upper',
                         sig.level = 0.01, insig = 'blank', p.mat = cor_out$P,  
                         main = paste0('Lake: ', lakes[i])), silent = TRUE)
  
  vars <- cor_out$r[cor_out$P > 0.05]
  
  
  r <- as.data.frame(cor_out$r)[1,]
  p <- as.data.frame(cor_out$P)[1,]
  r_sub <- r
  #for(j in 2:ncol(r_sub)){
  #  if(p[,j]>=0.05){
  #    r_sub[,j] <- NA
  #  }
  #}
  
  r_sub <- as.data.frame(r_sub)
  r_sub$lake <- lakes[i]
  
  out <- rbind(out, r_sub)
  
}

out <- out %>% 
  select(lake, everything(), -tli_monthly)

out_long <- out %>% 
  pivot_longer(DRP_mgm3_bottom:Kd, names_to = 'variable', values_to = 'value') 

out_long <- na.omit(out_long)

out_long %>% 
  #filter(abs(value) > 0.3) %>% 
ggplot(aes(x = lake, y = value, fill = lake)) +
  geom_col(position = 'dodge') +
  facet_wrap(~variable) +
  geom_hline(yintercept = 0) +
  theme_bw()

ggplot(out_long, aes(y = variable, x = value, fill = lake)) +
  geom_col(position = 'dodge') +
  facet_wrap(~lake) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 0.3) +
  geom_vline(xintercept = -0.3) +
  theme_bw() 


weather <- c("air_temp_min", "windspeed_min", "air_temp_mean", "windspeed_mean",
             "air_temp_max", "windspeed_max", "rain_sum", "water_level")
water_quality_bottom <- c("DRP_mgm3_bottom", "NH4_mgm3_bottom", "NNN_mgm3_bottom",
                          "pH_bottom", "DO_sat_bottom",  "PAR_umolm2s_bottom",
                          "spcond_uScm_bottom", "temp_C_bottom", "temp_C_bottom")
water_quality_top <- c( "pH_top", "DO_sat_top", 
                   "PAR_umolm2s_top", "spcond_uScm_top", "temp_C_top",
                   "DO_sat_top", "PAR_umolm2s_top", "spcond_uScm_top", "temp_C_top")
stratification <- c("thermo_depth", "schmidt_stability", "epi_temp", "epi_dens",
                    "hypo_temp", "hypo_dens", "meta_top", "meta_bot", "uStar",
                    "lake_num", "strat" )


  
  out_long <- out_long %>% 
    mutate(category = ifelse(variable %in% weather, 'weather', 
                             ifelse(variable %in% water_quality_bottom, 'water_quality_bottom',
                                    ifelse(variable %in% water_quality_top, 'water_quality_top',
                                           ifelse(variable %in% stratification, 'stratification',
                                                  NA))))) 



ggplot(out_long, aes(y = variable, x = value, fill = category)) +
  geom_col(position = 'dodge') +
  facet_wrap(~lake) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 0.3) +
  geom_vline(xintercept = -0.3) +
  theme_bw() 

ggplot(out_long, aes(y = variable, x = value, fill = lake)) +
  geom_col(position = 'dodge') +
  facet_wrap(~category) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 0.3) +
  geom_vline(xintercept = -0.3) +
  theme_bw() 
