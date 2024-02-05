# do some stuff with thresholds between TLI and driver variables

library(tidyverse)

#### model output
out <- read.csv('./data/processed_data/moving_window/model_output.csv')
vars <- unique(out$id_covar)

coef <- out %>% 
  filter(covar %in% vars) %>% 
  select(covar:p_value, r2, start_date, end_date)

### observational data
data <- read.csv('./data/master_rotoehu.csv')

#calculate hydro year
data$hydroyear <- as.POSIXct(data$date) + (184*60*60*24)
data$hydroyear <- format(data$hydroyear,"%Y")
data$hydroyear <- as.numeric(data$hydroyear)
data$hydroyear_label <- paste(data$hydroyear-1, data$hydroyear, sep = "-")

source('./scripts/R/tli_fx.R')

# calculate monthly TLI and log transform non-normal data
data_tf <- data %>% 
  group_by(month, year) %>%
  mutate(tli_monthly = tli_fx(chl = chla_ugL_INT, TN = top_TN_ugL, TP = top_TP_ugL, secchi = secchi_m)) %>% 
  select(date, tli_monthly, year, month, vars[vars!='none']) %>% 
  mutate(log_DRP = log10(bottom_DRP_ugL),
         log_NH4 = log10(bottom_NH4_ugL),
         log_schmidt_stability = log10(schmidt_stability),
         log_alum = log10(sum_alum))

colnames(data_tf)

data_long <- data_tf %>% 
  pivot_longer(c(temp_C_8:monthly_avg_level_m, log_DRP:log_alum), names_to = 'variable', values_to = 'value')

ggplot(data_long, aes(x = value, fill = as.factor(variable))) +
  geom_histogram() +
  facet_wrap(~as.factor(variable), scales = 'free') 
  
ggplot(data_long, aes(x = value, y = tli_monthly, color = as.factor(variable))) +
  geom_point() +
  facet_wrap(~as.factor(variable), scales = 'free') +
  geom_smooth() 

ggplotly(ggplot(data_long, aes(x = value, y = tli_monthly)) +
           geom_point() +
           facet_wrap(~as.factor(variable), scales = 'free') +
           geom_smooth() +
           geom_point(aes(x = value, y = tli_monthly, color = as.factor(year(date))))
)


## check correlations among variables
cor_df <- data_tf %>% 
  ungroup() %>% 
  select(tli_monthly, bottom_DRP_ugL:sum_alum) 
cor_df <- na.omit(cor_df)

cor(cor_df)

colnames(data_tf)

#####################################################################################
# can we connect shifts in parameter values to changes in drivers?

# for each simulation time period, calculate the mean, min, max values of each driver and see if that relates to changes in parameter values or TLI
summ <- NULL
dates <- unique(coef$start_date)

for (i in 1:length(dates)){
  sub <- data_tf %>% 
    ungroup() %>% 
    filter(date >= dates[i] & date <= dates[i + 100]) %>%
    select(vars[vars!='none']) %>% 
    pivot_longer(everything(), names_to = 'variable', values_to = 'obs_value') %>% 
    group_by(variable) %>% 
    mutate(mean = mean(obs_value, na.rm = TRUE),
           min = min(obs_value, na.rm = TRUE),
           max = max(obs_value, na.rm = TRUE)) %>% 
    distinct(variable, .keep_all = TRUE) %>% 
   # select(-obs_value) %>% 
    mutate(start_date = coef$start_date[i],
           sim_no = i)
  
  summ <- rbind(summ, sub)
  
}

summ <- summ %>% 
  rename(covar = variable)

x <- left_join(coef, summ, by = c('start_date', 'covar'))

ggplot(x, aes(x = mean, y = value, color = (covar))) + 
  geom_point() +
  facet_wrap(~covar, scales = 'free') +
  geom_smooth() +
  geom_jitter(aes(x = obs_value, y = value))

ggplot(x, aes(x = mean, y = value)) + 
  geom_point() +
  facet_wrap(~covar, scales = 'free') +
  geom_smooth() +
  geom_point(aes(x = mean, y = value, color = as.factor(year(start_date))))

ggplot(x, aes(x = min, y = value, color = as.factor(covar))) + 
  geom_point() +
  facet_wrap(~covar, scales = 'free')

ggplot(x, aes(x = max, y = value, color = as.factor(covar))) + 
  geom_point() +
  facet_wrap(~covar, scales = 'free')
