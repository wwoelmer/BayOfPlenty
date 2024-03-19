# time series of driver data
library(ggpomological)

# through 1990's 
data <- read.csv('./data/processed_data/90s_data/rotoehu_tli_drivers_1990_2021.csv')

ggplot(data, aes(x = rain_sum, y = monthly_avg_level_m)) +
  geom_point() 

ggplot(data, aes(x = air_temp_mean, y = monthly_avg_level_m)) +
  geom_point() 

ggplot(data, aes(x = windspeed_mean, y = monthly_avg_level_m)) +
  geom_point() 

ggplot(data, aes(x = soi_3mth_mean, y = monthly_avg_level_m)) +
  geom_point() +
  geom_smooth()

ggplot(data, aes(x = as.Date(date), y = soi_phase)) +
  geom_point()

# also add in the alum dosing data 
al <- read.csv('./data/processed_data/alum_dosing_rotoehu_2011_2022.csv')
al$date <- as.Date(al$date)
al <- al %>% 
  select(date, L_alum_day) 

# sum the amount of alum dosed since last sampling dates
dates <- unique(data$date)
al_sum <- data.frame()

for(i in 134:length(dates)){ # start at 134 before no alum was dosed before dates[134]
  sub <- al %>% 
    filter(date <= dates[i] & date > dates[i-1]) %>% 
    mutate(sum_alum = sum(L_alum_day), 
           sample_date = dates[i]) %>% 
    select(-L_alum_day)
  al_sum <- rbind(al_sum, sub)
  
}

al_sum <- al_sum %>% 
  distinct(sample_date, .keep_all = TRUE) %>% 
  select(-date) %>% 
  rename(date = sample_date)

ggplot(al_sum, aes(x = as.Date(date), y  = sum_alum)) +
  geom_point(size = 2)


data <- left_join(data, al_sum, by = 'date')

select_vars <-  c("DRP_mgm3", "NH4_mgm3", "temp_8",
                  "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                  "schmidt_stability", "sum_alum")

data_long <- data %>% 
  filter(decade!=2020) %>% 
  select(date, decade, hydroyear, select_vars) %>% 
  pivot_longer(DRP_mgm3:sum_alum, names_to = 'variable', values_to = 'value')

data_long$variable <- factor(data_long$variable, 
                             levels = c("DRP_mgm3", "NH4_mgm3", "temp_8",
                                        "air_temp_mean", "windspeed_min", "monthly_avg_level_m",
                                        "schmidt_stability", "de_trended_temp_anomaly", "sum_alum"),
                             labels = c("bottom DRP", "bottom NH4", "bottom water temp",
                                        "mean air temp", "min windspeed", "monthly water level", 
                                        "schmidt stability", "temp anomaly", "alum dosed"))

p1 <- ggplot(data_long, aes(x = as.Date(date), y = value, color = as.factor(decade))) +
  geom_point() +
  geom_line() +
  scale_color_pomological() +
  facet_wrap(~variable, ncol = 1, scales = 'free_y') +
  theme_bw() +
  labs(color = 'Decade') +
  xlab('Date') +
  ylab('value')
p1
ggplotly(p1)
ggsave('./figures/1991_2021_analysis/selected_vars_decade_timeseries.png', p1, dpi = 300, 
       units = 'mm', height = 800, width = 600, scale = 0.4)
