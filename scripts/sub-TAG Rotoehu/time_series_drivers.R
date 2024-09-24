# time series of driver data
library(tidyverse)
library(ggpomological)

data <- read.csv('./data/master_rotoehu.csv')


select_vars <-  c("bottom_DRP_ugL", "bottom_NH4_ugL",
                  "temp_C_8", "air_temp_mean", "windspeed_min", 
                  "monthly_avg_level_m", 
                  "schmidt_stability", 
                  "sum_alum")

data_long <- data %>% 
  select(date, hydroyear, select_vars) %>% 
  pivot_longer(bottom_DRP_ugL:sum_alum, names_to = 'variable', values_to = 'value')


p1 <- ggplot(data_long, aes(x = as.Date(date), y = value, color = as.factor(variable))) +
  geom_point() +
  geom_line() +
  scale_color_pomological() +
  facet_wrap(~variable, ncol = 1, scales = 'free_y') +
  theme_bw() +
  labs(color = 'Decade') +
  xlab('Date') +
  ylab('value')
p1

data_long %>% 
  filter(date > as.Date('2018-01-01')) %>% 
ggplot(aes(x = as.Date(date), y = value, color = as.factor(variable))) +
  geom_point() +
  geom_line() +
  scale_color_pomological() +
  facet_wrap(~variable, ncol = 1, scales = 'free_y') +
  theme_bw() +
  theme(legend.position = "none") +
  xlab('Date') +
  ylab('value') +
  ggtitle('Since 2018')

