# look at diffs between daily avg water level and monthly avg water level


library(tidyverse)
library(Hmisc)
library(ggthemes)
library(RColorBrewer)
library(plotly)
library(ggpomological)

df <- read.csv('./data/processed_data/90s_data/rotoehu_tli_drivers_1990_2021.csv')

ggplot(df, aes(x = as.Date(date), y = tli_monthly, color = as.factor(decade))) +
  geom_point() +geom_line()

# what months were sampled in the 90's compared to other decades
df <- df %>% 
  mutate(month = month(date),
         year = year(date))
df %>% 
  filter(decade!='2020') %>% 
  ggplot(aes(x = as.factor(month), color = as.factor(decade), fill = as.factor(decade))) +
  geom_bar(position = 'dodge')

# how variable are water levels across months
ggplotly(df %>% 
           filter(decade!='2020') %>% 
           ggplot(aes(x = as.factor(month), y = avg_level_m, color = as.factor(year))) +
           geom_point())

df %>% 
  ggplot(aes(y = avg_level_m, x = as.factor(month), color = as.factor(year), group = as.factor(year))) +
  geom_line()

df %>% 
  ggplot(aes(y = monthly_avg_level_m, x = as.factor(month), color = as.factor(year), group = as.factor(year))) +
  geom_line()

ggplot(df, aes(x = monthly_avg_level_m, y = avg_level_m, color = as.factor(decade))) +
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0))

# is water level lower in the 90's because of the months in which it was sampled?
m_90 <- unique(df$month[df$decade==90])
df %>% 
  filter(month %in% m_90,
         decade!='2020') %>% 
  ggplot(aes(x = monthly_avg_level_m, y = avg_level_m, color = as.factor(month), shape = as.factor(decade))) +
  geom_point(size = 3) +
  geom_abline(aes(slope = 1, intercept = 0))


df %>% 
  filter(month %in% m_90,
         decade!='2020') %>% 
  ggplot(aes(x = monthly_avg_level_m, y = avg_level_m, color = as.factor(month), shape = as.factor(decade))) +
  geom_point(size = 3) +
  facet_wrap(~decade) +
  geom_abline(aes(slope = 1, intercept = 0)) +
  ggtitle('Monthly average water level vs daily avg water level on day of sampling, subset to months sampled in 90s')

df %>% 
  filter(month %in% m_90,
         decade!='2020') %>% 
  ggplot(aes(x = as.Date(date), y = avg_level_m, color = as.factor(month), shape = as.factor(decade))) +
  geom_point(size = 3) 

df %>% 
  filter(month %in% m_90,
         decade!='2020') %>% 
  ggplot(aes(x = as.Date(date), y = monthly_avg_level_m, shape = as.factor(decade))) +
  geom_point(size = 3) +
  ylim(293, 296)
