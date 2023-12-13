# calculate some anoxia metrics using buoy data
library(lubridate)
library(tidyverse)

buoy <- read.csv('./data/raw_data/Rotoehu_202101-202309_profiles.csv')

ggplot(buoy, aes(x = as.Date(DateTime), y = DOconc)) +
  geom_line()

# calculate average daily values
buoy <- buoy %>% 
  mutate(date = as.Date(DateTime),
         depth_round = round(DptSns)) 

ggplot(buoy, aes(x = as.Date(DateTime), y = DOconc)) +
  geom_line() +
  facet_wrap(~depth_round)
# use 8m as the bottom since there is better data coverage (fewer obs at 9m and deeper)

buoy_avg <- buoy %>% 
  #filter(depth_round %in% c(1, 7, 8, 9)) %>% 
  group_by(date, depth_round) %>% 
  summarise(across(everything(), list(mean), na.rm = TRUE))

ggplot(buoy_avg, aes(x = as.Date(date), y = DOconc_1)) +
  geom_line() +
  facet_wrap(~depth_round) +
  geom_hline(aes(yintercept = 2))

ggplot(buoy_avg, aes(x = as.Date(date), y = DOpsat_1)) +
  geom_line() +
  facet_wrap(~depth_round) +
  geom_hline(aes(yintercept = 50))

# number of days below 2 mg/L
buoy_bot <- buoy_avg %>% 
  filter(depth_round==8) %>% 
  select(date, depth_round, DOconc_1, DOpsat_1) %>% 
  mutate(anox = ifelse(DOconc_1 <= 2, 1, 0),
         yearmonth = format(as.Date(date), "%Y-%m"))

buoy_summ <- buoy_bot %>% 
  group_by(yearmonth) %>% 
  mutate(n = sum(anox)) %>% 
  distinct(yearmonth, .keep_all = TRUE)

ggplot(buoy_summ, aes(x = as.Date(date), y = n)) +
  geom_line() +
  geom_point()

#anoxia factor

