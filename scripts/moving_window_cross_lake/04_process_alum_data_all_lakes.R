# process alum loading data for any lakes that have received it

library(readxl)

############################################################################################
# rotoehu
al <- read_excel('./data/raw_data/Daily aluminium to Rotoehu.xlsx')
al <- al[,1:4]

colnames(al) <- c('date', 'kg_aluminum_day', 'kg_alum_day', 'L_alum_day')

ggplot(al, aes(x = as.Date(date), y = L_alum_day)) +
  geom_line()

al$date <- as.Date(al$date)
al <- al %>% 
  select(date, L_alum_day) %>% 
  rename(alum = L_alum_day) %>% 
  mutate(note = 'alum_units_L_day',
         lake = 'Rotoehu')

# sum the amount of alum dosed since last sampling dates
df <- read.csv('./data/moving_window_analysis_cross_lake/BoP_WQ_formatted.csv') %>% 
  filter(lake=='Rotoehu')
dates <- unique(df$date)
al_sum <- data.frame()

for(i in 2:length(dates)){ 
  sub <- al %>% 
    filter(date <= dates[i] & date > dates[i-1]) %>% 
    mutate(sum_alum = sum(alum), 
           sample_date = dates[i]) %>% 
    select(-alum)
  al_sum <- rbind(al_sum, sub)
  
}

al_sum <- al_sum %>% 
  distinct(sample_date, .keep_all = TRUE) %>% 
  select(-date) %>% 
  rename(date = sample_date)

ggplot(al_sum, aes(x = as.Date(date), y  = sum_alum)) +
  facet_wrap(~lake, scales = 'free') +
  geom_point(size = 2) +
  geom_line()

############################################################################################
# okaro
# data came from email from Justine Randall at BoPRC

okaro_al <- read.csv('./data/raw_data/okaro_alum_dosing.csv')
okaro_al <- okaro_al %>% 
  select(date, placeholder_tonnes_alum) %>% 
  rename(alum = placeholder_tonnes_alum) %>% 
  mutate(note = 'alum_units_tonnes',
         lake = 'Okaro',
         date = as.Date(date))

okaro_al <- na.omit(okaro_al)

# sum dosing which occured in each month since last sampling date
df <- read.csv('./data/moving_window_analysis_cross_lake/BoP_WQ_formatted.csv') %>% 
  filter(lake=='Okaro')
dates_o <- unique(df$date)
al_sum_okaro <- data.frame()

for(i in 2:length(dates_o)){ 
  sub <- okaro_al %>% 
    filter(date <= dates_o[i] & date > dates_o[i-1]) %>% 
    mutate(sum_alum = sum(alum), 
           sample_date = dates_o[i]) %>% 
    select(-alum)
  al_sum_okaro <- rbind(al_sum_okaro, sub)
  
}

al_sum_okaro <- al_sum_okaro %>% 
  distinct(sample_date, .keep_all = TRUE) %>% 
  select(-date) %>% 
  rename(date = sample_date)

ggplot(al_sum_okaro, aes(x = as.Date(date), y  = sum_alum)) +
  geom_point(size = 2) +
  geom_line()
  

############################################################################################
# combine into one data file
al_both <- full_join(al_sum, al_sum_okaro, by = c('date', 'lake', 'sum_alum', 'note'))

ggplot(al_both, aes(x = as.Date(date), y = sum_alum)) +
  geom_point() +
  facet_wrap(~lake, scales = 'free')

############################################################################################
write.csv(al_both, './data/processed_data/alum_rotoehu_okaro.csv', row.names = FALSE)

