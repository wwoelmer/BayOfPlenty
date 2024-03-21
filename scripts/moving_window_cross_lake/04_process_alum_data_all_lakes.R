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
  select(date, L_alum_day) 

# sum the amount of alum dosed since last sampling dates
df <- read.csv('./data/moving_window_analysis_cross_lake/BoP_WQ_formatted.csv') %>% 
  filter(lake=='Rotoehu')
dates <- unique(df$date)
al_sum <- data.frame()

for(i in 2:length(dates)){ 
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
  geom_point(size = 2) +
  geom_line()

############################################################################################
# okaro
# data come from email from Justine Randall at BoPRC

okaro_al <- read.csv('./data/raw_data/okaro_alum_dosing.csv')


############################################################################################
# combine into one data file