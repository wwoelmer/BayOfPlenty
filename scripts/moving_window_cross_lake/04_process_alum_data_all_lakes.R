# process alum loading data for any lakes that have received it

library(readxl)

# rotoehu
al <- read_excel('./data/raw_data/Daily aluminium to Rotoehu.xlsx')
al <- al[,1:4]

colnames(al) <- c('date', 'kg_aluminum_day', 'kg_alum_day', 'L_alum_day')

ggplot(al, aes(x = as.Date(date), y = L_alum_day)) +
  geom_line()

al <- read.csv('./data/processed_data/alum_dosing_rotoehu_2011_2022.csv')
al$date <- as.Date(al$date)
al <- al %>% 
  select(date, L_alum_day) 

ggplot(al, aes(x = date, y  = L_alum_day)) +
  geom_point(size = 2)

# sum the amount of alum dosed since last sampling dates
dates <- unique(df$date)
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

ggplot(al_sum, aes(x = date, y  = sum_alum)) +
  geom_point(size = 2)


# okaro

# combine into one data file