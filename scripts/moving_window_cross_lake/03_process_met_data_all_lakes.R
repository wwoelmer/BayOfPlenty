# process met data across all lakes
# data coverage from 2000-2020 for all lakes
# data are from ERA5

library(tidyverse)

fls <- list.files('./data/moving_window_analysis_cross_lake/met_data/originals')
fls

for(i in 1:length(fls)){
  met <- read.csv(file.path('./data/moving_window_analysis_cross_lake/met_data', fls[i]))
  met <- met %>% 
    mutate(windspeed = sqrt(MET_wnduvu^2 + MET_wnduvv^2)) %>% 
    select(Date, MET_tmpair, windspeed, MET_pprain) %>% 
    rename(date = Date)
  met$date <- as.Date(met$date)
  
  colnames(met) <- c('date', 'air_temp', 'windspeed', 'rain')
  
  x <- str_split(fls[i], '[-_]')[[1]][1]
  met$lake <- x
  
  # calculate max, min, mean metrics
  met_summ <- met %>% 
    mutate(month = month(date),
           year = year(date)) %>% 
    group_by(month, year) %>% 
    summarise_at(vars(c('air_temp', 'windspeed')), 
                 list(min = min, mean = mean,  max = max))  
  
  rain_summ <- met %>% 
    mutate(month = month(date),
           year = year(date)) %>% 
    group_by(month, year) %>% 
    summarise(rain_sum = sum(rain))  
  
  all_sum <- left_join(met_summ, rain_summ)
  
  write.csv(met, paste0('./data/moving_window_analysis_cross_lake/met_data/', x,'_met_', min(year(met$date)), "_", 
                        max(year(met$date)), '.csv'), row.names = FALSE)
  write.csv(all_sum, paste0('./data/moving_window_analysis_cross_lake/met_data/', x,'_met_summaries',
                            min(year(met$date)), "_", 
                            max(year(met$date)), '.csv'), row.names = FALSE)
  
}
