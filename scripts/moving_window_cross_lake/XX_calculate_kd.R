# calculate Kd
library(stats)
library(tidyverse)

light <- read.csv('./data/moving_window_analysis_cross_lake/BoP_ctd_2003_2022.csv')
light <- light %>% 
  select(lake, site, date, depth_m, PAR_umolm2s)


#Limit to only observations with data (in case data is not entered for some days/depths)
light <- na.omit(light)

#get rid of zeroes (because you can't take log of 0)
light <- light[!light$PAR_umolm2s==0,]


ggplot(light, aes(x = as.Date(date), y = PAR_umolm2s, color = depth_m)) +
  geom_point() +
  facet_wrap(~lake)


#For-loop to plot PAR values by day
#Just to make sure everything looks relatively normal
plot_df <- light %>% 
  mutate(month_year = paste0(month(date), "_", year(date)))

dates <- unique(plot_df$month_year)

for (i in 250:length(dates)){
  j=dates[i]
  q <- subset(plot_df, plot_df$month_year == j)
  print(ggplot(q, aes(x = PAR_umolm2s, y = depth_m, color = lake)) +
    geom_point() +
    facet_wrap(~lake) +
      ggtitle(dates[i]))
  
  
} 


kd <- plyr::ddply(light, c('lake', 'site', 'date'), \(x){
  mod <- lm(log(x$PAR_umolm2s) ~ x$depth_m)
  Kd <- coef(mod)[[2]]
  return(data.frame(Kd = Kd))
  
  
})

head(kd)

ggplot(kd, aes(x = as.Date(date), y = Kd, color = site)) +
  geom_point() +
  facet_wrap(~lake)

write.csv(kd, './data/moving_window_analysis_cross_lake/light_extinction_kd.csv', row.names = FALSE)
