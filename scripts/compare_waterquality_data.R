# compare LAWA wq data to data from Deniz

lawa <- read.csv('./data/processed_data/BoP_wq_2007_2021.csv')
lawa <- lawa %>% 
  select(lake, site, date, chla_ugL_INT:TP_mgm3) 
colnames(lawa)[4:ncol(lawa)] <- paste0(colnames(lawa)[4:ncol(lawa)], "_lawa")
lawa <- lawa %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(lake=='Rotoehu')


other <- read.csv("./data/processed_data/rotoehu_waterquality_2000_2021.csv")  
other <- other %>% 
  select(lake, site, date, chla_ugL_INT:bottom_TP_ugL) 
colnames(other)[4:ncol(other)] <- paste0(colnames(other)[4:ncol(other)], "_other")
other <- other %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  select(-top_DRP_ugL_other, -bottom_DRP_ugL_other)
other$site <- as.character(other$site)

both <- left_join(lawa, other, by = c('year', 'month', 'lake', 'site'))
both <- both %>% 
  rename(date = date.x) %>% 
  select(-date.y, lake, site, date, year, month,
         chla_ugL_INT_lawa, chla_ugL_INT_other,
         NH4_mgL_lawa, top_NH4_ugL_other, bottom_NH4_ugL_other,
         pH_lawa, top_pH_other, bottom_pH_other,
         secchi_m_lawa, secchi_m_other,
         TN_mgm3_lawa, top_TN_ugL_other, bottom_TN_ugL_other,
         TP_mgm3_lawa, top_TP_ugL_other,  bottom_TP_ugL_other)

############### chl-a
ggplot(both, aes(x = chla_ugL_INT_lawa, y = chla_ugL_INT_other)) +
  geom_point() +
  geom_abline()

############### NH4 
#top
ggplot(both, aes(x = NH4_mgL_lawa, y = top_NH4_ugL_other/1000)) +
  geom_point() +
  geom_abline()

#bottom
ggplot(both, aes(x = NH4_mgL_lawa, y = bottom_NH4_ugL_other/1000)) +
  geom_point() +
  geom_abline()

############## pH
#top
ggplot(both, aes(x = pH_lawa, y = top_pH_other)) +
  geom_point() +
  geom_abline()


#bottom
ggplot(both, aes(x = pH_lawa, y = bottom_pH_other)) +
  geom_point() +
  geom_abline()


############## secchi
#top
ggplot(both, aes(x = secchi_m_lawa, y = secchi_m_other)) +
  geom_point() +
  geom_abline()


############## TN
#top
ggplot(both, aes(x = TN_mgm3_lawa, y = top_TN_ugL_other)) +
  geom_point() +
  geom_abline()


#bottom
ggplot(both, aes(x = top_TN_ugL_other, y = bottom_TN_ugL_other)) +
  geom_point() +
  geom_abline()


############## TP
#top
ggplot(both, aes(x = TP_mgm3_lawa, y = top_TP_ugL_other)) +
  geom_point() +
  geom_abline()

#bottom
ggplot(both, aes(x = TP_mgm3_lawa, y = bottom_TP_ugL_other)) +
  geom_point() +
  geom_abline()



