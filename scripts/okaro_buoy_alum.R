library(tidyverse)

okaro <- read.csv('./data/raw_data/Okaro_202104-202402_profiles.csv')

okaro_long <- okaro %>% 
  select(-c(TmpChg, StrsTt)) %>% 
  pivot_longer(TmpWtr:FlPhyc, names_to = 'variable', values_to = 'value')

ggplot(okaro_long, aes(x = as.Date(DateTime), y = value, color = round(DptSns))) +
  geom_point() +
  facet_wrap(~variable, scales = 'free')


okaro_long %>% 
  filter(variable=='FlChlr') %>% 
  ggplot(aes(x = as.Date(DateTime), y = value, color = DptSns)) +
  geom_point() +
  scale_color_continuous(high = "#132B43", low = "#56B1F7") +
  ggtitle('Okaro FlChlr')


okaro_long %>% 
  filter(variable=='FlPhyc') %>% 
  ggplot(aes(x = as.Date(DateTime), y = value, color =(round(DptSns)))) +
  geom_point() 
