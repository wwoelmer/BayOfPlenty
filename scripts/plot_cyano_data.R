cyano <- read.csv('./data/processed_data/BOPRC_cyanobacteria_okaro_rotoehu_2024.csv')

ggplot(cyano, aes(x = as.Date(cyano$date), y = biovolume, color = color)) +
  geom_point(size = 4) +
  scale_color_manual(values = c('green', 'orange', 'red')) +
  facet_wrap(~lake) +
  theme_bw() +
  xlab('Date') +
  ylab('Cyanobacterial Biovolume (mm3/L)') +
  theme(text = element_text(size = 18),
        legend.position = "none") 

cyano %>% 
  filter(lake=='rotoehu') %>% 
ggplot(aes(x = as.Date(date), y = biovolume, color = color)) +
  geom_point(size = 4) +
  scale_color_manual(values = c('green', 'orange', 'red')) +
  theme_bw() +
  xlab('Date') +
  ylab('Cyanobacterial Biovolume (mm3/L)') +
  theme(text = element_text(size = 18),
        legend.position = "none") 

cyano %>% 
  filter(lake=='okaro') %>% 
  ggplot(aes(x = as.Date(date), y = biovolume, color = color)) +
  geom_point(size = 4) +
  scale_color_manual(values = c('green', 'orange', 'red')) +
  theme_bw() +
  xlab('Date') +
  ylab('Cyanobacterial Biovolume (mm3/L)') +
  theme(text = element_text(size = 18),
        legend.position = "none") 
