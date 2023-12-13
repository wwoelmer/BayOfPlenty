# process alum dosing data

library(readxl)

al <- read_excel('./data/raw_data/Daily aluminium to Rotoehu.xlsx')
al <- al[,1:4]

colnames(al) <- c('date', 'kg_aluminum_day', 'kg_alum_day', 'L_alum_day')

ggplot(al, aes(x = as.Date(date), y = L_alum_day)) +
  geom_line()

write.csv(al, './data/processed_data/alum_dosing_rotoehu_2011_2022.csv', row.names = FALSE)
