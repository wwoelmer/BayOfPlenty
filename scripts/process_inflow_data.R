# read in and combine different inflow data from BOPRC
library(tidyverse)

dir <- './data/raw_data/inflow'
fils <- list.files(dir)

# some metadata about units are in the first few columns
head_inf <- read.csv(file.path(dir, fils[2]))

# read in starting with data
inf <- read.csv(file.path(dir, fils[2]), skip = 14)
colnames(inf) <- c('date', 'date_utc', 'discharge_m3s', 'approval', 'grade', 'qualifiers')
inf <- inf %>% select(date, discharge_m3s)

ggplot(inf, aes(x = as.Date(date), y = discharge_m3s)) +
  geom_point()

write.csv(inf, paste0('./data/processed_data/discharge_', year(min(inf$date)), '_', year(max(inf$date)), '.csv'), row.names = FALSE)

###############################################################
# go through the nutrient data and combine
# some metadata about units are in the first few columns
head_drp <- read.csv(file.path(dir, fils[3]))

dat <- read.csv(file.path(dir, fils[3]), skip = 14)
colnames(dat) <- c('date', 'date_utc', 'DRP_gm3', 'approval', 'grade', 'qualifiers')
dat <- dat %>% select(date, DRP_gm3)

ggplot(dat, aes(x = as.Date(date), y = DRP_gm3)) +
  geom_point()

for(i in 4:length(fils)){
  head <- read.csv(file.path(dir, fils[i]))
  print(head[6,1])
  
  id <- str_split(head[6,1], ": ")[[1]][2]
  units <- str_split(head[5,1], ": ")[[1]][2]
  units <- gsub("/", "", units)
  units <- str_remove(units, stringr::fixed("^"))
  var_name <- paste0(id, "_", units)
  
  d <- read.csv(file.path(dir, fils[i]), skip = 14)
  colnames(d) <- c('date', 'date_utc', var_name, 'approval', 'grade', 'qualifiers')
  d <- d %>% select(date, var_name)

  dat <- left_join(dat, d)
}

# combine the TKN and TN data
dat <- dat %>% 
  mutate(TN_gm3 = ifelse(is.na(TN_gm3), TKN_gm3, TN_gm3)) %>% 
  select(-TKN_gm3)

dat_long <- dat %>% 
  pivot_longer(DRP_gm3:TP_gm3, names_to = 'variable', values_to = 'value')

ggplot(dat_long, aes(x = as.Date(date), y = value)) +
  geom_point() +
  facet_wrap(~variable, scales = 'free')

colnames(dat) <- paste0(colnames(dat), "_inf")

write.csv(dat, paste0('./data/processed_data/inflow_nutrients_', year(min(dat$date)), '_', year(max(dat$date)), '.csv'), row.names = FALSE)




