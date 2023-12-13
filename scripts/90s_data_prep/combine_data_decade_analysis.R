# driver analysis for 90's data at Rotoehu
library(tidyverse)
library(lubridate)

# tli
tli <- read.csv('./data/processed_data/90s_data/tli_rotoehu.csv')

getDecade <- function(year) {
  year <- ifelse(year<2000, year-1900, year)
  decade <- floor(year/10) * 10
  return (decade)
}

################################################################################
# met
met <- read.csv('./data/processed_data/Rotoehu_met_summaries_1980_2022.csv')
### convert to hydroyear
yr_to_hydro_yr <- function(data){ # data = dataframe with 'date' column in as.POSIXct() format
  data$hydroyear <- data$date+(184*60*60*24)
  data$hydroyear <- format(data$hydroyear,"%Y")
  data$hydroyear <- as.numeric(data$hydroyear)
  data$hydroyear_label <- paste(data$hydroyear-1, data$hydroyear, sep = "-")
  return(data)
}
met$date <- paste0(met$year, "-", met$month, "-01")
met$date <- as.POSIXct(met$date)
met <- yr_to_hydro_yr(met)
met$decade <- getDecade(met$year)

# select most "important" met variables
met <- met %>% 
  select(year, hydroyear, month, decade, everything(), -hydroyear_label, -date)

met_long <- met %>% 
  filter(decade!='2020' & decade!='80') %>% 
  pivot_longer(air_temp_min:windspeed_max, names_to = 'variable', values_to = 'value')

df <- left_join(tli, met, by = c('hydroyear', 'month'))
df <- df %>% 
  select(date, hydroyear, hydroyear_label, month, decade, tli_annual, tli_monthly, everything(), -year)
################################################################################
# climate indices
soi <- read.csv('./data/raw_data/soi-rolling-average-1990-2022-and-temperature-anomaly-1990-2020.csv')
soi <- soi %>% 
  mutate(month = ifelse(month < 10, paste0("0", month), month))
soi$date <- paste0(soi$year, "-", soi$month, "-01")
soi$date <- ymd(soi$date)
soi$decade <- getDecade(soi$year)
soi <- soi %>% filter(decade!='2020' & decade!='80') 
soi$date <- as.POSIXct(soi$date)
soi <- yr_to_hydro_yr(soi)
soi$month <- as.integer(soi$month)

ggplot(soi, aes(x = as.Date(date), y = soi_3mth_mean, color = as.factor(year))) +
  geom_point() +
  geom_line()

ggplot(soi, aes(x = as.Date(date), y = de_trended_temp_anomaly)) +
  geom_point() +
  geom_line()

soi <- soi %>% select(-date)

a <- ggplot(soi, aes(x = as.factor(decade), y = soi_3mth_mean, fill = as.factor(decade))) +
  geom_boxplot() +
  theme_bw() +
  labs(fill = 'Decade') +
  xlab('Decade') +
  ylab('3 month mean SOI')

b <- ggplot(soi, aes(x = as.factor(decade), y = de_trended_temp_anomaly, fill = as.factor(decade))) +
  geom_boxplot() +
  theme_bw() +
  labs(fill = 'Decade') +
  xlab('Decade') +
  ylab('Detrended Temp Anomaly')

c <- ggplot(soi, aes(fill = soi_phase, x = as.factor(decade))) +
  geom_bar(position = 'dodge') +
  scale_fill_brewer(palette = 'Dark2') +
  ylab('Count (months)') +
  xlab('Decade') +
  labs(fill = 'Phase') +
  theme_bw()
ggarrange(a, b, c)

df <- left_join(df, soi, by = c('hydroyear', 'month', 'decade', 'hydroyear_label'))

#################################################################################
# temp and thermal stratification metrics
temp <- read.csv('./data/processed_data/90s_data/temp_metrics_rotoehu_90s.csv')
temp <- temp %>% 
  select(-X) %>% 
  mutate(month = month(date)) 
temp$date <- as.POSIXct(temp$date)
temp <- yr_to_hydro_yr(temp)
temp$month <- as.integer(temp$month)
temp <- temp %>% select(-date, -hydroyear_label) %>% 
  distinct(hydroyear, month, .keep_all = TRUE)

df <- left_join(df, temp, by = c('hydroyear', 'month'))

#################################################################################
# lake level
lvl <- read_excel('./data/raw_data/EDS-686238-HL143688-Entire Record.xlsx', skip = 5)
lvl <- na.omit(lvl)
colnames(lvl) <- c('date', 'end', 'avg_level_m')
lvl$avg_level_m <- as.numeric(lvl$avg_level_m)
lvl <- lvl %>% select(-end)
lvl$date <- as.POSIXct(lvl$date)
lvl <- yr_to_hydro_yr(lvl)
lvl$month <- month(lvl$date)
lvl <- lvl %>% select(-date)

df <- left_join(df, lvl, by = c('hydroyear', 'hydroyear_label', 'month'))

# land cover (only starts in 1996)?

#################################################################################
# bottom nutrients from Lake_Rotoehu
nuts <- read_excel('./data/raw_data/Rotoehu_1990_1999_PaulScholes.xlsx')
nuts$Date <- as.POSIXct(nuts$Date)
nuts <- nuts %>% 
  rename(date = Date,
         depth_m = DepthFrom) %>% 
  select(date, depth_m, Unit, Results) %>% 
  filter(depth_m > 6.75) 
nuts_long <- nuts %>% 
  pivot_wider(names_from = 'Unit', values_from = 'Results') %>% 
  select(date, depth_m, `DRP (mg/m3)`, `NH4-N (mg/m3)`) %>% 
  rename(DRP_mgm3 = `DRP (mg/m3)`,
         NH4_mgm3 = `NH4-N (mg/m3)`) %>% 
  mutate(month = month(date)) 
nuts_long <- yr_to_hydro_yr(nuts_long)
nuts_long <- nuts_long %>% 
  distinct(month, hydroyear, .keep_all = TRUE) %>% 
  select(-depth_m)

nuts_2000s <- read.csv("./data/processed_data/rotoehu_waterquality_2000_2021.csv")
nuts_2000s$date <- as.POSIXct(nuts_2000s$date)
nuts_2000s <- nuts_2000s %>% 
  select(date, bottom_DRP_ugL, bottom_NH4_ugL) %>% 
  mutate(month = month(date)) %>% 
  rename(DRP_mgm3 = bottom_DRP_ugL,
         NH4_mgm3 = bottom_NH4_ugL)
nuts_2000s <- yr_to_hydro_yr(nuts_2000s)

nuts_all <- rbind(nuts_long, nuts_2000s)
nuts_all <- na.omit(nuts_all)

ggplot(nuts_all, aes(x = as.Date(date), y = DRP_mgm3)) +
  geom_point()

nuts_all <- nuts_all %>% select(-date)

df <- left_join(df, nuts_all, by = c('hydroyear', 'hydroyear_label', 'month'))

#################################################################################
df <- df %>% 
  distinct(hydroyear, month, .keep_all = TRUE)
write.csv(df, './data/processed_data/90s_data/rotoehu_tli_drivers_1990_2021.csv', row.names = FALSE)


