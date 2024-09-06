# combine multiple data sources together
library(tidyverse)
library(plotly)
library(zoo)
library(readxl)

yr_to_hydro_yr <- function(data){ # data = dataframe with 'date' column in as.POSIXct() format
  data$hydroyear <- data$date+(184*60*60*24)
  data$hydroyear <- format(data$hydroyear,"%Y")
  data$hydroyear <- as.numeric(data$hydroyear)
  data$hydroyear_label <- paste(data$hydroyear-1, data$hydroyear, sep = "-")
  return(data)
}

##########################
# use water quality data from Deniz, which goes back further in time
wq1 <- read.csv("./data/processed_data/rotoehu_waterquality_2000_2021.csv")
wq1$date <- as.Date(wq1$date)
wq1$site <- as.character(wq1$site)

# add in newest wq data
wq2 <- read.csv('./data/processed_data/rotoehu_waterquality_2000_2024.csv') 
wq2 <- wq2 %>% 
  filter(date > max(wq1$date)) %>% 
  mutate(site = as.character(site),
         date = as.Date(date))

wq <- full_join(wq1, wq2)
wq <- wq %>% 
  mutate(year = year(date),
         month = month(date)) 
wq$category <- NA

###########################
# ctd data
ctd <- read.csv('./data/processed_data/rotoehu_ctd_1990_2024.csv')
ctd$date <- as.Date(ctd$date)
ctd <- ctd %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(depth_m %in% c(1, 8)) %>% # keep surface and bottom data
  distinct(year, month, depth_m, .keep_all = TRUE) %>% 
  rename(chla_ugL = chl_ugL,
         turbidity_NTU = turb_NTU,
         PAR_umolm2s = PAR)

ggplot(ctd, aes(x = date, y = DO_mgL, color = as.factor(depth_m))) +
  geom_line()

ggplot(ctd, aes(x = date, y = chla_ugL, color = as.factor(depth_m))) +
  geom_line()

ctd_wide <- ctd %>% 
  pivot_wider(names_from = depth_m, values_from = chla_ugL:temp_C)

ggplot(ctd_wide, aes(x = chla_ugL_1, y = chla_ugL_8)) +
  geom_point()


df <- left_join(wq, ctd_wide, by = c('year', 'month'))
df <- df %>% 
  rename(date = date.x) %>% 
  select(-c(category, date.y, DO_mgL_1, DO_mgL_8, chla_ugL_1, 
            PAR_umolm2s_8, turbidity_NTU_8)) %>% 
  select(lake, site, date, year, month, chla_ugL_INT, top_TN_ugL, top_TP_ugL,
         secchi_m, everything())

##############################################################################################
# get rid of 2024 data because we don't have met
df <- df %>% 
  filter(date < as.Date('2024-01-01'))

###########################################################################################
######################################
### thermal mixing metrics
mix <- read.csv('./data/processed_data/BoP_mix_index_1990_2024.csv')
mix <- mix %>% 
  select(lake, date, everything()) %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  distinct(lake, year, month, .keep_all = TRUE) %>% 
  select(-date)


df <- left_join(df, mix, by = c('lake', 'year', 'month'))

###############################################################################################
# interpolate missing data
df <- df[order(df$date),]

df <- df %>% 
  group_by(lake, site) %>% 
  mutate(chla_ugL_8 = na.approx(chla_ugL_8, na.rm = FALSE, rule = 2, maxgap = 15),
         DO_sat_1 = na.approx(DO_psat_1, na.rm = FALSE, rule = 2, maxgap = 15),
         DO_sat_8 = na.approx(DO_psat_8, na.rm = FALSE, rule = 2, maxgap = 15),
         PAR_umolm2s_1 = na.approx(PAR_umolm2s_1, na.rm = FALSE, rule = 2, maxgap = 15),
         turbidity_ntu_1 = na.approx(turbidity_NTU_1, na.rm = FALSE, rule = 2, maxgap = 15),
         temp_C_1 = na.approx(temp_C_1, na.rm = FALSE, rule = 2, maxgap = 15),
         temp_C_8 = na.approx(temp_C_8, na.rm = FALSE, rule = 2, maxgap = 15),
         schmidt_stability = na.approx(schmidt_stability, na.rm = FALSE, rule = 2, maxgap = 15))

#####################################
### met data
met <- read.csv('./data/processed_data/Rotoehu_met_summaries_1999_2023.csv')
df <- left_join(df, met, by = c('month', 'year'))

#####################################
## lake level
lvl <- read.csv('./data/processed_data/water_level_rotoehu.csv')
lvl <- na.omit(lvl)
lvl$avg_level_m <- as.numeric(lvl$avg_level_m)
lvl$date <- as.POSIXct(lvl$date)

lvl <- lvl %>% 
  group_by(hydroyear, month) %>% 
  mutate(monthly_avg_level_m = mean(avg_level_m, na.rm = TRUE))

df$date <- as.Date(df$date)
lvl$date <- as.Date(lvl$date)
lvl <- lvl %>% 
  ungroup() %>% 
  select(-month)

df <- left_join(df, lvl, by = 'date')

######################################
## alum loading
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


df <- left_join(df, al_sum, by = 'date')
# set NA's to zero as there was no dosing done on these dates
df <- df %>% 
  mutate(sum_alum = ifelse(date < min(al$date), 0, sum_alum),
         sum_alum = ifelse(date > max(al$date), 0, sum_alum))



ggplot(df, aes(x = date, y  = sum_alum)) +
  geom_point(size = 2)

## nutrients
nuts <- read.csv(paste0('./data/processed_data/inflow_nutrients_2001_2023.csv'))
nuts <- nuts %>% 
  mutate(year = year(date_inf),
         month = month(date_inf)) %>% 
  select(-date_inf) %>% 
  distinct(year, month)

df <- left_join(df,  nuts, by = c('year', 'month'))

######################################
## land cover data

lc <- read.csv('./data/processed_data/landcover_1996_2018.csv')
lc$site <- as.character(lc$site)

lc_wide <- lc %>% 
  pivot_wider(names_from = lc_category, values_from = c(area_pct, area_ha)) %>% 
  select(-catchment)

df <- left_join(df, lc_wide, by = c('lake', 'site', 'year'))

#################################################
# permutation entropy
#pe <- read.csv('./data/processed_data/PE_tli_vars.csv')
#pe <- pe %>% 
#  pivot_wider(names_from = variable, values_from = pe, names_prefix = 'PE_')
#pe$year <- as.numeric(pe$year)
#pe <- pe %>% 
#  select(-c(ndemb, n))

#df <- left_join(df, pe, by = 'year')

## write the dataset as master file
write.csv(df, './data/master_rotoehu.csv', row.names = FALSE)

