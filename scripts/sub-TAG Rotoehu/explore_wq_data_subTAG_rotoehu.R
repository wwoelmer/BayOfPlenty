library(readxl)
library(tidyverse)

multiplesheets <- function(fname) {
  
  # getting info about all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x))
  data_frame <- lapply(tibble, as.data.frame)
  
  # assigning names to data frames
  names(data_frame) <- sheets
  
  # print data frame
  print(data_frame)
}

# specifying the path name
path <- './data/raw_data/Lakes Rotoehu WQ sample and CTD profile data from 2021-01-01 to 2024-08-01.xlsx'
dat <- multiplesheets(path)

# create dataframes from list
list2env(dat, envir = .GlobalEnv)
wq <- `WQ Sample Data`

wq <- wq %>% 
  select(LocationName, Time, `CHLA (mg/m^3)`, `DIN (g/m^3)`:`DRP (g/m^3)`, `NH4-N (g/m^3)`:`NO3-N (g/m^3)`, `TN (g/m^3)`, `TP (g/m^3)`, "VC - SD (m)") %>% 
  rename(chl_mgm3 = `CHLA (mg/m^3)`,
         DIN_gm3 = `DIN (g/m^3)`,
         DOC_gm3 = "DOC (g/m^3)",
         DRP_gm3 = "DRP (g/m^3)",
         NH4_gm3 = "NH4-N (g/m^3)",
         NNN_gm3 = "NNN (g/m^3)",
         NO2_gm3 = "NO2-N (g/m^3)",
         NO3_gm3 = "NO3-N (g/m^3)",
         TN_gm3 = "TN (g/m^3)",
         TP_gm3 = "TP (g/m^3)",
         secchi_m = "VC - SD (m)") 

# add depth column
wq <- wq %>% 
  mutate(depth = ifelse(LocationName=="Lake Rotoehu at Site 3 (Integrated)", 'integrated', 'bottom')) %>% 
  select(-LocationName)

# convert units
wq <- wq %>% 
  mutate(TN_mgm3 = TN_gm3*1000,
         TP_mgm3 = TP_gm3*1000,
         DRP_mgm3 = DRP_gm3*1000,
         DIN_mgm3 = DIN_gm3*1000,
         NH4_mgm3 = NH4_gm3*1000,
         NNN_mgm3 = NNN_gm3*1000)

# create date column, average if more than one entry per day
wq <- wq %>% 
  mutate(date = as.Date(Time)) %>% 
  select(-Time)

wq_long <- wq %>% 
  select(-c(DOC_gm3, DIN_gm3, DRP_gm3, NH4_gm3, NNN_gm3, NO2_gm3, NO3_gm3, TN_gm3, TP_gm3)) %>% 
  pivot_longer(c(chl_mgm3, TN_mgm3, TP_mgm3,
                 DRP_mgm3, DIN_mgm3, NH4_mgm3, NNN_mgm3, secchi_m), 
               names_to = 'variable', values_to = 'value')

wq_long <- wq_long %>% 
  group_by(date, depth, variable) %>% 
  summarise(value = mean(value, na.rm = TRUE))

ggplot(wq_long, aes(x = as.Date(date), y = value, color = depth)) +
  geom_point() +
  facet_wrap(~variable, scales = 'free') +
  theme_bw() +
  xlab('Date')

# calculate hydroyear, then monthly and annual TLI
wq_wide <- wq_long %>% 
  pivot_wider(names_from = variable, values_from = value)
  
### convert to hydroyear
yr_to_hydro_yr <- function(data){ # data = dataframe with 'date' column in as.POSIXct() format
  data$hydroyear <- data$date+(184*60*60*24)
  data$hydroyear <- format(data$hydroyear,"%Y")
  data$hydroyear <- as.numeric(data$hydroyear)
  data$hydroyear_label <- paste(data$hydroyear-1, data$hydroyear, sep = "-")
  return(data)
}

wq_wide$date <- as.POSIXct(wq_wide$date)
wq_wide <- yr_to_hydro_yr(data = wq_wide)
wq_wide <- wq_wide %>% 
  mutate(month = month(date))

wq_wide$date <- as.Date(wq_wide$date)

###################################################################################
# combine with whole time series
dat_hist <-  read.csv('./data/processed_data/90s_data/tli_rotoehu.csv')
dat_hist$depth <- 'integrated'
dat_hist$date <- as.POSIXct(dat_hist$date)
dat_hist <- dat_hist %>% 
  select(-tli_annual, -tli_monthly)


wq_all <- full_join(wq_wide, dat_hist)

wq_all <- wq_all %>% 
  arrange(date)
write.csv(wq_all, './data/processed_data/rotoehu_wq_data_1990_2024.csv', row.names= FALSE)  

wq_all_long <- wq_all %>% 
  pivot_longer(DIN_mgm3:secchi_m, names_to = 'variable')


wq_all_long %>% 
  filter(variable %in% c('chl_mgm3', 'secchi_m', 'TN_mgm3', 'TP_mgm3'),
         depth=='integrated') %>% 
ggplot(aes(x = as.Date(date), y = value, color = as.factor(hydroyear))) +
  geom_point(size = 2) +
  facet_wrap(~variable, scales = 'free') +
  theme_bw() +
  labs(color = 'Hydroyear') +
  xlab('Date')

###################################################################################
# calculate TLI
source('./scripts/R/tli_fx.R')

tli <- wq_all %>% 
  group_by(hydroyear) %>% 
  mutate(tli_annual = tli_fx(chl = chl_mgm3, TN = TN_mgm3, TP = TP_mgm3, secchi = secchi_m, timescale = 'annual')) %>% 
  mutate(month = month(date)) %>% 
  group_by(hydroyear, month) %>% 
  mutate(tli_monthly = tli_fx(chl = chl_mgm3, TN = TN_mgm3, TP = TP_mgm3, secchi = secchi_m, timescale = 'monthly'))

ggplot(tli, aes(x = as.Date(date), y = tli_annual)) +
  geom_line(size = 1.5) +
  geom_point(size = 1.5, aes(x = as.Date(date), y = tli_monthly, color = as.factor(month))) +
  theme_bw() +
  labs(color = 'Month') +
  geom_hline(yintercept = 3.9) +
  xlab('Date')

# calculate the annual standard deviation in TLI
tli <- tli %>% 
  group_by(hydroyear) %>% 
  mutate(sd = sd(tli_monthly, na.rm = TRUE),
         cv = sd/mean(tli_monthly))

library(plotly)
ggplotly(ggplot(tli, aes(x = tli_annual, y = sd)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = 'lm', group = 1))

tli %>% 
ggplot(aes(x = tli_annual, y = sd)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = 'lm', group = 1)


# clean up and convert into top and bottom data
tli_save <- tli %>% 
  ungroup() %>% 
  select(-DIN_mgm3, -c(hydroyear:cv)) %>% 
  mutate(label = ifelse(depth=='integrated', 'top', 'bottom')) %>% 
  rename(DRP_ugL = DRP_mgm3,
         NH4_ugL = NH4_mgm3,
         NO3_ugL = NNN_mgm3,
         TN_ugL = TN_mgm3,        
         TP_ugL = TP_mgm3,
         chla_ugL = chl_mgm3) %>% 
  pivot_longer(DRP_ugL:secchi_m, names_to = 'variable', values_to = 'value') %>% 
  na.omit() %>% 
  mutate(variable2 = paste0(label, "_", variable)) %>% 
  select(-variable, -label, -depth) %>% 
  mutate(value = ifelse(value < 0 , 0, value)) %>%
  distinct(date, variable2, .keep_all = TRUE) %>% 
  pivot_wider(names_from = 'variable2', values_from = 'value') %>% 
  rename(chla_ugL_INT = top_chla_ugL,
         secchi_m = top_secchi_m) %>% 
  mutate(lake = 'Rotoehu',
         site = 3) %>% 
  select(lake, site, date, everything()) %>% 
  mutate(DN_DP = ((top_NH4_ugL + top_NO3_ugL)/top_DRP_ugL),
         DN_TN = ((top_NH4_ugL + top_NO3_ugL)/top_TN_ugL),
         DP_TP = (top_DRP_ugL/top_TP_ugL),
         TN_TP = top_TN_ugL/top_TP_ugL)

tli_save <- tli_save %>% 
  filter(date > as.Date('2021-06-15'))

write.csv(tli_save, "./data/processed_data/rotoehu_waterquality_2000_2024.csv", row.names = FALSE)
##################################################################################
# ctd data
ctd <- `CTD Profile Data - 1m intervals`
ctd <- ctd %>% 
  rename(depth_m = "Depth (m)",
         chl_ugL = "Chlorophyll (ug/l)",
         DO_gm3 = "DO (g/m^3)",
         DO_sat = "DO sat (%)",
         PAR_umolm2s = "PAR (umol/m^2/s)",
         spcond_uScm = "SpC (uS/cm)",
         turb_NTU = "Turbidity NTU (_NTU)",
         temp_C = "Water Temp (degC)")

ctd <- ctd %>% 
  select(LocationName, Time, depth_m:temp_C)

ctd_long <- ctd %>% 
  pivot_longer(chl_ugL:temp_C, names_to = 'variable', values_to = 'value')

ggplot(ctd_long, aes(x = as.Date(Time), y = value, color = depth_m)) +
  geom_point() +
  facet_wrap(~variable, scales = 'free') +
  theme_bw()

# format for merging with historical ctd data
ctd_write <- ctd %>% 
  rename(date = Time,
         DO_psat = DO_sat,
         DO_mgl = DO_gm3,
         PAR = PAR_umolm2s,
         Conductivity = spcond_uScm)
  

write.csv(ctd_write, './data/processed_data/ctd_2021_2024_rotoehu.csv', row.names = FALSE)

#####################################
## lake level
lvl <- read_excel('./data/raw_data/EDS-686238-HL143688-Entire Record.xlsx', skip = 5)
lvl <- na.omit(lvl)
colnames(lvl) <- c('date', 'end', 'avg_level_m')
lvl$avg_level_m <- as.numeric(lvl$avg_level_m)
lvl <- lvl %>% select(-end)
lvl$date <- as.POSIXct(lvl$date)

new_lvl <- read.csv('./data/raw_data/BulkExport-HL143688-20240821165504.csv', skip = 5) 
colnames(new_lvl) <- c('datetime', 'level_m')
new_lvl <- na.omit(new_lvl)

new_lvl <- new_lvl %>% 
  mutate(date = as.Date(datetime, format = "%Y-%m-%d")) %>% 
  group_by(date) %>% 
  summarise(avg_level_m = mean(level_m, na.rm = TRUE))


## combine both level dataframes
lvl <- full_join(lvl, new_lvl)

lvl <- yr_to_hydro_yr(lvl)
lvl$month <- month(lvl$date)

lvl %>% 
  filter(date > as.Date('1954-01-01')) %>% 
ggplot(aes(x = as.Date(date), y = avg_level_m)) +
  geom_line(size = 1) +
  theme_bw() +
  xlab('Date') +
  ylab('Water level (m)')

write.csv(lvl, './data/processed_data/water_level_rotoehu.csv', row.names = FALSE)
