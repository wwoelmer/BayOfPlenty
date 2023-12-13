# check if spc and lake level are related

# specifying the path name
path <- './data/raw_data/BOPRC Lake Sampling CTD Profile Data - All Lakes Full Record.xlsx'
dat <- multiplesheets(path)

# create dataframes from list
list2env(dat, envir = .GlobalEnv)

sheet = excel_sheets(path)

# applying sheet names to dataframe names
data_frame = lapply(setNames(sheet, sheet), 
                    function(x) read_excel(path, sheet=x))

# attaching all dataframes together
data_frame = bind_rows(data_frame, .id="Sheet")

###########################################################################################
# format data

# split site name up into distinct columns
df <- data_frame %>% 
  separate(LocationName, c('blank', 'lake', 'blank2', 'blank3', 'site', 'blank4')) 

# fix okawa issue
df <- df %>% 
  mutate(site = ifelse(blank3!='Site', blank3, site))

df <- df %>% 
  select(-c('blank', 'blank2', 'blank3', 'blank4', Site, Sheet))

# rename columns
colnames(df) <- c('lake', 'site', 'date', 'depth_m', 'chla_ugL', 'DO_gm3', 'DO_sat',
                  'PAR_umolm2s', 'spcond_uScm', 'SpC_uScm', 'turbidity_ntu', 'temp_C', 'lake_level_m')

df$method <- 'ctd'
df$time <- df$date
df$date <- as.Date(df$date)

# and rearrange columns
df <- df %>% 
  select(lake, site, date, depth_m, everything(), -lake_level_m)

###############################################################################################
# interpolate missing data
df <- df[order(df$date),]

df <- df %>% 
  group_by(lake, site, depth_m) %>% 
  mutate(chla_ugL = na.approx(chla_ugL, na.rm = FALSE, rule = 2, maxgap = 15),
         DO_gm3 = na.approx(DO_gm3, na.rm = FALSE, rule = 2, maxgap = 15),
         DO_sat = na.approx(DO_sat, na.rm = FALSE, rule = 2, maxgap = 15),
         PAR_umolm2s = na.approx(PAR_umolm2s, na.rm = FALSE, rule = 2, maxgap = 15),
         spcond_uScm = na.approx(spcond_uScm, na.rm = FALSE, rule = 2, maxgap = 15),
         SpC_uScm = na.approx(SpC_uScm, na.rm = FALSE, rule = 2, maxgap = 15),
         turbidity_ntu = na.approx(turbidity_ntu, na.rm = FALSE, rule = 2, maxgap = 15),
         temp_C = na.approx(temp_C, na.rm = FALSE, rule = 2, maxgap = 15))

##################################################################################
# I think SpC_uScm is uncorrected for temperature and spcond_uScm is corrected
# calculate corrected from temp when values are missing
spc <- df %>% 
  select(lake, site, depth_m, date, spcond_uScm, SpC_uScm, temp_C) %>% 
  mutate(corr_spc = SpC_uScm/(1 + 0.02*(temp_C - 25)),
         diff = spcond_uScm - corr_spc)


#####################################
## lake level

lvl <- read_excel('./data/raw_data/EDS-686238-HL143688-Entire Record.xlsx', skip = 5)
lvl <- na.omit(lvl)
colnames(lvl) <- c('date', 'end', 'avg_level_m')
lvl$avg_level_m <- as.numeric(lvl$avg_level_m)
lvl <- lvl %>% select(-end)
lvl$date <- as.Date(lvl$date)

dat <- left_join(spc, lvl) %>% 
  filter(lake=='Rotoehu')

ggplot(dat, aes(y = spcond_uScm, x = avg_level_m)) +
  geom_point() +
  geom_smooth(method = 'lm')
