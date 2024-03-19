library(tidyverse)

df <- read.csv('./data/raw_data/WQ Master_Dataframe_2021.csv', sep = ';')
df$Date <- as.Date(df$Date)
min(df$Date)

df <- df %>% 
  mutate(original_site = LocationName) %>% 
  separate(LocationName, c('blank', 'lake', 'blank2', 'blank3', 'site', 'depth'))

df <- df %>% 
  select(-c(blank, blank2, blank3, SiteID, depth, original_site))

colnames(df) <- c('lake', 'site', 'date', 'depthfrom', 'variable', 'value', 'sample_depth')

# add in max depth for each lake
dpth <- read.csv('./data/processed_data/geomorphic_characteristics.csv', sep = ';') %>% 
  select(lake, depth_m) %>% 
  rename(max_depth = depth_m)

df <- left_join(df, dpth, by = 'lake') %>% 
  select(lake, site, max_depth, date, variable, value, depthfrom, sample_depth)

# remove erroneous datapoint where secchi is 28m in Rotoehu which is only 13m deep
df$value[df$variable=='SecchiDepth (m)' & df$lake=='Rotoehu' & df$date==as.Date('2020-11-17')] <- NA

# try to figure out how to reassign the 'hypo' samples, should these all just be 'bottom'?
ggplot(df, aes(x = date, y = value, color = as.factor(paste0(lake, sample_depth)))) +
  geom_point() +
  facet_wrap(~variable, scales = 'free')

df %>% 
  filter(lake=='Okaro') %>% 
  ggplot(aes(x = date, y = value, color = as.factor(paste0(lake, sample_depth)))) +
  geom_point() +
  facet_wrap(~variable, scales = 'free')

ggplot(df, aes(x = depthfrom, fill = sample_depth)) +
  geom_histogram() +
  facet_wrap(~lake)

## if both hypo and bottom measurements exist, take the bottom one
df_clean <- df %>% 
  group_by(lake, site, date, variable) %>% 
  filter(if(any(sample_depth=='Bottom')) sample_depth!='Hypo' else TRUE) %>% 
  mutate(depth = ifelse(sample_depth=='Top', 'top', 'bottom'))
  
ggplot(df_clean, aes(x = as.Date(date), y = value, color = as.factor(depth))) +
  geom_point() +
  facet_wrap(~variable, scales = 'free')
  
df_clean <- df_clean %>% 
  mutate(pct_depth_diff = abs(max_depth - depthfrom)/max_depth)

# how different are the depth values from the maximum depth
ggplot(df_clean, aes(x = sample_depth, y = pct_depth_diff, color = paste0(lake, site))) +
  geom_point() +
  facet_wrap(site~lake, scales = 'free')
# I think some of the weirdness in rotoiti is due to sampling at some very 
# shallow sites, so the "bottom" there is very different from teh max depth of 
# rotoiti (which is super deep)

df_wide <- df_clean %>% 
  select(-c(max_depth, depthfrom, sample_depth, pct_depth_diff)) %>% 
  group_by(lake, site, date, variable, depth) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  pivot_wider(names_from = c(variable, depth), values_from = value)

# rename the variable columns to follow good naming convention
colnames(df_wide) <- c("lake", "site", "date", "chla_mgm3_top", "DRP_mgm3_bottom", "DRP_mgm3_top", 
  "NH4_mgm3_bottom", "NH4_mgm3_top","NNN_mgm3_bottom", "NNN_mgm3_top", "secchi_m", 
  "TN_mgm3_bottom", "TN_mgm3_top", "TP_mg_m3_bottom", "TP_mgm3_top", "turb_NTU_bottom", "turb_NTU_top",
  "pH_bottom", "pH_top", "chla_mgm3_bottom" )

 write.csv(df_wide, './data/BoP_WQ_formatted.csv', row.names = FALSE)

########################################################################################################
# format rotoehu specifically
df_rotoehu <- df %>% 
  filter(lake=='Rotoehu')

ggplot(df_rotoehu, aes(x = date, y = value, color = as.factor(depthfrom))) +
  geom_point() +
  facet_wrap(~variable, scales = 'free')





df <- df %>% 
  mutate(depth = ifelse(sample_depth=='Top', 'top', 'bottom'))

ggplot(df, aes(x = depthfrom, fill = depth)) +
  geom_histogram() +
  facet_wrap(~lake)


# group samples at top or bottom based on 'depthfrom'
df_rotoehu <- df_rotoehu %>% 
  mutate(depth = ifelse(depthfrom < 2.6, 'top', 'bottom'),
         depth = ifelse(is.na(depth), sample_depth, depth),
         depth = ifelse(depth=='Top', 'top', depth),
         depth = ifelse(depth=='Bottom', 'bottom', depth))

df_rotoehu <- df_rotoehu %>% 
  select(-c(depthfrom, sample_depth))

# now that we've categorized depths, there are some duplicates in cases where more than one sample above or below 2.6m was taken
# let's take the average when more than one sample is present
df_rotoehu <- df_rotoehu %>% 
  group_by(lake, date, depth, variable) %>% 
  mutate(value = mean(value)) %>% 
  distinct(lake, date, depth, variable, .keep_all = TRUE)

ggplot(df_rotoehu, aes(x = date, y = value, color = as.factor(depth))) +
  geom_point() +
  facet_wrap(~variable, scales = 'free')

# wide format
df_wide <- df_rotoehu %>% 
  pivot_wider(names_from = c(depth, variable), values_from = value) 

# add units to col names of values
colnames(df_wide) <- c('lake', 'site', 'date', 
                       'chla_ugL', 
                       'top_DRP_ugL', 'bottom_DRP_ugL',
                       'top_NH4_ugL', 'bottom_NH4_ugL',
                       'top_NO3_ugL', 'bottom_NO3_ugL',
                       'top_pH', 'bottom_pH', 
                       'secchi_m', 
                       'top_TN_ugL', 'bottom_TN_ugL',
                       'top_TP_ugL', 'bottom_TP_ugL',
                       'top_turbidity_NTU', 'bottom_turbidity_NTU')

ggplot(df_wide, aes(x = top_pH, y = bottom_pH)) +
  geom_point() +
  geom_abline()

ggplot(df_wide, aes(x = top_turbidity_NTU, y = bottom_turbidity_NTU)) +
  geom_point() +
  geom_abline()

###############################################################################################
# interpolate missing data
df_wide <- df_wide[order(df_wide$date),]

df_wide <- df_wide %>% 
  group_by(lake, site) %>%
  mutate(across(chla_ugL:bottom_turbidity_NTU, na.approx))

###############################################################################################
# calculate nitrogen to phosphorus ratios, just for the top

# add small value to drp data for ratios when dividing by 0
min_val <- min(df_wide[df_wide$top_DRP_ugL>0,"top_DRP_ugL"], na.rm = TRUE)


df_wide <- df_wide %>% 
  mutate(TN_TP = top_TN_ugL/top_TP_ugL,
         DN_DP = (top_NH4_ugL + top_NO3_ugL)/top_DRP_ugL,
         DN_TN = (top_NH4_ugL + top_NO3_ugL)/top_TN_ugL,
         DP_TP = top_DRP_ugL/top_TP_ugL)

df_wide <- df_wide %>% 
  rename(chla_ugL_INT = chla_ugL)
colnames(df_wide)

write.csv(df_wide, paste0('./data/processed_data/rotoehu_waterquality_', min(year(df_wide$date)), '_', max(year(df_wide$date)), '.csv'), row.names = FALSE)

#################################################################################################
# some figures
ggplot(df_wide, aes(x = date, y = TN_TP, color = (DP_TP))) +
  geom_point() +
  scale_color_distiller(palette = 'Spectral')

ggplot(df_wide, aes(x = date, y = TN_TP, color = (DN_TN))) +
  geom_point() +
  scale_color_distiller(palette = 'Spectral')

ggplot(df_wide, aes(x = date, y = DN_DP)) +
  geom_point()

ggplot(df_wide, aes(x = date, y = DN_TN)) +
  geom_point()

ggplot(df_wide, aes(x = date, y = DP_TP)) +
  geom_point()

ggplot(df_wide, aes(x = TN_TP, y = DN_DP, color = DP_TP)) +
  geom_point() +
  ylim(0, 50) +
  scale_color_distiller(palette = 'Spectral')

