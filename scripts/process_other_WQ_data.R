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

# remove erroneous datapoint where secchi is 28m in Rotoehu which is only 13m deep
df$value[df$variable=='SecchiDepth (m)' & df$lake=='Rotoehu' & df$date==as.Date('2020-11-17')] <- NA

df_rotoehu <- df %>% 
  filter(lake=='Rotoehu')

ggplot(df_rotoehu, aes(x = date, y = value, color = as.factor(depthfrom))) +
  geom_point() +
  facet_wrap(~variable, scales = 'free')

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

