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

# reassign the 'hypo' samples, should these all just be 'bottom'?
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

 write.csv(df_wide, './data/moving_window_analysis_cross_lake/BoP_WQ_formatted.csv', row.names = FALSE)

