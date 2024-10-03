# calculate epilimnetic and hypolimnetic volume in Rotoehu for Grant's alum calculations

# read in CTD profiles from monitoring days
ysi <- read.csv('./data/processed_data/rotoehu_ctd_1990_2024.csv')

# read in bathy estimates
bty <- read_excel('./data/raw_data/Rotlakes_bathymetry.xls', skip = 1) 
colnames(bty) <- c('lake', 'depth_m', 'vol_to_bottom_m3', 'vol_at_countour_m3', 
                   'planar_sa_m2', 'model_sd_m2')

bty$depth_m <- abs(bty$depth_m)
bty <- bty %>% 
  filter(lake=='Rotoehu')


# area and depth of bathymetry
bthA <- bty$model_sd_m2
bthD <- bty$depth_m

bty_interp <- data.frame(lake = bty$lake[1],
                         depth_m = seq(1, max(bty$depth_m), 1),
                         vol_at_contour_m3  = approx(bty$depth_m, bty$vol_at_countour_m3 , seq(0, max(bty$depth_m) - 1, 1))$y)

bty_interp <- bty_interp %>% 
  mutate(depth_m = depth_m - 1)

rotoehu_vol <- bty_interp %>% 
  summarise(vol = sum(vol_at_contour_m3))
rotoehu_vol

ysi <- ysi %>% 
  select(date, depth_m, temp_C) %>% 
  distinct(date, depth_m, .keep_all = TRUE) %>% 
  group_by(date) %>% 
  mutate(thermo_depth = thermo.depth(temp_C, depth_m, mixed.cutoff = 1)) %>% 
  mutate(hypo_depth = meta.depths(temp_C, depth_m, seasonal = FALSE, mixed.cutoff = 0)[2]) %>% 
  mutate(epi_depth = meta.depths(temp_C, depth_m, seasonal = FALSE, mixed.cutoff = 0)[1])# 

ggplot(ysi, aes(x = as.Date(date), y = thermo_depth)) +
  geom_line() 

ysi <- ysi %>% 
  distinct(date, .keep_all = TRUE) %>% 
  select(date, thermo_depth, epi_depth, hypo_depth) %>% 
  mutate(lake = 'Rotoehu')


vol <- full_join(ysi, bty_interp)

vol <- vol %>% 
  group_by(date) %>% 
  mutate(epi_vol = sum(ifelse(depth_m < round(thermo_depth), vol_at_contour_m3, 0), na.rm = TRUE),
         hypo_vol = sum(ifelse(depth_m >= round(thermo_depth), vol_at_contour_m3, 0), na.rm = TRUE),
         epi_vol = ifelse(is.nan(thermo_depth), sum(vol_at_contour_m3, na.rm = TRUE), epi_vol),
         whole_vol = sum(vol_at_contour_m3, na.rm = TRUE),
         epi_plus_hypo = epi_vol + hypo_vol ) 


vol %>% 
  distinct(date, .keep_all = TRUE) %>% 
ggplot(aes(x = as.Date(date), y = epi_vol, color = 'epilimnion volume')) +
  geom_point() +
  geom_line() +
  geom_point(aes(x = as.Date(date), y= hypo_vol, color = 'hypolimnion volume')) +
  geom_line(aes(x = as.Date(date), y= hypo_vol, color = 'hypolimnion volume'))

ggplot(vol, aes(x = as.Date(date), y = epi_vol - hypo_vol)) +
  geom_point() +
  geom_line()

write.csv(vol, './data/processed_data/epi_hypo_vol_estimates_rotoehu.csv', row.names = FALSE)

