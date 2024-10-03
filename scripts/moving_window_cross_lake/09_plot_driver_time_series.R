# plot time series of drivers

dat <- read.csv('./data/moving_window_analysis_cross_lake/all_lakes_TLI_drivers.csv')

colnames(dat)

longtermtli <- dat %>% 
  group_by(lake) %>% 
  summarise(mean = mean(tli_monthly))
longtermtli

dat$lake <- factor(dat$lake, levels = c('Rotorua', 'Rotoehu', 'Rerewhakaaitu', 'Okaro', 'Okareka', 'Tarawera'))

dat <- dat %>% 
  mutate(mixing_state = ifelse(lake %in% c('Rotorua', 'Rotoehu', 'Rerewhakaaitu'), 'polymictic', 'monomictic')) %>% 
  group_by(lake) %>% 
  mutate(TLI_avg = mean(tli_monthly, na.rm = TRUE))

ggplot(dat, aes(x = as.Date(date), y = tli_monthly, color = lake)) +
  geom_point() +
  geom_line(aes(x = as.Date(date), y = tli_annual), size = 2) +
  scale_color_manual(values = c('#B22222', '#D05A22', '#ED9121', '#C9F2C7', 'seagreen','royalblue')) +
  theme_bw() +
  facet_wrap(~fct_rev(mixing_state)) +
  xlab('Date') +
  ylab('TLI') +
  theme(text = element_text(size = 16))

##################################################################################
# plots for each of the selected driver variables
a <- ggplot(dat, aes(x = year(as.Date(date)), y = DO_sat_bottom, fill = TLI_avg, group = year(as.Date(date)))) +
           geom_boxplot() +
           facet_wrap(~lake) +
           scale_fill_viridis_c(option = 'plasma') +
           theme_bw() +
  xlab('Date') +
  ylab('Bottom water DO (% Sat)')

b <- ggplot(dat, aes(x = year(as.Date(date)), y = DRP_mgm3_bottom, fill = TLI_avg, group = year(as.Date(date)))) +
  geom_boxplot() +
  facet_wrap(~lake) +
  scale_fill_viridis_c(option = 'plasma') +
  theme_bw() +
  theme(legend.position = 'none') +
  xlab('Date') +
  ylab('Bottom water DRP (ug/L)')

c <- ggplot(dat, aes(x = year(as.Date(date)), y = temp_C_top, fill = TLI_avg, group = year(as.Date(date)))) +
  geom_boxplot() +
  facet_wrap(~lake) +
  scale_fill_viridis_c(option = 'plasma') +
  theme_bw() +
  theme(legend.position = 'none') +
  xlab('Date') +
  ylab('Surface water temperature (C)')

d <- ggplot(dat, aes(x = year(as.Date(date)), y = NH4_mgm3_bottom, fill = TLI_avg, group = year(as.Date(date)))) +
  geom_boxplot() +
  facet_wrap(~lake) +
  scale_fill_viridis_c(option = 'plasma') +
  theme_bw() +
  theme(legend.position = 'none') +
  xlab('Date') +
  ylab('Bottom water NH4 (ug/L)')

e <- ggplot(dat, aes(x = year(as.Date(date)), y = NNN_mgm3_bottom, fill = TLI_avg, group = year(as.Date(date)))) +
  geom_boxplot() +
  facet_wrap(~lake) +
  scale_fill_viridis_c(option = 'plasma') +
  theme_bw() +
  theme(legend.position = 'none') +
  xlab('Date') +
  ylab('Bottom water NO3 (ug/L)')

f <- ggplot(dat, aes(x = year(as.Date(date)), y = PAR_umolm2s_top, fill = TLI_avg, group = year(as.Date(date)))) +
  geom_boxplot() +
  facet_wrap(~lake) +
  scale_fill_viridis_c(option = 'plasma') +
  theme_bw() +
  theme(legend.position = 'none') +
  xlab('Date') +
  ylab('Surface PAR (umol/m2s)')

g <- ggplot(dat, aes(x = year(as.Date(date)), y = schmidt_stability, fill = TLI_avg, group = year(as.Date(date)))) +
  geom_boxplot() +
  facet_wrap(~lake) +
  scale_fill_viridis_c(option = 'plasma') +
  theme_bw() +
  theme(legend.position = 'none') +
  xlab('Date') +
  ylab('Schmidt stability (J/m2??)')

h <- ggplot(dat, aes(x = year(as.Date(date)), y = temp_C_bottom, fill = TLI_avg, group = year(as.Date(date)))) +
  geom_boxplot() +
  facet_wrap(~lake) +
  scale_fill_viridis_c(option = 'plasma') +
  theme_bw() +
  xlab('Date') +
  ylab('Bottom water temperature (C)')

ggarrange(a, b, c, d, e, f, g, h, common.legend = TRUE, labels = 'auto')
##########################################################################################

ggplotly(ggplot(dat, aes(x = as.Date(date), y = Kd, color = lake)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lake) +
  theme_bw())

ggplotly(ggplot(dat, aes(x = as.Date(date), y = PAR_umolm2s_top, color = lake)) +
           geom_point() +
           geom_line() +
           facet_wrap(~lake) +
           theme_bw())

ggplotly(ggplot(dat, aes(x = as.Date(date), y = DRP_mgm3_bottom, color = lake)) +
           geom_point() +
           facet_wrap(~lake, scales = 'free') +
           theme_bw())



ggplotly(ggplot(dat, aes(x = as.Date(date), y = pH_top, color = 'top')) +
           geom_point() +
           geom_line() +
           geom_line(data = dat, aes(x = as.Date(date), y = pH_bottom, color = 'bottom')) +
           facet_wrap(~lake, scales = 'free') +
           theme_bw())


ggplotly(ggplot(dat, aes(x = year(as.Date(date)), y = schmidt_stability, color = lake)) +
           geom_line() +
           facet_wrap(~lake) +
           theme_bw())

ggplotly(ggplot(dat, aes(x = yday(as.Date(date)), y = thermo_depth, color = as.factor(year(as.Date(date))))) +
           geom_line() +
           facet_wrap(~lake) +
           theme_bw())



ggplot(dat, aes(x = as.Date(date), y = meta_bot, color = lake)) +
  geom_point()


ggplot(dat, aes(x = as.Date(date), y = meta_bot, color = lake)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lake)

ggplot(dat, aes(x = as.Date(date), y = hypo_temp, color = lake)) +
  geom_point()


ggplot(dat, aes(x = as.Date(date), y = hypo_temp, color = lake)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lake)

