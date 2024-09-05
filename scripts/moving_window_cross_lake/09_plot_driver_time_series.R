# plot time series of drivers

dat <- read.csv('./data/moving_window_analysis_cross_lake/all_lakes_TLI_drivers.csv')

colnames(dat)

longtermtli <- dat %>% 
  group_by(lake) %>% 
  summarise(mean = mean(tli_monthly))
longtermtli

dat$lake <- factor(dat$lake, levels = c('Rotorua', 'Rotoehu', 'Rerewhakaaitu', 'Okaro', 'Okareka', 'Tarawera'))

dat <- dat %>% 
  mutate(mixing_state = ifelse(lake %in% c('Rotorua', 'Rotoehu', 'Rerewhakaaitu'), 'polymictic', 'monomictic'))

ggplot(dat, aes(x = as.Date(date), y = tli_monthly, color = lake)) +
  geom_point() +
  geom_line(aes(x = as.Date(date), y = tli_annual), size = 2) +
  scale_color_manual(values = c('#B22222', '#D05A22', '#ED9121', '#C9F2C7', 'seagreen','royalblue')) +
  theme_bw() +
  facet_wrap(~fct_rev(mixing_state)) +
  xlab('Date') +
  ylab('TLI') +
  theme(text = element_text(size = 16))

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
           geom_boxplot() +
           facet_wrap(~lake) +
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


ggplot(dat, aes(x = as.Date(date), y = lake_num, color = lake)) +
  geom_point()


ggplot(dat, aes(x = as.Date(date), y = lake_num, color = lake)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lake)
