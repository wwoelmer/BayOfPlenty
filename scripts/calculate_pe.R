# calculate PE on each component of the TLI (annual and monthly timescales)
install.packages('ggpmisc')
install.packages('statcomp')

library(tidyverse)
library(ggpmisc)
library(statcomp)

dat <- read.csv('./data/master_rotoehu.csv')

dat <- dat %>% 
  select(date, year, month, chla_ugL_INT, top_TN_ugL, top_TP_ugL, secchi_m)

dat_long <- dat %>% 
  pivot_longer(chla_ugL_INT:secchi_m, names_to = 'variable', values_to = 'value')

# determine the embedding dimension
# example
t <- dat_long %>% 
  filter(variable=='chla_ugL_INT', year==2000)
opd = ordinal_pattern_distribution(x = t$value, ndemb = 6)
permutation_entropy(opd)

t <- dat_long %>% 
  filter(year > 2000 & year < 2010)

df <- matrix(ncol = 4, nrow = 10)
colnames(df) <- c('year', 'variable', 'PE', 'demb')
out <- plyr::ddply(t, c("year", "variable"), \(x) {
  print(head(x))
  data = x$value
  pe <- sapply(3:10, \(i) {
    opd = ordinal_pattern_distribution(x = data, ndemb = i)
    permutation_entropy(opd)
  })
  data.frame(ndemb = 3:10, pe = pe)
})
out 

ggplot(out, aes(x = ndemb, y = pe, color = as.factor(year))) +
  geom_line() +
  facet_wrap(~variable) +
  theme_bw() +
  xlab('Embedding Distance') +
  ylab('Permutation Entropy') +
  labs(color = 'Year')

# go with 4?

PE <- plyr::ddply(dat_long, c("year", "variable"), \(x) {
  print(head(x))
  data = x$value
  opd = weighted_ordinal_pattern_distribution(x = data, ndemb = 4)
  pe <- permutation_entropy(opd)
  data.frame(ndemb = 4, pe = pe, n = nrow(x))
})
PE

# only keep years with observations in most months
PE <- PE %>% filter(n > 6)

ggplot(PE, aes(x = as.factor(year), y = pe, color = as.factor(year))) +
  geom_point(size = 3) +
  geom_line(aes(group = as.factor(variable))) +
  facet_wrap(~variable) +
  theme_bw() +
  xlab('Year') +
  ylab('Permutation Entropy') +
  labs(color = 'Variable') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

ggplot(PE, aes(x = as.factor(year), y = pe, color = as.factor(variable))) +
  geom_point(size = 3) +
  geom_line(aes(group = as.factor(variable))) +
  theme_bw() +
  xlab('Year') +
  ylab('Permutation Entropy') +
  labs(color = 'Variable') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

mean_annual <- PE %>% 
  group_by(variable) %>% 
  mutate(mean = mean(pe),
         min = min(pe),
         max = max(pe), 
         median = median(pe)) %>% 
  distinct(variable, .keep_all = TRUE)
mean_annual

PE_wide <- PE %>% 
  pivot_wider(names_from = 'variable', values_from = 'pe')

ggplot(PE_wide, aes(x = chla_ugL_INT, y = top_TP_ugL)) +
  geom_point() 

ggplot(PE_wide, aes(x = chla_ugL_INT, y = top_TN_ugL)) +
  geom_point() 

ggplot(PE_wide, aes(x = chla_ugL_INT, y = secchi_m)) +
  geom_point() 

ggplot(PE_wide, aes(x = top_TN_ugL, y = top_TP_ugL)) +
  geom_point() 

ggplot(PE_wide, aes(x = top_TN_ugL, y = secchi_m)) +
  geom_point() 

write.csv(PE, './data/processed_data/PE_tli_vars.csv', row.names = FALSE)
