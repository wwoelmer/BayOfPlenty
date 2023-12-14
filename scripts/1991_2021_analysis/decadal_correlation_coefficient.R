# calculate correlation coefficients by decade

library(tidyverse)
#install.packages('Hmisc')
library(Hmisc)

df <- read.csv('./data/processed_data/90s_data/rotoehu_tli_drivers_1990_2021.csv')

df_long <- df %>% 
  select(-soi_phase, -year, -temp_0) %>%
  filter(decade <2020) %>% 
  pivot_longer(air_temp_min:NH4_mgm3, names_to = 'variable', values_to = 'value')

ggplot(df_long, aes(x = as.factor(decade), y = value, fill = as.factor(decade))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = 'free') +
  theme_bw() +
  xlab('Decade') +
  labs(fill = 'Decade')

df_summary <- df_long %>% 
  select(variable, value, decade) %>% 
  group_by(variable, decade) %>% 
  reframe(mean = round(mean(value, na.rm = TRUE), 2), min = min(value, na.rm = TRUE), max= max(value, na.rm = TRUE), range = min - max)

#################################################################################
# plot correlations

decades <- c('90', '2000', '2010', 'all')
vars_out <- data.frame(variable = NA,
                       value = NA,
                       decade = NA)

for(i in 1:length(decades)){
  if(decades[i]!='all'){
    cor_df <- df %>% 
      filter(decade==decades[i]) %>% 
      select(-c(date, hydroyear, hydroyear_label, month, tli_annual, decade, 
                year, soi_phase, TP_mgm3, TN_mgm3, chl_mgm3, secchi_m, temp_0))
  }else{
    cor_df <- df %>% 
      select(-c(date, hydroyear, hydroyear_label, month, tli_annual, decade, 
                year, soi_phase, TP_mgm3, TN_mgm3, chl_mgm3, secchi_m, temp_0))
  }
  print(decades[i])
  print(nrow(cor_df))
  
  cor_df <- na.omit(cor_df)
  cor_out <- rcorr(as.matrix(cor_df)) # default is pearson
  try(corrplot::corrplot(cor_out$r, type = 'upper',
                         sig.level = 0.01, insig = 'blank', p.mat = cor_out$P,  
                         main = paste0('Decade: ', decades[i], "'s")), silent = TRUE)
  
  vars <- cor_out$r[cor_out$P > 0.05]
  
  
  r <- as.data.frame(cor_out$r)[1,]
  p <- as.data.frame(cor_out$P)[1,]
  r_sub <- r
  for(j in 2:ncol(r_sub)){
    if(p[,j]>=0.05){
      r_sub[,j] <- NA
    }
  }
  
  r_long <- r_sub %>% 
    pivot_longer(tli_monthly:NH4_mgm3, names_to = 'variable', values_to = 'value')
  r_long$decade <- decades[i]
  vars_out <- rbind(vars_out, r_long)
  
  write.csv(cor_out$r, paste0('./data/processed_data/90s_data/correlation_matrix_Rotoehu_', decades[i], 's.csv'), row.names = FALSE)
  
}

vars_out <- na.omit(vars_out)
vars_out <- vars_out %>% 
  filter(variable!='tli_monthly')
vars_out$decade <- factor(vars_out$decade, levels = c('90', '2000', '2010', 'all'))


library(RColorBrewer)
col_pal <- colorRampPalette(brewer.pal(9, "Paired"))(13)

p1 <- vars_out %>% 
  filter(value > 0.3 | value < -0.3) %>% 
  ggplot(aes(x = decade, y = value, fill = variable)) +
  geom_col(position = 'dodge') +
  scale_fill_manual(values = col_pal) +
  geom_vline(xintercept = 1.5) +
  geom_vline(xintercept = 2.5) +
  geom_vline(xintercept = 3.5) +
  theme_bw() +
  ylab('Correlation Coefficient') 
p1
ggsave('./figures/r_by_decade.png', p1, dpi = 300, units = 'mm', height = 300, width = 500, scale = 0.5)

################################################################################
ggplot(df, aes(x = as.Date(date), y = longwave_mean, color = as.factor(decade))) +
  geom_point() +
  geom_line() +
  theme_bw()