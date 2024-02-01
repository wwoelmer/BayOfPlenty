
###############################################################################
### compare difference between aic and r2 output

library(tidyverse)
library(ggplot2)

out <- read.csv('./data/processed_data/moving_window/model_output.csv')



ggplot(out, aes(x = iter_start, y = r2, color = id_covar)) +
  geom_line() +
  geom_point(size = 2) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('Start date of iteration (+100 obs)') +
  ylab('R2') +
  scale_color_manual(values = col_pal) +
  labs(color = 'Covariate')

aicc <- ggplot(out, aes(x = iter_start, y = aic, color = id_covar)) +
  geom_line() +
  geom_point(size = 2) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('Start date of iteration (+100 obs)') +
  ylab('AICc') +
  scale_color_manual(values = col_pal) +
  labs(color = 'Covariate')

aic_r2_compare <- ggplot(out, aes(x = aic, y = r2, color = id_covar)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('AICc') +
  ylab('R2') +
  scale_color_manual(values = col_pal) +
  labs(color = 'Covariate')

combined_fig <- ggarrange(aicc, aic_r2_compare, common.legend = TRUE, labels = 'AUTO')
combined_fig
ggsave('./figures/moving_window/aic_r2_comparison.png', combined_fig,
       dpi = 300, units = 'mm', height = 300, width = 700, scale = 0.5)
