# bootstrap confidence intervals on TLI estimates

#install.packages('boot')
library(boot)
library(tidyverse)

mean.function <- function(x, index) {
  d <- x[index]     # This first line will go in ever bootstrap function you make.
  return(mean(d))  
}

df <- read.csv('./data/processed_data/90s_data/rotoehu_tli_drivers_1990_2021.csv')

df_90 <- df %>% 
  filter(decade==90)

n_sampled <- df %>% 
  group_by(hydroyear) %>% 
  summarise(n = n())

df_00_21 <- df %>% 
  filter(hydroyear > 2000)

tli <- df %>% select(hydroyear, tli_annual) %>% distinct(hydroyear, .keep_all = TRUE)


#########################################################################
# manual bootstrapping based on number of times sampled
out <- data.frame(hydroyear = NULL,
                  mean_btstrp = NULL,
                  mean_obs = NULL,
                  subsample_n = NULL)
n_iter <- 500
years <- unique(df_00_21$hydroyear)
n <- unique(n_sampled$n)

# estimate means
for(i in 1:n_iter){
  for(j in 2:length(years)){
    for(k in 1:length(n)){
      df.x <- df_00_21 %>% 
        filter(hydroyear==years[j])
      
      
      if(length(df.x$tli_monthly) >= n[k]){
        n_samp <- n[k]}else{
          n_samp <- length(df.x$tli_monthly)
        }
      mean_btstrp <- mean(sample(df.x$tli_monthly, n_samp))
      mean_obs <- mean(df.x$tli_annual)
      out.x <- data.frame(hydroyear = years[j],
                          mean_btstrp = mean_btstrp,
                          mean_obs = mean_obs,
                          subsample_n = n_samp)
      out <- rbind(out.x, out)  
      
    }
  }
}

out <- na.omit(out)
out <- out %>% 
  mutate(diff = mean_obs - mean_btstrp)

out_sum <- out %>% 
  group_by(hydroyear, subsample_n) %>% 
  mutate(mean_btstrp_all = mean(mean_btstrp)) %>% 
  distinct(hydroyear, .keep_all = TRUE)

ggplot(out, aes(x = mean_obs, y = mean_btstrp, color = as.factor(hydroyear))) +
  geom_point() +
  facet_wrap(~subsample_n) +
  geom_abline() 

ggplot() +
geom_point(data = out_sum, aes(x = mean_obs, y = mean_btstrp_all, color = as.factor(hydroyear))) +
  geom_abline()


ggplot(out, aes(x = diff, fill = as.factor(subsample_n))) +
  geom_histogram()

mean(out$diff)
sd(out$diff)
quantile(out$mean_btstrp, probs = c(0.25, 0.975))

# calculate 95% confidence interval
CI <- NULL
for(i in 1:length(years)){
  for(k in 1:length(n)){
    df.2 <- out %>% 
      filter(hydroyear==years[i],
             subsample_n==n[k])
    x <- quantile(df.2$mean_btstrp, probs=c(.025, .975) )
    CI_out <- data.frame(hydroyear = years[i],
                         upper = round(as.vector(x[1]), 2),
                         lower = round(as.vector(x[2]), 2),
                         subsample_n = n[k])
    CI <- rbind(CI, CI_out)
    
  }
  
}
CI

CI <- left_join(CI, tli, by = 'hydroyear')

# calculate the percentage of overall TLI which CI takes up
CI <- CI %>% 
  group_by(hydroyear, subsample_n) %>% 
  mutate(range = abs(upper - lower),
         pct_range = range/tli_annual*100)
mean_CI <- CI %>% 
  group_by(subsample_n) %>% 
  summarise(mean_pct_range = round(mean(pct_range, na.rm = TRUE), 1))

ggplot(CI, aes(x = hydroyear, y = tli_annual)) +
  geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper, alpha = 0.5)) +
  theme_bw() +
  facet_wrap(~subsample_n) +
  ggtitle('Effect of increased subsampling on confidence interval around TLI in Rotoehu') +
  theme(legend.position = "none")
  
ggplot(CI, aes(x = hydroyear, y = pct_range)) +
  geom_point() +
  geom_line() +
  geom_hline(data = mean_CI, aes(yintercept = mean_pct_range, color = 'Overall Mean')) +
  facet_wrap(~subsample_n) +
  theme_bw()

ggplot(CI, aes(x = hydroyear, y = pct_range, color = as.factor(subsample_n))) +
  geom_point() +
  geom_line() +
  geom_hline(data = mean_CI, aes(yintercept = mean_pct_range, color = as.factor(subsample_n))) +
  theme_bw()


### apply percent CI ranges to 90's dataset based on number of times sampled
df_90_adj <- df_90 %>% 
  select(date, hydroyear, month, tli_annual, tli_monthly) %>% 
  group_by(hydroyear) %>% 
  mutate(n = n()) %>% 
  distinct(hydroyear, .keep_all = TRUE) %>% 
  select(-tli_monthly)

df_90_adj$tli_annual <- round(df_90_adj$tli_annual, 1)
df_90_adj$tli_annual_upper <- NA
df_90_adj$tli_annual_lower <- NA

for(i in 1:nrow(df_90_adj)){
  idx <- df_90_adj$n[i]
  pct <- mean_CI$mean_pct_range[mean_CI$subsample_n==idx]/100/2
  
  df_90_adj$tli_annual_upper[i] <- round(df_90_adj$tli_annual[i] + df_90_adj$tli_annual[i]*pct, 1)
  df_90_adj$tli_annual_lower[i] <- round(df_90_adj$tli_annual[i] - df_90_adj$tli_annual[i]*pct, 1)
  
}

df_90_adj <- na.omit(df_90_adj)

p1 <- ggplot(df_90_adj, aes(x = hydroyear, y = tli_annual)) +
  geom_point() +
  geom_ribbon(aes(ymin = tli_annual_lower, ymax = tli_annual_upper, alpha = 0.5)) +
  theme_bw() +
  scale_x_continuous(breaks = c(unique(df_90_adj$hydroyear))) +
  theme(legend.position = 'NONE') +
  geom_hline(aes(yintercept = 3.9)) +
  ylab('Annual TLI')+
  xlab('Hydroyear')
p1
ggsave('./figures/uncertainty_90s_tli.png', p1, dpi = 300, units = 'mm', 
       height = 200, width = 400, scale = 0.5)

