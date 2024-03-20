# test the effect of window length

window_length <- 100
n_iter <- seq(1, nrow(dat) - window_length)

out <- data.frame()

for(i in 1:length(test_vars)){
  dat_ar <- dat %>% 
    ungroup() %>% 
    select(date, id_var, test_vars[i])
  
  for(j in 1:length(n_iter)){
    
    # subset to the 100 observations in the iteration
    start <- j
    end <- j + window_length
    dat_sub <- dat_ar[start:end,]
    
    # run the model
    d <- run_ar(data = dat_sub, id_var = id_var, id_covar = test_vars[i], window_length = window_length)
    d$iter_start <- start
    d$iter_end <- end
    d$start_date <- min(dat_sub$date)
    d$end_date <- max(dat_sub$date)
    d$n <- nrow(dat_sub)
    out <- rbind(out, d)
    
  }
}

## define color palettes for the right number of variables
col_no <- length(unique(out$id_covar))
col_pal <- colorRampPalette(brewer.pal(9, "Set1"))(col_no)



ggplotly(ggplot(out, aes(x = iter_start, y = r2, color = id_covar)) +
           geom_point() +
           scale_color_manual(values = col_pal) +
           geom_line() +
           theme_bw()) 

out_prop <- out %>% 
  distinct(id_covar, iter_start, .keep_all = TRUE) %>% 
  select(id_covar:iter_end, start_date, end_date, r2) %>% 
  group_by(iter_start) %>% 
  mutate(diff_from_best = max(r2) - r2,
         rank = dense_rank(desc(r2)))

ggplot(out_prop, aes(x = as.Date(start_date), y = diff_from_best, color = id_covar)) +
  geom_point() +
  scale_color_manual(values = col_pal) +
  theme_bw() +
  ylab('Difference from Best Performing Model') +
  xlab('Start of Iteration') +
  labs(color = 'Covariate') +
  theme(text=element_text(size=18))


ggplot(out_prop, aes(x = as.Date(start_date), y = as.factor(rank), color = as.factor(id_covar))) +
  geom_point() +
  facet_wrap(~id_covar) +
  theme_bw() +
  ylab('Rank') +
  xlab('Start of Iteration') +
  theme(text=element_text(size=12)) +
  scale_color_manual(values = col_pal) +
  labs(color = 'Covariate')

ggplot(out_prop, aes(x = as.factor(id_covar), y = rank, fill = as.factor(id_covar), color = as.factor(id_covar))) +
  geom_boxplot() + 
  theme_bw() +
  scale_fill_manual(values = col_pal) +
  scale_color_manual(values = col_pal) +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) 

ggplot(out_prop, aes(y = rank, fill = as.factor(id_covar), color = as.factor(id_covar))) +
  geom_histogram() + 
  theme_bw() +
  facet_wrap(~id_covar) +
  scale_fill_manual(values = col_pal) +
  scale_color_manual(values = col_pal) +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) 

out_rank <- plyr::ddply(out_prop, c("id_covar", "rank"), \(x) {
  print(unique(x$id_covar))
  print(unique(x$rank))
  n <- nrow(x)
  pct <- round(n/length(unique(out_prop$iter_start)),3)*100
  return(data.frame(pct = pct))
})


# define colors for the right number of ranks
## define color palettes for the right number of variables
num_ranks <- length(unique(out_rank$rank))
rank_pal <- colorRampPalette(brewer.pal(9, "YlGnBu"))(num_ranks)

out_rank <- out_rank %>% 
  group_by(rank) %>% 
  arrange(pct)

rank <- ggplot(out_rank, aes(x = reorder(id_covar, rank), y = pct, fill = fct_rev(as.factor(rank)))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = rank_pal) +
  theme_bw() +
  ylab('Percent of time') +
  xlab('Covariate') +
  labs(fill = 'Rank') +
  theme(text=element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) 
rank
