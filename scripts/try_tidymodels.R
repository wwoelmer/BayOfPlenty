#install.packages('tidymodels')
library(tidymodels)



lake_split <- initial_split(dat1, prop = 0.75)

lake_training <- lake_split %>% 
  training()

lake_testing <- lake_split %>% 
  testing()

lake_rf <-  rand_forest(trees = 1000, mode = "regression") %>%
  set_engine("randomForest") %>%
  fit(chla_ugL_INT ~ ., data = lake_training)

imp <- lake_rf$fit$importance
lake_rf$fit$
imp <- as.data.frame(imp)
imp$variable <- rownames(imp) # row names to column
rownames(imp) <- NULL  

## assign categories
cat <- read.csv('./data/driver_categories.csv', sep = ';')

df_long <- left_join(imp, cat)

df_long %>% 
  filter(IncNodePurity > 1) %>% 
  ggplot(aes(x=reorder(variable, IncNodePurity), weight=IncNodePurity, fill=as.factor(category))) + 
  geom_bar() +
  scale_fill_discrete(name="Variable Group") +
  ylab("IncNodePurity") +
  xlab("Variable Name") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#install.packages('vip')
library(vip)
lake_rf %>% 
  vip()

df <- lake_rf %>% 
  predict(lake_testing) %>% 
  bind_cols(lake_testing) 

metrics <- lake_rf %>% 
  predict(lake_testing) %>% 
  bind_cols(lake_testing) %>% 
  metrics(truth = chla_ugL_INT, estimate = .pred)

r2 <- round(metrics$.estimate[metrics$.metric=='rsq'], 2)
rmse <- round(metrics$.estimate[metrics$.metric=='rmse'], 2)

ggplot(df, aes(x = chla_ugL_INT, y = .pred)) +
  geom_point() +
  geom_abline() +
  ggtitle(paste0('Test R2 = ', r2, ' RMSE = ', rmse)) +
  theme_bw()

##################################################################################
rf <- model
data = lake_training
target = "TP_mgm3"
name = 'Train'



lake_rf <-  rand_forest(trees = 1000, mode = "regression", mtry = 45/3) %>%
  set_engine("randomForest") %>%
  fit(TP_mgm3 ~ ., data = lake_training)

lake_rf %>% 
  vip()

rf_figure_output(rf = lake_rf, data = lake_training, target = "TP_mgm3", name = 'Rotoehu TP Train')
rf_figure_output(rf = lake_rf, data = lake_testing, target = "TP_mgm3", name = 'Rotoehu TP Test')
