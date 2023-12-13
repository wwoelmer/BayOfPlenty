
rf_compare_to_predictions <- function(rf, # random forest model
                                      data, # dataframe you want to predict on
                                      target, # character, variable you are predicting
                                      comparison = 'OOB', # whether you are comparing to training, testing, or out-of-bag prediction
                                      name = NULL){ # any sim name you want to identify the result with on the output figure
  # for OOB, need to leave the predict() empty
  if(comparison=='OOB'){
    df <- rf %>% 
      predict() %>% 
      bind_cols(data) %>% 
      rename(pred = ...1)
    
    metrics <- rf %>% 
      predict() %>% 
      bind_cols(data) %>% 
      rename(pred = ...1) %>% 
      metrics(truth = target, estimate = pred)
  }else{
    df <- rf %>% 
      predict(data) %>% 
      bind_cols(data) %>% 
      rename(pred = ...1)
    
    metrics <- rf %>% 
      predict(data) %>% 
      bind_cols(data) %>% 
      rename(pred = ...1) %>% 
      metrics(truth = target, estimate = pred)
  }
  
  r2 <- round(metrics$.estimate[metrics$.metric=='rsq'], 2)
  rmse <- round(metrics$.estimate[metrics$.metric=='rmse'], 2)
  
  idvar <- which(colnames(df)==target)
  
  ggplot(df, aes_string(x = target, y = "pred")) +
          geom_point() +
          geom_abline() +
          ggtitle(paste0(comparison, ' R2 = ', r2, ' RMSE = ', rmse)) +
          theme_bw()
}
