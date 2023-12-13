#install.packages('pdp')
#install.packages('egg')
library(egg)
library(pdp)
library(randomForest)
library(tidyverse)
library(ggpubr)
library(tidymodels)

rf_diagnostic <- function(data, # data = dataframe in format for random forest
                          target, # target = the variable you want to predict, character
                          train = .75, # train = numeric, decimal, the proportion of the dataset you want to set aside for training
                          mtry = ncol(data)/3, # the number of predictor variables randomly chosen for each split
                          ntree = 1000, # the number of trees to grow
                          lake_name = NULL # character, an identifier for the model you are running (e.g., Rotoehu TP)
                          ){
  # set up sim name
  name = paste0(lake_name, " ", target)
  
  # set up train and test datasets if true
  lake_split <- initial_split(data, prop = train)
  
  lake_training <- lake_split %>% 
    training()
  
  lake_testing <- lake_split %>% 
    testing()
  
  fml <- as.formula(paste0(target, " ~ ."))

  # run the model
  model <- randomForest(formula = fml,
                        data = lake_training, 
                        importance = TRUE,
                        ntree = ntree,
                        mtry= mtry)
  

  # store model output
  imp <- importance(model)
  impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
  impvar <- impvar[1:6]
  
  imp <- as.data.frame(imp)
  imp$variable <- rownames(imp) # row names to column
  rownames(imp) <- NULL  
  
  
  p1 <- pdp::partial(model, pred.var = impvar[1]) %>% 
    plotPartial(ylab = target)
  p2 <- pdp::partial(model, pred.var = impvar[2]) %>% 
    plotPartial(ylab = target)
  p3 <- pdp::partial(model, pred.var = impvar[3]) %>% 
    plotPartial(ylab = target)
  p4 <- pdp::partial(model, pred.var = impvar[4]) %>% 
    plotPartial(ylab = target)
  p5 <- pdp::partial(model, pred.var = impvar[5]) %>% 
    plotPartial(ylab = target)
  p6 <- pdp::partial(model, pred.var = impvar[6]) %>% 
    plotPartial(ylab = target)
  
  p_all <- arrangeGrob(p1, p2, p3, p4, p5, p6, ncol = 3)
  fname <- paste0('./figures/', lake_name, "/", lake_name, '_', target, '_partial_dependence_',  '.jpg')
  ggsave(fname, p_all)
  

  ## assign categories and colors
  cat <- read.csv('./data/driver_categories.csv', sep = ';')
  cat$category <- factor(cat$category, c("mixing", "physicochemical", "chemical", "biological",
                            "meteorological", "land_cover", "dosing", "time"), ordered = TRUE)
  ncolors <- length(unique(cat$category))
  cols <- RColorBrewer::brewer.pal(ncolors, "Set3")
  cat_cols <- data.frame("colors" = cols, "category" = levels(cat$category))

  df_long <- left_join(imp, cat)
  
  # visualize
  r2 <- round(model$rsq[which.max(model$rsq)], 2)
  rmse <- round(sqrt(model$mse[which.min(model$mse)]), 2)
  
  df_long <- df_long[order(df_long$`%IncMSE`, decreasing = TRUE),]
  df_long$category <- factor(df_long$category, c("mixing", "physicochemical", "chemical", "biological",
                                         "meteorological", "land_cover", "dosing", "time"), ordered = TRUE)
  # choose the top twenty variables
  vars <- df_long[1:20,]
  
  a <- vars %>% 
    ggplot(aes(x=reorder(variable, IncNodePurity), weight=IncNodePurity, fill=as.factor(category))) + 
    geom_bar() +
    coord_flip() +
    scale_fill_manual(values = cat_cols$colors, name = 'Variable Group') +
    ylab("IncNodePurity") +
    xlab("Variable Name") +
    theme_bw()
  a
  b <- vars %>% 
    ggplot(aes(x=reorder(variable, `%IncMSE`), weight=`%IncMSE`, fill=as.factor(category))) + 
    geom_bar() +
    coord_flip() +
    scale_fill_manual(values = cat_cols$colors, name = 'Variable Group') +
    ylab("% Increase MSE") +
    xlab("Variable Name") +
    theme_bw()
  b
  ## add 1:1 plots of predictions from test dataset and OOB from fit
  source('./scripts/R/rf_compare_to_predictions.R')
  
  oob_p <- rf_compare_to_predictions(rf = model, target = target, data = lake_training, comparison = 'OOB') 
  test_p <- rf_compare_to_predictions(rf = model, data = lake_testing, target = target, comparison = 'test')

  # make final plot
  fig <- ggpubr::ggarrange(a, b, oob_p, test_p, common.legend = TRUE, heights = c(0.7, 0.7))
  fig <- annotate_figure(fig, top = text_grob(paste0(name, " ", " Results"), size =22))

  ggsave(paste0('./figures/', lake_name, '/', lake_name, "_", target, '_RF_results.jpeg'), fig, 
         dpi = 300, units = 'mm', height = 600, width = 600, scale = 0.5)
 
   
}


