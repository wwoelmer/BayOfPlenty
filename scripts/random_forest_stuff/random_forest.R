# try to run the random forest
#install.packages('randomForest')
library(randomForest)

dat <- read.csv('./data/master.csv')

ggplot(dat, aes(x = as.Date(date), y = chla_ugL_INT)) +
  geom_line()

# reorganize data
dat <- dat %>% 
  select(chla_ugL_INT, everything(), -lake, -site, -laketype, -date) 

# remove cols with lots of zeroes
dat <- dat %>% 
  select(-secchi_m, -TN_mgm3, -lake_num)

dat <- na.omit(dat)

# keep only land cover percentages, not area
dat <- dat %>% 
  select(-c("area_ha_aq_vegetation", "area_ha_decid_hardwood", "area_ha_exotic_forest",
            "area_ha_forest_harvested", "area_ha_gorse", "area_ha_hp_exotic_grassland" ,
            "area_ha_manuka",  "area_ha_native_forest", "area_ha_native_hardwood",
            "area_ha_settlement", "area_ha_water"))

n_train <- 0.75*nrow(dat)
train <- dat[1:n_train,]

n_test <- nrow(train):nrow(dat)
test <- dat[nrow(train):nrow(dat),]

# run the model
model <- randomForest(formula = chla_ugL_INT ~ .,
                   data = train, importance = TRUE,
                   ntree = 1000)

# some diagnostics
which.min(model$mse)
sqrt(model$mse[which.min(model$mse)]) 
model$rsq[which.max(model$rsq)]
plot(model)
varImpPlot(model) 

imp <- varImpPlot(model) 
imp <- as.data.frame(imp)
imp$variable <- rownames(imp) # row names to column
rownames(imp) <- NULL  

## assign categories
cat <- read.csv('./data/driver_categories.csv', sep = ';')

df_long <- left_join(imp, cat)

###############################################
r2 <- round(model$rsq[which.max(model$rsq)], 2)
rmse <- round(sqrt(model$mse[which.min(model$mse)]), 2)

a <- df_long %>% 
  filter(IncNodePurity > 1) %>% 
  ggplot(aes(x=reorder(variable, IncNodePurity), weight=IncNodePurity, fill=as.factor(category))) + 
  geom_bar() +
  scale_fill_discrete(name="Variable Group") +
  ylab("IncNodePurity") +
  xlab("Variable Name") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

b <- df_long %>% 
  filter(IncNodePurity > 1) %>% 
  ggplot(aes(x=reorder(variable, `%IncMSE`), weight=`%IncMSE`, fill=as.factor(category))) + 
  geom_bar() +
  scale_fill_discrete(name="Variable Group") +
  ylab("% Increase MSE") +
  xlab("Variable Name") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

library(ggpubr)
fig <- ggarrange(a, b, common.legend = TRUE)
annotate_figure(fig, top = text_grob(paste0("R2 = ", r2, " RMSE = ", rmse)))

# predict using the model
pred <- predict(model, newdata=test)

ggplot(data = dat, aes(x = as.Date(date), y = chla_ugL_INT, color = 'obs')) +
  geom_point() +
  geom_line(aes(x = as.Date(date), y = pred, color = 'RF'))

plot(test$chla_ugL_INT, pred)
abline(0, 1)
