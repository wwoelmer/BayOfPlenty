# global AR model

#install.packages('MuMIn')
library(MuMIn)
library(tidyverse)

# read in data
dat <- read.csv('./data/processed_data/BoP_wq_2007_2021.csv')
#dat <- read.csv('./data/master.csv')
dat <- na.omit(dat)

# what variable are you trying to predict
id_var <- "chla_ugL_INT"

# some cleaning
dat <- dat %>% 
  select(- laketype, - method) %>% 
  filter(lake=='Rotoehu')

ggplot(dat, aes(x = as.Date(date), y = chla_ugL_INT)) +
  geom_line()

# create lag as determined by PACF
#### NEED TO AUTOMATE STILL #####
# if the needed lag is not one, then add it here
dat <- dat %>% 
  mutate(chla_lag_4 = lag(chla_ugL_INT, n = 4))
dat <- na.omit(dat)


# create train and test set
test <- dat %>% 
  filter(date > as.Date('2019-12-31'))

train <- dat %>% 
  filter(date < as.Date('2019-12-31'))

# set names of variables to go into model
front <- c("lake", "site", "date", "depth_m", "method", "laketype")
lag_var <- 'chla_lag_4'
drivers <- colnames(train)[colnames(train)!=front & colnames(train)!=id_var]
drivers

# create formula
fml <- as.formula(paste0(id_var, " ~ ", paste(drivers, collapse= "+")))
fml

global_model <- glm(fml, data = train, family = gaussian, na.action = 'na.fail')

glm_global <- dredge(global_model, rank = "BIC", fixed = lag_var)  
select_global <- subset(glm_global, delta<2 )

summary(select_global)

# select one model and test it
extras <- c("df", "logLik", "BIC", "delta", "weight", "(Intercept)")
`%notin%` <- Negate(`%in%`)
drivers_selected <- names(select_global[1])[names(select_global[1])%notin%extras]
drivers_selected

fml_selected <- as.formula(paste0(id_var, " ~ ", paste(drivers_selected, collapse= "+")))
fml_selected

m1 <- lm(fml_selected, data = train)
summary(m1)

pred_train <- predict(m1, newdata = train)
for (i in 1:length(pred_train)) {
  if(pred[i] < 0){
    pred[i] <- 0
  }
}
ggplot(train, aes(x = as.Date(date), y = chla_ugL_INT, color = lake)) +
  geom_point() +
  geom_line(aes(x = as.Date(date), y = pred_train, color = 'predicted train'))

ggplot(train, aes(x = chla_ugL_INT, y = pred_train)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

plot(m1)
hist(m1$residuals)

pred <- predict(m1, newdata = test)
for (i in 1:length(pred)) {
  if(pred[i] < 0){
    pred[i] <- 0
  }
}

ggplot(test, aes(x = as.Date(date), y = chla_ugL_INT, color = lake)) +
  geom_point() +
  ylim(0, 100) +
  xlim(min(as.Date(train$date)), max(as.Date(test$date))) +
  facet_wrap(~lake, scales = 'free') +
  geom_line(aes(x = as.Date(date), y = pred, color = 'predicted'))

ggplot() +
  geom_point(data = train, aes(x = as.Date(date), y = chla_ugL_INT)) +
  geom_line(data = train, aes(x = as.Date(date), y = pred_train, color = 'train')) +
  geom_line(data = test, aes(x = as.Date(date), y = pred, color = 'test')) +
  geom_point(data = test, aes(x = as.Date(date), y = chla_ugL_INT)) 

ggplot(test, aes(x = chla_ugL_INT, y = pred, color = lake)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

ggplot(train, aes(x = chla_ugL_INT, y = pred_train, color = 'train')) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(data = test, aes(x = chla_ugL_INT, y = pred, color = 'test'))
