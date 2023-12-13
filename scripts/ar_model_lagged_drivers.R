# global AR model

#install.packages('MuMIn')
library(MuMIn)
library(tidyverse)

# read in data
dat <- read.csv('./data/processed_data/BoP_wq_2007_2021.csv')

# what variable are you trying to predict
id_var <- "chla_ugL_INT"

# define target variable and set everything else back one month
dat <- dat %>% 
  mutate(target = lead(chla_ugL_INT, n = 1L))

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
drivers <- colnames(train)[colnames(train)!=front & colnames(train)!='target']
drivers

# create formula
fml <- as.formula(paste0('target', " ~ ", paste(drivers, collapse= "+")))
fml

global_model <- glm(fml, data = train, family = gaussian, na.action = 'na.fail')

glm_global <- dredge(global_model, rank = "BIC", fixed = lag_var)  
select_global <- subset(glm_global, delta<2 )

summary(select_global)

# select one model and test it
select_global[1]
m1 <- lm(target ~ chla_lag_4 + pH + TP_mgm3, data = train)
summary(m1)

pred <- predict(m1, newdata = test)

ggplot(test, aes(x = as.Date(date), y = chla_ugL_INT)) +
  geom_point() +
  geom_line(aes(x = as.Date(date), y = pred, color = 'predicted'))

ggplot(test, aes(x = chla_ugL_INT, y = pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)
