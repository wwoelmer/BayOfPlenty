# global AR model

#install.packages('MuMIn')
library(MuMIn)
library(tidyverse)

# read in data
#dat <- read.csv('./data/processed_data/BoP_wq_2007_2021.csv')
dat <- read.csv('./data/master_rotoehu.csv')
dat <- dat %>% select(-lake_num) %>% 
  mutate(chl_log = log10(chla_ugL_INT))
# what variable are you trying to predict
id_var <- "chl_log"

# some cleaning
dat <- dat %>% 
#  select(- laketype, - method) %>% 
  filter(lake=='Rotoehu')

ggplot(dat, aes(x = as.Date(date), y = chla_ugL_INT)) +
  geom_line()

ggplot(dat, aes(x = as.Date(date), y = chl_log)) +
  geom_line()

# create lag as determined by PACF
#### NEED TO AUTOMATE STILL #####
# if the needed lag is not one, then add it here
dat <- dat %>% 
  mutate(chla_lag_4 = lag(chl_log, n = 4),
         chla_lag_3 = lag(chl_log, n = 3),
         chla_lag_2 = lag(chl_log, n = 2),
         chla_lag_1 = lag(chl_log, n = 1))
dat <- dat %>% 
  filter(!is.na(chla_ugL_INT)) 

# remove cols with lots of zeroes
dat <- dat %>% 
  select(-c(epi_temp, epi_dens, hypo_temp, hypo_dens, meta_top, meta_bot, uStar))

# keep only land cover percentages, not area
dat <- dat %>% 
  select(-c("area_ha_aq_vegetation", "area_ha_decid_hardwood", "area_ha_exotic_forest",
            "area_ha_forest_harvested", "area_ha_gorse", "area_ha_hp_exotic_grassland" ,
            "area_ha_manuka",  "area_ha_native_forest", "area_ha_native_hardwood",
            "area_ha_settlement", "area_ha_water"))

# remove some variables which are correlated with each other or have very low correlation with chl INT
dat <- dat %>% 
  select(-c(lake, site, year, month, chla_ugL_INT, method))
    
    
dat <- na.omit(dat)

cor_table <- round(cor(dat[,2:ncol(dat)]), 2)
write.csv(cor_table, './data/raw_data/correlation_table_drivers.csv', row.names = FALSE)




colnames(dat_clean)

# create train and test set
test <- dat_clean %>% 
  filter(date > as.Date('2016-12-31'))

train <- dat_clean %>% 
  filter(date < as.Date('2016-12-31'))

# set names of variables to go into model
front <- c("lake", "site", "date", "depth_m", "method", "laketype", "year", "month")
lag_var <- 'chla_lag_1'
`%notin%` <- Negate(`%in%`)

drivers <- colnames(train)[colnames(train)%notin%front & colnames(train)!=id_var]
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

for (i in 1:nrow(select_global)) {
  drivers_selected <- names(select_global[i])[!is.na(select_global[i]) & names(select_global[i])%notin%extras]

  fml_selected <- as.formula(paste0(id_var, " ~ ", paste(drivers_selected, collapse= "+")))
  m1 <- lm(fml_selected, data = train)
  t <- summary(m1)

  pred_train <- predict(m1, newdata = train)
  pred <- predict(m1, newdata = test)
  
  a <- ggplot() +
    geom_point(data = train, aes(x = as.Date(date), y = chla_ugL_INT)) +
    geom_line(data = train, aes(x = as.Date(date), y = pred_train, color = 'train')) +
    geom_line(data = test, aes(x = as.Date(date), y = pred, color = 'test')) +
    geom_point(data = test, aes(x = as.Date(date), y = chla_ugL_INT)) +
    ggtitle(fml_selected) +
    theme(plot.title = element_text(size = 8, face = "bold"))
  
  b <- ggplot(train, aes(x = chla_ugL_INT, y = pred_train, color = 'train')) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(data = test, aes(x = chla_ugL_INT, y = pred, color = 'test')) +
    ggtitle(paste0("model ", i, " r-squared (train): ", round(t$adj.r.squared, 2)))
  
  c <- ggplot(m1, aes(x = m1$residuals)) +
    geom_histogram()
  print(a+b+c)
}

drivers_selected <- names(select_global[1])[!is.na(select_global[1]) & names(select_global[1])%notin%extras]
drivers_selected

fml_selected <- as.formula(paste0(id_var, " ~ ", paste(drivers_selected, collapse= "+")))
fml_selected

m1 <- lm(fml_selected, data = train)
summary(m1)

pred_train <- predict(m1, newdata = train)
for (i in 1:length(pred_train)) {
  if(pred_train[i] < 0){
    pred_train[i] <- 0
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
