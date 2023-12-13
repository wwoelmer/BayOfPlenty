install.packages('astsa')
install.packages('forecast')
library(forecast)
library(astsa)
library(tidyverse)
library(plotly)

wq <- read.csv('./data/processed_data/BoP_wq_2007_2021.csv')
wq$date <- as.Date(wq$date)

chl <- wq %>% select(lake:chla_ugL_INT)
chl <- na.omit(chl)

lst <- lapply(split(chl$chla_ugL, chl$lake), acf, plot = FALSE)
par(mfrow = c(3, 4))
lapply(names(lst), function(x) plot(lst[[x]], main = x))

lst[[1]]
fit <- auto.arima(chl$chla_ugL[chl$lake=='Rotoehu'], seasonal = 'FALSE')
fit %>% forecast(h=10) %>% autoplot(include=80)
## https://stackoverflow.com/questions/35248624/print-significant-auto-correlation-value
# first rotoehu, now need to generalize

x <- chl$chla_ugL[chl$lake=='Rotoehu']
par(mfrow = c(1, 1))
pacf(x, plot = TRUE)
acf(x, plot = TRUE)
r <- pacf(x, plot = FALSE)$acf
lag_id <- which(abs(r)[-1] >= qnorm(1 - 0.05 / 2) / sqrt(length(x))) # this removes the first one
lag_id
# but what to do with so many lags? keep all of them?
# then store the output of which() in a df for each lake, then reference df in ar_model.R to create lags




dat <- wq %>% select(lake:date, NH4_mgL)
dat <- na.omit(dat)

lst <- lapply(split(dat$chla_ugL, dat$lake), acf, plot = FALSE)
par(mfrow = c(3, 4))
lapply(names(lst), function(x) plot(lst[[x]][2:30], main = x))

lst <- lapply(split(dat$chla_ugL, dat$lake), pacf, plot = FALSE)
par(mfrow = c(3, 4))
lapply(names(lst), function(x) plot(lst[[x]], main = x))

lst[[1]]
