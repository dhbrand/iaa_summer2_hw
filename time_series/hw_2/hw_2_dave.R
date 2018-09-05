library(readxl)
library(lubridate)
library(tidyverse)
library(magrittr)
library(forecast)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
setwd("time_series/hw_2")

# read only the well sheet from excel
well <- read_excel("../F-179.xlsx", sheet = "Well")

well_2 <- well %>% 
  mutate(Corrected = Corrected + 1) %>% 
  group_by(date = as.yearmon(date)) %>% 
  summarise(avg = mean(Corrected))

# well_3 <- well %>% 
#   group_by(year(date), month(date)) %>% 
#   summarise(avg = mean(Corrected)) %>% 
#   unite(date, 1, 2, sep = "-") %>% 
#   mutate(date = zoo::as.yearmon(date))

# created testing set with last 6 months of data
train <- well_2 %>% 
  filter(date <= '2017-12')
test <- anti_join(well_2, train, by = "date")


# make the timeseries object
well_ts <- ts(train$avg, start = c(2007,10), frequency = 12)

# stl decomposition
decomp_stl <- stl(well_ts, s.window = 7)
plot(decomp_stl)

plot(train, col = "grey", 
     main = "Average Well Depth - Trend/Cycle", 
     xlab = "", 
     ylab = "Depth (in Feet)", 
     lwd = 2)
lines(decomp_stl$time.series[,2], 
      col = "red", 
      lwd = 2)

seas_well <- well_ts - decomp_stl$time.series[,1]
plot(well_ts, 
     col = "grey", 
     main = "Average Well Depth - Seasonally Adjusted", 
     xlab = "", 
     ylab = "Depth (in Feet)", 
     lwd = 2)
lines(seas_well, 
      col = "red", 
      lwd = 2)

monthplot(decomp_stl$time.series[,"seasonal"], 
          main = "Average Well Depth - Monthly Effects",
          ylab = "Seasonal Sub Series",
          xlab = "Seasons (Months)", 
          lwd = 2)


# Building a Single Exponential Smoothing Model - Well Data #
ses_well <- ses(well_ts, initial = "optimal", h = 6)
summary(ses_well)

plot(ses_well, 
     main = "Average Well Depth with Simple ESM Forecast", 
     xlab = "Date", 
     ylab = "Depth (in Feet)")
abline(v = 2018, 
       col = "red", 
       lty = "dashed")
round(accuracy(ses_well, 2))

autoplot(ses_well)+
  autolayer(fitted(ses_well),
            series="Fitted") + 
  ylab("Average Well Depth with Simple ESM Forecast")

# Building a Linear Exponential Smoothing Model - Well Data #
les_well <- holt(well_ts, initial = "optimal", h = 6)
summary(les_well)

plot(les_well, 
     main = "Average Well Depth with Linear ESM Forecast", 
     xlab = "Date", 
     ylab = "Depth (in Feet)")
abline(v = 2018,
       col = "red",
       lty = "dashed")

autoplot(les_well) +
  autolayer(fitted(les_well), 
            series="Fitted") +
  ylab("Average Well Depth with Holt ESM Forecast")

ldes_well <- holt(well_ts, initial = "optimal", h = 6, damped = TRUE)
summary(ldes_well)

plot(ldes_well, 
     main = "Average Well Depth with Linear Damped ESM Forecast", 
     xlab = "Date", 
     ylab = "Depth (in Feet)")
abline(v = 2018, 
       col = "red", 
       lty = "dashed")



# Building a Holt-Winters ESM - Well Data #
hwes_well_add <- hw(well_ts, seasonal = "additive", h = 6)
summary(hwes_well_add)

plot(hwes_well_add, 
     main = "Average Well Depth with Holt-Winters ESM Forecast", 
     xlab = "Date", 
     ylab = "Depth (in Feet)")
abline(v = 2018, 
       col = "red", 
       lty = "dashed")

hwes_well_mult <- hw(well_ts, seasonal = "multiplicative", h = 6)
summary(hwes_well_mult)

plot(hwes_well_mult, 
     main = "Average Well Depth with Holt-Winters ESM Forecast", 
     xlab = "Date", 
     ylab = "Depth (in Feet)")
abline(v = 2018, 
       col = "red", 
       lty = "dashed")

#####Using a holdout data set
well_ts_train <- ts(train$avg, start = c(2007,10), frequency = 12)
well_ts_test <- ts(test$avg, start = c(2018,1), frequency = 12)


# Building a Single Exponential Smoothing Model - Well Data #
ses_well_train <- ses(well_ts_train, initial = "optimal", h = 6)
summary(ses_well_train)

error_ses <- well_ts_test - ses_well_train$mean
MAE <- mean(abs(error_ses)) # 0.310
MAPE <- mean(abs(error_ses)/abs(well_ts_test)) # 0.232

# Building a Linear Exponential Smoothing Model - Well Data #
les_well_train <- holt(well_ts_train, initial = "optimal", h = 6)
summary(les_well_train)

error_les <- well_ts_test - les_well_train$mean
MAE <- mean(abs(error_les)) # 0.310
MAPE <- mean(abs(error_les)/abs(well_ts_test)) # 0.229

# Building a Linear Exponential Smoothing Model with Damped Component - Well Data #
ldes_well_train <- holt(well_ts_train, initial = "optimal", h = 6, damped = TRUE)
summary(ldes_well_train)

error_ldes <- well_ts_test - ldes_well_train$mean
MAE <- mean(abs(error_ldes)) # 0.320
MAPE <- mean(abs(error_ldes)/abs(well_ts_test)) # 0.240

# testing holt winters additive model
hwes_well_train_a <- hw(well_ts_train, seasonal = "additive", initial='optimal', h = 6)
summary(hwes_well_train_a)

error_a <- well_ts_test - hwes_well_train_a$mean
MAE <- mean(abs(error_a)) # 0.151
MAPE <- mean(abs(error_a)/abs(well_ts_test)) # 0.118

# testing holt winters multiplicative model
hwes_well_train_m <- hw(well_ts_train, seasonal = "multiplicative", initial='optimal', h = 6)
summary(hwes_well_train_m)
plot(hwes_well_train_m)

error_m <- well_ts_test - hwes_well_train_m$mean
MAE <- mean(abs(error)) # 0.116
MAPE <- mean(abs(error)/abs(well_ts_test)) # 0.09


#plot of predicted vs actual
pred_actual <- bind_cols(date = as.yearmon(time(well_ts_test)), actual = as.numeric(well_ts_test), predicted = as.numeric(hwes_well_train_m$mean))

ggplot(gather(pred_actual, act_pred, depth, -date), aes(date, depth, group = act_pred)) +
  geom_point(aes(color = act_pred), size = 4) +
  geom_line(aes(color = act_pred), size = 2) +
  theme_bw()
