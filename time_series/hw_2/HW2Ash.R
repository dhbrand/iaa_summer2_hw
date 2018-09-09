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
library(ggplot2)
setwd("time_series/hw_2")

# read only the well sheet from excel
well <- read_excel("../F-179.xlsx", sheet = "Well")

well_2 <- well %>% 
  mutate(Corrected = Corrected + 1) %>% 
  group_by(date = as.yearmon(date)) %>% 
  summarise(avg = mean(Corrected))

view(well_2)
# well_3 <- well %>% 
#   group_by(year(date), month(date)) %>% 
#   summarise(avg = mean(Corrected)) %>% 
#   unite(date, 1, 2, sep = "-") %>% 
#   mutate(date = zoo::as.yearmon(date))

# created testing set with last 6 months of data
train <- well_2 %>% 
  filter(date < 'Jan 2018')
test <- well_2 %>%
  filter(date >= 'Jan 2018')


# make the timeseries object
well_ts <- ts(train$avg, start = c(2007,10), frequency = 12)

#STL Decomposition
model <- stl(well_ts,s.window=7)
plot(model)

plot(well_ts,col="dark gray",
     main="Well Depth - Trend/Cycle",
     xlab="Time", ylab = "Depth (ft.)", lwd=2)
lines(model$time.series[,2],col="red",lwd=2)

#Seasonally Adj.
seas = well_ts - model$time.series[,1]
plot(well_ts, col='gray', main="Well Depth - Seasonally Adj.", xlab="time",ylab="depth",lwd=2)
lines(seas,col="red",lwd=2)

#Simple Exp Smoothing Model
sesWell <- ses(well_ts, initial = "optimal", h = 6)
summary(sesWell)

plot(sesWell, main = "Well depth Simple SES Forecast", xlab = "Date", ylab = "Depth (ft)")
abline(v = 2018, col = "red", lty = "dashed")
summary(sesWell)


    
#Linear Exp Smoothing Model
lesWell <- holt(well_ts, initial = "optimal", h=6)
summary(lesWell)

plot(lesWell, main = "Well Depth with Linear ESM Forecast", xlab = "Date", ylab = "Well depth (ft)")
abline(v = 2018, col = "red", lty = "dashed")

autoplot(lesWell)+
  autolayer(fitted(lesWell),series="Fitted")+ylab("Well Depth with Holt ESM Forecast")

#Holt-Winters ESM
HWwella <- hw(well_ts,seasonal="additive", h=6)
summary(HWwella)
HWwellm <- hw(well_ts,seasonal="multiplicative", h=6)    ##### Best model based off MAPE is Holt-Winters(mult)
summary(HWwellm)
plot(HWwellm,main="Well Depth with Holt-Winters Forecast")
abline(v=2018,col="red",lty='dashed')















