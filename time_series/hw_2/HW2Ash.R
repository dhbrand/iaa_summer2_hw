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

sesWell <- ses(well_ts, initial = "optimal", h = 6)
summary(sesWell)

plot(sesWell, main = "Well depth Simple SES Forecast", xlab = "Date", ylab = "Depth (ft)")
abline(v = 2018, col = "red", lty = "dashed")
round(accuracy(sesWell,2))

    
#Linear Exp Smoothing Model

lesWell <- holt(well_ts, initial = "optimal", h=6)
summary(lesWell)

plot(lesWell, main = "Well Depth with Linear ESM Forecast", xlab = "Date", ylab = "Well depth (ft)")
abline(v = 2018, col = "red", lty = "dashed")

autoplot(lesWell)+
  autolayer(fitted(lesWell),series="Fitted")+ylab("Well Depth with Holt ESM Forecast")
