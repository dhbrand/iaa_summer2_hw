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
excel_sheets("../F-179.xlsx")
# rain <- read_excel("F-179.xlsx", sheet = "Rain")
# tide <- read_excel("F-179.xlsx", sheet = "Tide")

# read only the well sheet from excel
well <- read_excel("../F-179.xlsx", sheet = "Well")

#well <- read_excel("F-179.xlsx", sheet = "Well", col_types = c("date", "date", "skip", "skip", "skip", "numeric"))
# summary(rain)
# summary(tide)

# look at 5 number summary of dataframe
summary(well)

# histogram plot of well
hist(well$Corrected)



well %<>% 
  group_by(year(date), month(date)) %>% 
  summarise(avg = mean(Corrected)) %>% 
  unite(date, 1, 2, sep = "-") %>% 
  mutate(date = zoo::as.yearmon(date))

train <- well %>% 
  filter(date <= '2017-12')
test <- anti_join(well, train, by = "date")



# messing around with timeseries objects and functions
well_ts <- ts(train$avg, start = c(1,1), frequency = 12)
decomp_stl <- stl(well_ts, s.window = 7)
plot(decomp_stl)
