library(readxl)
library(lubridate)
library(tidyverse)
library(tseries)
library(forecast)
library(lmtest)
library(zoo)


# read only the well sheet from excel
well <- read_excel("time_series/F-179.xlsx", sheet = "Well")

well_2 <- well %>% 
  mutate(Corrected = Corrected + 1) %>% 
  group_by(date = as.yearmon(date)) %>% 
  summarise(avg = mean(Corrected))

# created testing set with last 6 months of data
train <- well_2 %>% 
  filter(date <= '2017-12')
test <- anti_join(well_2, train, by = "date")


# make the timeseries object with training data
well_ts_train <- ts(train$avg, start = c(2007,10), frequency = 12)

# create a holdout data set
well_ts_test <- ts(test$avg, start = c(2018,1), frequency = 12)

# stl decomposition
decomp_stl <- stl(well_ts_train, s.window = 7)
plot(decomp_stl)

adf_out <- list()
for (i in 1:3) {
  adf_out[[i]] <- adf.test(well_ts_train, "stationary", k = i)
}

nsdiffs(well_ts_train)

well_ts_train %>% 
  diff(lag = 12) %>% 
  ndiffs()

Box.test(diff(well_ts_train), lag = 12, type="Ljung-Box")
well_ts_seas_diff <- diff(well_ts_train, lag = 12)
nsdiffs(well_ts_seas_diff)     
plot(well_ts_seas_diff)

well_ts_train %>% 
  ur.kpss() %>% 
  summary()
