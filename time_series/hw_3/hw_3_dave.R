library(readxl)
library(lubridate)
library(tidyverse)
library(tseries)
library(forecast)
library(lmtest)
library(zoo)
library(broom)
library(ggfortify)
library(uroot)

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

# classical decomposition
decomposed <- decompose(well_ts_train)
s <- seasonal(decomposed)
t <- trendcycle(decomposed)
r <- remainder(decomposed)
1 - ( var(r, na.rm = T) / var(s + r, na.rm = T)) # seasonal strength = 0.577262 
1 - ( var(r, na.rm = T) / var(t + r, na.rm = T)) # trend strength = 0.1507067

# stl decomposition
decomp_stl <- stl(well_ts_train, s.window = 7)
plot(decomp_stl)
summary(decomp_stl)
# calculating the strength of trend and seasonality
s <- seasonal(decomp_stl)
t <- trendcycle(decomp_stl)
r <- remainder(decomp_stl)
1 - ( var(r, na.rm = T) / var(s + r, na.rm = T)) # seasonal strength = 0.725874 
1 - ( var(r, na.rm = T) / var(t + r, na.rm = T)) # trend strength = 0.2632431


# hyndman measures decomposition
source("time_series/time_series_measures.R")
measures(well_ts_train)
mean(decomp(well_ts_train)$season)
mean(decomp(well_ts_train)$trend)


s <- decomp(well_ts_train)$season
r <- decomp(well_ts_train)$remainder
t <- decomp(well_ts_train)$trend
1 - ( var(r, na.rm = T) / var(s + r, na.rm = T)) # seasonal strength = 0.618117 
1 - ( var(r, na.rm = T) / var(t + r, na.rm = T)) # trend strength = 0.2314797

# looking at seasonality of ts object
nsdiffs(well_ts_train, test =  "seas")
nsdiffs(well_ts_train, test =  "ocsb")
nsdiffs(well_ts_train, test =  "hegy")
nsdiffs(well_ts_train, test =  "ch")

well_ts_train %>% 
  diff(lag = 12) %>% 
  ndiffs()

stationary_s <- diff(well_ts_train, lag = 12)
ndiffs(stationary_s)
plot(stationary_s)



ocsb.test(well_ts_train, lag.method = "AIC", maxlag = 12)
ch_mod <- ch.test(well_ts_train)
summary(ch_mod)
hegy_mod <- hegy.test(well_ts_train)
summary(hegy_mod)
# easy way to generate dummy vars
seasonal.dummies(well_ts_train)

# building a linear regressin model with the ts object and adding season
tslm_mod <- tslm(well_ts_train ~  season)
summary(tslm_mod)

#gettin the residuals for the model after removing deterministic seasonsal effects
stationary_d <- tslm_mod$residuals

arima(well_ts_train,order=c(0,0,0),seasonal=list(order=c(1,0,0),period=12))


# looking at trend in ts object
x <- time(well_ts_seas_diff)
y <- as.numeric(well_ts_seas_diff)
reg_mod <- lm(y ~ x)
summary(reg_mod)
y.ts <- ts(y)
arima.trend <- Arima(y.ts, xreg = x, order = c(0, 0, 0))
summary(arima.trend)
res.lm.trend <- reg_mod$residuals
res.trend.arima <- arima.trend$residuals
plot(res.lm.trend, res.trend.arima)
plot(x,y)
lines(reg_mod$fitted.values)
plot(well_ts_seas_diff)
trend <- ts(reg_mod$residuals)


# checking for stochasticity in ts object after removing seasonality
adf_out_seas <- list()
for (i in 1:3) {
  adf_out_seas[[i]] <- tidy(adf.test(stationary_d, "stationary", k = i))
}
adf_df_seas <- bind_rows(adf_out_seas[[1]], adf_out_seas[[2]], adf_out_seas[[3]])
adf_df_seas


adf_out_seas <- list()
for (i in 1:3) {
  adf_out_seas[[i]] <- tidy(adf.test(stationary_s, "stationary", k = i))
}
adf_df_seas <- bind_rows(adf_out_seas[[1]], adf_out_seas[[2]], adf_out_seas[[3]])
adf_df_seas

# plotting adjustment for seasonality
autoplot(stationary_d, ts.colour = 'blue') +
  theme_bw() +
  labs(x = "Dates from October 2008 to December 2017",
       y = "Depth of Well adjusted for Seasonal Differences(in Feet)",
       title = "Stationarity Time Series of Well F-179 ") +
  geom_point(aes(y = as.numeric(stationary_d)), size = 0.5)
autoplot(well_ts_train)


autoplot(stationary_s, ts.colour = 'red') +
  theme_bw() +
  labs(x = "Dates from October 2008 to December 2017",
       y = "Depth of Well adjusted for Seasonal Differences(in Feet)",
       title = "Stationarity Time Series of Well F-179 ") +
  geom_point(aes(y = as.numeric(stationary_s)), size = 0.5) +
  geom_hline(yintercept = 0, color = "grey")+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
  
ggplot(data = train, aes(x = date, y = avg)) +
  geom_line(color = "grey")
