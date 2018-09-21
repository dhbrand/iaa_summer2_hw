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

# code from hw2 ####
plot(well_ts_train, col = "grey", 
     main = "Average Well Depth - Trend/Cycle", 
     xlab = "", 
     ylab = "Depth (in Feet)", 
     lwd = 2)
lines(decomp_stl$time.series[,2], 
      col = "red", 
      lwd = 2)

seas_well <- well_ts_train - decomp_stl$time.series[,1]
plot(well_ts_train, 
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
ses_well <- ses(well_ts_train, initial = "optimal", h = 6)
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
les_well <- holt(well_ts_train, initial = "optimal", h = 6)
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

ldes_well <- holt(well_ts_train, initial = "optimal", h = 6, damped = TRUE)
summary(ldes_well)

plot(ldes_well, 
     main = "Average Well Depth with Linear Damped ESM Forecast", 
     xlab = "Date", 
     ylab = "Depth (in Feet)")
abline(v = 2018, 
       col = "red", 
       lty = "dashed")



# Building a Holt-Winters ESM - Well Data #
hwes_well_add <- hw(well_ts_train, seasonal = "additive", h = 6)
summary(hwes_well_add)

plot(hwes_well_add, 
     main = "Average Well Depth with Holt-Winters ESM Forecast", 
     xlab = "Date", 
     ylab = "Depth (in Feet)")
abline(v = 2018, 
       col = "red", 
       lty = "dashed")

hwes_well_mult <- hw(well_ts_train, seasonal = "multiplicative", h = 6)
summary(hwes_well_mult)

plot(hwes_well_mult, 
     main = "Average Well Depth with Holt-Winters ESM Forecast", 
     xlab = "Date", 
     ylab = "Depth (in Feet)")
abline(v = 2018, 
       col = "red", 
       lty = "dashed")




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

error_m <- log(well_ts_test) - hwes_well_train_m$mean
MAE <- mean(abs(error_m)) # 0.116
MAPE <- mean(abs(error_m)/abs(well_ts_test)) # 0.09


#plot of predicted vs actual
pred_actual <- bind_cols(date = as.character(as.yearmon(time(well_ts_test))), actual = as.numeric(well_ts_test), predicted = as.numeric(hwes_well_train_m$mean))

ggplot(gather(pred_actual, act_pred, depth, -date), aes(date, depth, group = act_pred)) +
  geom_point(aes(color = act_pred), size = 4) +
  geom_line(aes(color = act_pred), size = 2) +
  theme_bw() +
  labs(x = "Date",
       y = "Depth (in Feet)",
       title = "Actual vs Predicted Test Values for Well Depth",
       color = "Legend") +
  scale_color_manual(values = c("red", "blue"))


# fanning of season for multiplicative model
well_ts_train %>%
  timetk::tk_tbl() %>% 
  group_by(year = year(index)) %>% 
  summarize(min = min(value),
            max = max(value)) %>% 
  mutate(diff = max - min) %>% 
  arrange(desc(diff))

season_well <- well_ts_train - decomp_stl$time.series[,2] - decomp_stl$time.series[,3]
plot(well_ts_train, 
     col = "grey", 
     main = "Average Well Depth - Seasonality", 
     xlab = "", 
     ylab = "Depth (in Feet)", 
     lwd = 2)
lines(season_well, 
      col = "red", 
      lwd = 2)



# code for hw1 ####

# look at 5 number summary of dataframe
summary(well)

# histogram plot of well
hist(well$Corrected)


# Question 1 ####
well_2 <- well %>% 
  # create a new variable which is an integer for the hour of each time
  mutate(time_2 = hour(time)) %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Corrected variable to depth
  select(datetime, depth = Corrected) 

well_3 <- well_2 %>% 
  # group by will find the smallest increment which is hour in datetime and prep for grouping functions
  group_by(datetime) %>%
  # you can create new variable which aggregate the values of datetime using the mean and stdev functions
  summarise(avg = mean(depth))

mean(well_3$avg)
sd(well_3$avg)

# Question 2 ####
# create a sequnce of dates in the year/month/day hour format incremented by each hour
i <- seq(ymd_h("2007-10-01 01"), ymd_h("2018-06-04 10"), by = '1 hour')
# find the difference between the new date sequence and the original unique hour values for our data
diff <- length(i) - length(well_3$datetime)
missing_dates <- distinct(tibble(missing = as.yearmon(i[!i %in% well_3$datetime])))
# Question 3 ####
# create a gg object using the well_3 dataframe with aesthetics: x = datetime and y = avg
ggplot(well_3, aes(datetime, avg)) +
  # draw a line plot using the above defined aesthetics
  geom_line(color = "blue") +
  # drop some of the ugly R thematic elements for a simple look
  theme_bw() +
  # label the axes and add a title
  labs(x = "Date And Time (in hours)", y = "Avg Depth of Well (in feet)", title = "Avg Depth of Well From 2007-2018")


# faceted time plot by year
ggplot(well_3, aes(datetime, avg)) +
  # draw a line plot using the above defined aesthetics
  geom_line(color = "blue") +
  # drop some of the ugly R thematic elements for a simple look
  theme_bw() +
  # label the axes and add a title
  labs(x = "Date And Time (in hours)", y = "Avg Depth of Well (in feet)", title = "Avg Depth of Well From 2007-2018") +
  facet_wrap(~ year(datetime), scales = "free_x")


# messing around with timeseries objects and functions
well_ts <- ts(well_3$avg, start = c(1,1), frequency = 24*365)
decomp_stl <- stl(well_ts, s.window = 7)
plot(decomp_stl)
