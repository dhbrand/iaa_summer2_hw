library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(tseries)



#### Deterministic Trend
x <- seq(1, 48)
set.seed(38752)
error <- rnorm(48, 0, 2.5)
y <- 3 + 1.3 * x + error
plot(x, y, type = "l")
plot(x, y, xlab = "Time", ylab = "y", main = "Example of a trending time series", type = "l")
reg.lm <- lm(y ~ x)
summary(reg.lm)
y.ts <- ts(y)
arima.trend <- Arima(y.ts, xreg = x, order = c(0, 0, 0))
summary(arima.trend)
res.lm.trend <- reg.lm$residuals
res.trend.arima <- arima.trend$residuals[1:48]
plot(res.lm.trend, res.trend.arima)
plot(x, y, type = "l")
fit.y <- arima.trend$fitted
lines(fit.y)


#### Deterministic Seasonal
season <- matrix(rep(c(1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0), 12), byrow = T, nrow = 48)
seas.coef <- c(5.1, 8.6, 15)
y2 <- 3 + season %*% seas.coef + error
plot(x, y2, type = "l")
y3 <- 3 + 1.3 * x + season %*% seas.coef + error
plot(x, y3, type = "l")
plot(x, y3, type = "l", xlab = "Time", ylab = "y", main = "Both trend and seasonality")
reg.seas <- lm(y2 ~ season)
summary(reg.seas)
y2.ts <- ts(y2, frequency = 4)
arima.season <- Arima(y2.ts, xreg = season, order = c(0, 0, 0))
summary(arima.season)

###### Both trend and season
X.reg <- cbind(x, season)
reg.model <- lm(y3 ~ X.reg)
summary(reg.model)
y3.ts <- ts(y3, frequency = 4)
arima.model <- Arima(y3.ts, xreg = X.reg, order = c(0, 0, 0))
summary(arima.model)
res.arima <- arima.model$residuals[1:48]
res.reg <- reg.model$residuals
plot(res.arima, res.reg)
fit.y <- arima.model$fitted
plot(x, y3, type = "l")
lines(fit.y)


###### Stochastic

#### Create a random walk

y <- rep(NA, 15)
y[1] <- 7
x <- seq(1, 15)
plot(x, y)

### IN class

newy <- -1
i <- 15 - sum(is.na(y))
y[i + 1] <- y[i] + newy
plot(x, y, type = "l")

###### Let's do many of them....
y <- vector(length = 48)
y[1] <- 8
step.dir <- sample(c(0, 1), 47, replace = T)
y.step <- ifelse(step.dir == 0, -1, 1)

x <- seq(1, 48)
for (i in 2:48)
{
  y[i] <- y[i - 1] + y.step[i - 1] + rnorm(1, 0, 2.5)
}
plot(x, y, type = "l")
diff.y <- diff(y, lag = 1)
plot(diff.y, type = "l")
y.ts <- ts(y)
rw.model <- Arima(y.ts, order = c(0, 1, 0))
summary(rw.model)


#### Random Walk with Drift

y1 <- vector(length = 48)
y1[1] <- 8
for (i in 2:48)
{
  y1[i] <- 1.6 + y1[i - 1] + y.step[i - 1] + rnorm(1, 0, 2.5)
}
plot(x, y1, type = "l")
diff.y1 <- diff(y1, lag = 1)
plot(diff.y1, type = "l")
y1.ts <- ts(y1)
rw.drift <- Arima(y1.ts, order = c(0, 1, 0))
summary(rw.model)
##### Seasonal random walk

y2 <- vector(length = 48)
y2[1:4] <- c(4, 5, 7, 2)

for (i in 1:11)
{
  y2[(4 * i) + 1] <- y2[(4 * i) - 3] + rnorm(1, 0, 2)
  y2[(4 * i) + 2] <- y2[(4 * i) - 2] + rnorm(1, 0, 2)
  y2[(4 * i) + 3] <- y2[(4 * i) - 1] + rnorm(1, 0, 2)
  y2[(4 * i) + 4] <- y2[(4 * i)] + rnorm(1, 0, 2)
}

plot(x, y2, type = "l")
diff.y2 <- diff(y2, lag = 4)
y2.ts <- ts(y2, frequency = 4)
rw.season <- Arima(y2.ts, season = c(0, 1, 1))

##### Augmented Dickey-Fuller test
file.dir <- "~/msa_drive/fall_1/times_series/data/"
input.file1 <- "usairlines.sas7bdat"
input.file2 <- "leadyear.sas7bdat"
input.file3 <- "ebay9899.sas7bdat"

USAirlines <- read_sas(paste(file.dir, input.file1, sep = ""))
Lead.Year <- read_sas(paste(file.dir, input.file2, sep = ""))
Ebay <- read_sas(paste(file.dir, input.file3, sep = ""))
Passenger <- ts(USAirlines$Passengers, start = 1990, frequency = 12)
Daily.High <- ts(na.omit(Ebay$DailyHigh))


# Augmented Dickey-Fuller Testing #
adf.test(Daily.High, alternative = "stationary", k = 0)

ADF.Pvalues <- rep(NA, 3)
for (i in 0:2) {
  ADF.Pvalues[i + 1] <- adf.test(Daily.High, alternative = "stationary", k = i)$p.value
}

# Automated Differencing Test Function #
ndiffs(Daily.High)

# Automated Seasonal Differencing Test Function #
nsdiffs(Passenger)
ndiffs(diff(Passenger, lag = 12))

