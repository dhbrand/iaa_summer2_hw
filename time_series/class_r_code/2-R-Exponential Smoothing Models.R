#------------------------------------#
#        Exponential Smoothing       #
#               Models               #
#                                    #
#           Dr Susan Simmons         #
#------------------------------------#

# Needed Libraries for Analysis #
library(haven)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(ggplot2)

# Saving File Locations and Uploading SAS File #
file.dir <- "data/"
input.file1 <- "usairlines.sas7bdat"
input.file2 <- "steel.sas7bdat"

USAirlines <- read_sas(paste(file.dir, input.file1, sep = ""))
Steel <- read_sas(paste(file.dir, input.file2, sep = ""))

# Creating Time Series Data Objects #
Passenger <- ts(USAirlines$Passengers, start = 1990, frequency = 12)

SteelShp <- ts(Steel$steelshp, start = 1984, frequency = 12)

# Building a Single Exponential Smoothing Model - Steel Data #
SES.Steel <- ses(SteelShp, initial = "optimal", h = 24)
summary(SES.Steel)

plot(SES.Steel, main = "US Steel Shipments with Simple ESM Forecast", xlab = "Date", ylab = "Shipments (Thousands of Net Tons)")
abline(v = 1992, col = "red", lty = "dashed")
round(accuracy(SES.Steel),2)

autoplot(SES.Steel)+
  autolayer(fitted(SES.Steel),series="Fitted")+ylab("US Steel Shipments with Simple ESM Forecast")

# Building a Linear Exponential Smoothing Model - Steel Data #
LES.Steel <- holt(SteelShp, initial = "optimal", h = 24)
summary(LES.Steel)

plot(LES.Steel, main = "US Steel Shipments with Linear ESM Forecast", xlab = "Date", ylab = "Shipments (Thousands of Net Tons)")
abline(v = 1992, col = "red", lty = "dashed")

autoplot(LES.Steel)+
  autolayer(fitted(LES.Steel),series="Fitted")+ylab("US Steel Shipments with Holt ESM Forecast")

LDES.Steel <- holt(SteelShp, initial = "optimal", h = 24, damped = TRUE)
summary(LDES.Steel)

plot(LDES.Steel, main = "US Steel Shipments with Linear Damped ESM Forecast", xlab = "Date", ylab = "Shipments (Thousands of Net Tons)")
abline(v = 1992, col = "red", lty = "dashed")

# Building a Linear Exponential Smoothing Model - US Airlines Data #
LES.USAir <- holt(Passenger, initial = "optimal", h = 24)
summary(LES.USAir)

plot(LES.USAir, main = "US Airline Passengers with Linear ESM Forecast", xlab = "Date", ylab = "Passengers (Thousands)")
abline(v = 2008.25, col = "red", lty = "dashed")

LDES.USAir <- holt(Passenger, initial = "optimal", h = 24, damped = TRUE)
summary(LDES.USAir)

plot(LDES.USAir, main = "US Airline Passengers with Linear Damped ESM Forecast", xlab = "Date", ylab = "Passengers (Thousands)")
abline(v = 2008.25, col = "red", lty = "dashed")

# Building a Holt-Winters ESM - US Airlines Data #
HWES.USAir <- hw(Passenger, seasonal = "additive")
summary(HWES.USAir)

plot(HWES.USAir, main = "US Airline Passengers with Holt-Winters ESM Forecast", xlab = "Date", ylab = "Passengers (Thousands)")
abline(v = 2008.25, col = "red", lty = "dashed")

HWES.USAir <- hw(Passenger, seasonal = "multiplicative")
summary(HWES.USAir)

plot(HWES.USAir, main = "US Airline Passengers with Holt-Winters ESM Forecast", xlab = "Date", ylab = "Passengers (Thousands)")
abline(v = 2008.25, col = "red", lty = "dashed")

#####Using a holdout data set
training=subset(Passenger,end=length(Passenger)-12)
test=subset(Passenger,start=length(Passenger)-11)
HWES.USAir.train <- hw(training, seasonal = "multiplicative",initial='optimal')
test.results=forecast(HWES.USAir.train,h=12)

error=test-test.results$mean
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(test))
 


