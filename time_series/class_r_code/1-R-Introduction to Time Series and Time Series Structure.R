#------------------------------------#
#    Introduction to Time Series     #
#      & Time Series Structure       #
#                                    #
#           Dr Susan Simmons           #
#------------------------------------#

# Needed Libraries for Analysis #
# install.packages('forecast',dependencies = T)
# install.packages('tseries')
# install.packages(c('expsmooth','lmtest','zoo','seasonal'))
library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)

# Saving File Locations and Uploading SAS File #
file.dir <- "data/"
input.file1 <- "usairlines.sas7bdat"
input.file2 <- "ar2.sas7bdat"

USAirlines <- read_sas(paste(file.dir, input.file1,sep = ""))
AR2 <- read_sas(paste(file.dir, input.file2, sep = ""))

# Creation of Time Series Data Object #
Passenger <- ts(USAirlines$Passengers, start = 1990, frequency =12)

# Time Series Decomposition ...STL#
decomp_stl <- stl(Passenger, s.window = 7)
plot(decomp_stl)

plot(Passenger, col = "grey", main = "US Airline Passengers - Trend/Cycle", xlab = "", ylab = "Number of Passengers (Thousands)", lwd = 2)
lines(decomp_stl$time.series[,2], col = "red", lwd = 2)

seas_pass=Passenger-decomp_stl$time.series[,1]
plot(Passenger, col = "grey", main = "US Airline Passengers - Seasonally Adjusted", xlab = "", ylab = "Number of Passengers (Thousands)", lwd = 2)
lines(seas_pass, col = "red", lwd = 2)

monthplot(decomp_stl$time.series[,"seasonal"], main = "US Airline Passengers - Monthly Effects", ylab = "Seasonal Sub Series", xlab = "Seasons (Months)", lwd = 2)


#####For fun....X13
decomp_x13=seas(Passenger)
summary(decomp_x13)
# install.packages('seasonalview')
library(seasonalview)
view(decomp_x13)
