# Time series - ARIMA

library(tseries)
library(forecast)

# set the work directory
setwd('C:\\EK\\work_data')

# read the data
data = read.csv('Tractor-Sales.csv')

# transform to time series object and plot
data = ts(data[,2],start = c(2003,1),frequency = 12)
plot(data, xlab='Years', ylab = 'Tractor Sales')

# decompose the time series
components <- decompose(data,type = 'multiplicative')
plot(components)

# split into train and test, the last 12 months to test
train = data[1:132]
test = data[133:144]

# auto arima
AUTO_ARIMA_fit <- auto.arima(train, approximation=FALSE,trace=FALSE)
summary(AUTO_ARIMA_fit)
# The best fit model is selected based on Akaike Information Criterion (AIC)
# and Bayesian Information Criterion (BIC) values. 
# The idea is to choose a model with minimum AIC and BIC values. 

AUTO_ARIMA_fit <- arima (train, order=c(1,1,1))
AUTO_ARIMA_fit

# forecast for the next year (12 months)
pred = predict(AUTO_ARIMA_fit, n.ahead = 12)

# forecast versus actual plot
par(mfrow = c(1,1))
plot(1:132,train,type='l',xlim=c(1,144), xlab = 'Year',ylab = 'Tractor Sales')
lines(133:144,pred$pred,col='blue')
lines(133:144,test,col='red')

# Now let's do it on our own, without auto arima
# Using ACF and PACF to find the model orders.
# First, we need to make the time series stationary.

# difference the data to try make it stationary
plot(diff(data),ylab='Differenced Tractor Sales')
# stationary on mean but not on variance

# logarithmic transformation on the data
plot(log10(data),ylab='Log (Tractor Sales)')
# the variance becomes constant

# difference the log-transformed series
plot(diff(log10(data)),ylab='Differenced Log (Tractor Sales)')
# this looks stationary in both mean and variance

# check for stationarity
# Augmented Dickie-Fuller test
adf.test(diff(log10(data)))

# ACF and PACF plots
par(mfrow = c(1,2))
acf(ts(diff(log10(data))),main='ACF Tractor Sales')
pacf(ts(diff(log10(data))),main='PACF Tractor Sales')

# ARIMA(p,d,q)(P,D,Q)12
# d=1 (differenced once)
# p=0, q=0 (nothing significant after lag 0)
# D=0 (not differenced across seasons)
# P=1, Q=0 (decay in the seasonal lags of PACF, single drop in ACF)

# NOTE THAT WE WILL BE MODELLING THE LOGARITHM OF THE ORIGINAL DATA

# (0,1,0)x(1,0,0)12 ARIMA model
ARIMA_fit <- arima (log10(train),
                  order=c(0,1,0),
                  seasonal = list(order=c(1,0,0),period=12))
ARIMA_fit

# forecast for the next year (12 months)
pred = predict(ARIMA_fit, n.ahead = 12)

# The model is on the logarithm of sales at base 10. 
# Let us convert the values. We need to take the exponent of the logarithm - 
# this is the opposite function, the antilogarithm.
forecast = 10^pred$pred

# forecast versus actual plot
par(mfrow = c(1,1))
plot(1:132,train,type='l',xlim=c(1,144), xlab = 'Year',ylab = 'Tractor Sales')
lines(133:144,forecast,col='blue')
lines(133:144,test,col='red')

# Mean Absolute Error (MAE)
MAE = mean(abs(forecast - test))
round(MAE, 2)

# Mean Absolute Percentage Error (MAPE)
MAPE = mean(abs(forecast - test)/abs(test))
round(MAPE, 4)
