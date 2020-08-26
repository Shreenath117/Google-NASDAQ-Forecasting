# Case Study Solutions : Goog.csv file
#---------------------------------------------------------------------------------------------------------------
library("imputeTS")
library(forecast)
library(fpp2)
library(wavelets)
library(zoo)
library(ggpubr)
library(TSA)
library(xts)
library(timeSeries)
library("vcdExtra")
library("boot")
library(spatialEco)
library("Hmisc")
library(tseries)
library("hts")
library(knitr)
library(forecast)
library(knitr)
library(vars)
library(fpp)
library(dygraphs)

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 1

# Reading the CSV file and dropping the first column
data=read.csv('goog.csv')
# View the data loaded
head(data)
# Dropping the first column which is nothing but the Serial number
data=data[3:3]
# View the dimensions (shape) of the data to be used for the analysis
dim(data)
# There are 1000 rows and 2 columns

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 2

# Time Series:  A collection of observations along with time spaced intervals or a given time point. 
# For example, the closing price of stock over past few months, height of a certain sample of population on a given time like day or month. Too long or too short time period can create a bias in time period. 
# The time series is generally used for forecasting the future based on current & past patter and trends.

# Cross Sectional: Collection of observations from different data points at the same time.
# For example, considering the closing stock price of multiple companies of a given day would be a cross sectional data. 
# Samples are constructed by collecting the data interest across a range of observational units, people, objects at the same time. 
# We cannot perform forecasting on cross sectional data, we can only have prediction done on such data sets.

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 3

data<- as.ts(data)
head(data)
# Here: the data is time series with stock value in observations for 1000 consecutive trading days

summary(data)

# Here: the value has max value of 835.7 and mean of 599.4

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 4

class(data)

# Time Series data  class

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 5

# install.packages("forecast")
library(forecast)

autoplot(data)

# The Trend seems to be a long term increase.
# However, as it's a trading data there will be volatility over time With a longer outlook we may be able to see some seasonality and cycles occuring as well

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 6

data_time <- ts(data, start = c(2013,02,25), end = c(2017,02,13), frequency = 249.5)

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 7

ggseasonplot(df_time, year.labels = TRUE)

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 8

#As we can see, there's no clearly defined seasonality and the price movements are based off other variables

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 9

gglagplot(data)

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 10

# Stationarity and it's significance for time series forecasting :

# In time series data which has constant mean, variance, covariance, autocorrelation over time is stationarity. 
# The algorithm used in time series assumes that the data is stationarized. 
# It makes it easy for forecasting. 
# The forecasting for stationarized series can be untransformed by obtaining forecast for original series. 
# Stationarizing it time series through differencing is an important for fitting in models like ARIMA.

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 11

adf.test(data)
adf.test(data_time)

# Rejects the null hypthesis for both : and shows stationary time series

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 12

# ADF Test :
# The null hypothesis of the test is that the time series can be represented by a unit root, that it is not stationary (has some time-dependent structure). 
# The alternate hypothesis (rejecting the null hypothesis) is that the time series is stationary.

# p-value > 0.05: Fail to reject the null hypothesis (H0), the data has a unit root and is non-stationary.
# p-value <= 0.05: Reject the null hypothesis (H0), the data does not have a unit root and is stationary.

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 13

# Seasonality: In a given time series data, when it has patterns of ups and downs of fluctuations due to seasonal variations with a time of a day, week, month and year. 
# For example: sales of warm clothing during winter or high volumes sales of FMCG at the beginning of the month. Seasonality is always fixed and known pattern.

# Cyclicality: When a time series data has not fixed pattern with unusual rise and fall. 
# If the fluctuations are not of fixed period, they are cyclic. 
# The average length of cycles is longer than seasonal patterns. 
# For example: 5 years of economic growth, followed by 2 years of slump, and again 7 years of growth. 

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 14

#Applying the diff function to our data will give us daily changes in google stock prices. 
#Let's see the daily changes and if there's any white noise We can also apply the ggAcf() function mentioned above

data_diff <- diff(data)
autoplot(data_diff)

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 15

ggAcf(data_diff)
ggPacf(data_diff)

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 16

# ACF plot is a bar plot of coefficient of correlation between time series and lag. 
# If a data is Stationary, the ACF plot will help to drop to zero quickly. 
# If the data is not stationary, then the ACF plot will slowly help to drop to Stationary. 

# Also, the value at Lag1 is usually large and positive for stationary data. 

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 17

decomp_additive <- decompose(df_time, type= "additive")

decomp_multiplicative <- decompose(df_time,type= "multiplicative")

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 18

plot(decomp_additive)

plot(decomp_multiplicative)

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 19

# It is time dependent. 
# So the basic assumption of a linear regression model that the observations are independent doesn't hold in this case.


#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 20

# These three parameters account for seasonality, trend, and noise in data
# ARIMA models are made of three parts:
# i) A weighted sum of lagged values of the series (Auto-regressive (AR) part)
# ii) A weighted sum of lagged forecasted errors of the series (Moving-average (MA) part)
# iii) A difference of the time series (Integrated (I) part)

# An ARIMA model is often noted ARIMA(p, d, q) where p represents the order of the AR part, d the order of differencing ("I" part), and q the order of the MA term.

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 21

manual<-Arima(df_time,order = c(0,0,2), seasonal = c(2,0,0))
summary(manual)

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 22

autoArimaFit <- auto.arima(df_time)
autoArimaFitDiff <- auto.arima(df_time_diff)

#---------------------------------------------------------------------------------------------------------------
# Soln. to question: 23

for_man<-forecast(manual)
plot(for_man)
plot(forecast(autoArimaFit, h=50))
plot(forecast(autoArimaFitDiff, h=50))

#---------------------------------------------------------------------------------------------------------------
