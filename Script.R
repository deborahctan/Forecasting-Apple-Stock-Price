# Final Class Project
# Author: Debbie Tan

# ----------------------------------------------
# Install the required libraries for this project
# ----------------------------------------------

install.packages("dplyr")
install.packages("lubridate")
install.packages("quantmod")
install.packages("TTR")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("forecast")
install.packages("plotly")
install.packages("ggfortify")
install.packages("tseries")
install.packages("gridExtra")
install.packages("docstring")
install.packages("readr")
install.packages("tseries")
install.packages("factorEx")
install.packages("plotly")
install.packages("here")
install.packages("TSstudio")
install.packages("tsbox")


# ----------------------------------------------
# Import the required libraries for this project
# ----------------------------------------------

library(dplyr)
library(lubridate)
library(quantmod)
library(TTR)
library(ggplot2)
library(tidyverse)
library(forecast)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
library(tseries)
library(factorEx)
library(plotly)
library(here)
library(TSstudio)
library(tsbox)

# -------------------------------------------------------------------
# EDA
#--------------------------------------------------------------------

# Import stock price data using existing R packages
getSymbols('AAPL', from='2018-01-01') 

# Run summary and head functions to check for NAs and get general distribution of data set
summary(AAPL)

# Index              AAPL.Open        AAPL.High         AAPL.Low        AAPL.Close      AAPL.Volume        AAPL.Adjusted   
# Min.   :2018-01-02   Min.   : 35.99   Min.   : 36.43   Min.   : 35.50   Min.   : 35.55   Min.   : 45448000   Min.   : 34.61  
# 1st Qu.:2018-11-06   1st Qu.: 47.10   1st Qu.: 47.50   1st Qu.: 46.71   1st Qu.: 47.15   1st Qu.: 90104400   1st Qu.: 45.78  
# Median :2019-09-17   Median : 56.25   Median : 56.83   Median : 55.62   Median : 56.10   Median :114731400   Median : 54.94  
# Mean   :2019-09-16   Mean   : 72.63   Mean   : 73.49   Mean   : 71.78   Mean   : 72.66   Mean   :131078821   Mean   : 71.73  
# 3rd Qu.:2020-07-24   3rd Qu.: 96.99   3rd Qu.: 98.11   3rd Qu.: 95.94   3rd Qu.: 97.01   3rd Qu.:154465725   3rd Qu.: 96.37  
# Max.   :2021-06-02   Max.   :143.60   Max.   :145.09   Max.   :141.37   Max.   :143.16   Max.   :426510000   Max.   :142.70  

head(AAPL)

#            AAPL.Open AAPL.High AAPL.Low AAPL.Close AAPL.Volume AAPL.Adjusted
# 2018-01-02   42.5400   43.0750  42.3150    43.0650   102223600      41.31007
# 2018-01-03   43.1325   43.6375  42.9900    43.0575   118071600      41.30288
# 2018-01-04   43.1350   43.3675  43.0200    43.2575    89738400      41.49474
# 2018-01-05   43.3600   43.8425  43.2625    43.7500    94640000      41.96716
# 2018-01-08   43.5875   43.9025  43.4825    43.5875    82271200      41.81128
# 2018-01-09   43.6375   43.7650  43.3525    43.5825    86336000      41.80650

# From here we can see that there are no NAs to account for. We have a complete data set!
# We also do not need to transform the date data, as the data set is already in XTS format with dates as the index.

# Let's create some charts to visualize this data set. We're using adjusted close price to account for stock splits.
boxplot(AAPL$AAPL.Adjusted,
        ylab = "AAPL Adjusted Closing Price (in US $)")

# No extreme outliers here, so we don't necessarily need to omit any numbers at this point.
# The median is at a pretty low price ($54.94) relative to what it is now ($124.61)
# We're seeing a long upper whisker, especially with the meteoric rise in price during 2020!

# Plot adjusted closing price over time with technical indicators
chartSeries(AAPL,
            type="candlesticks",
            subset='2018-01::2021-05',
            theme=chartTheme('white'))
addSMA(n=50, on=1, col="blue")
addSMA(n=200, on=1, col="red")
addRSI(n=14,maType="EMA")

# -------------------------------------------------------------------
# TEST FOR STATIONARITY
# -------------------------------------------------------------------
# Use Augmented Dickey-Fuller Test for stationarity. To run time series models, we must adjust for non-stationarity.
adf.test(AAPL$AAPL.Adjusted)

# Augmented Dickey-Fuller Test
# data:  AAPL$AAPL.Adjusted
# Dickey-Fuller = -2.0226, Lag order = 9, p-value = 0.5687
# alternative hypothesis: stationary

# p-value is high, so we accept null hypothesis (non-stationarity). Let's visualize the data further.

# Create training set to which we'll compare values for 2021.

aapl_adjusted_only = AAPL[,6] # Subset to only adjusted close price column

aapl_adjusted_ts = ts_ts(aapl_adjusted_only) # Convert from XTS to TS format for easier use with TSstudio package functions.

# Split data set into test and train sets
aapl_par = ts_split(aapl_adjusted_ts, sample.out=365) 
train = aapl_par$train
test = aapl_par$test

# Remove NAs from data sets for visualization function purposes (some were added with the ts_split function)
aapl_adjusted_ts_cleaned = na.remove(aapl_adjusted_ts)

# Let's decompose the full data set to check for any trends.
plot(decompose(aapl_adjusted_ts_cleaned))

# From the decomposed time series of AAPL, we see that there is a component of seasonality here that we may need to adjust for.

# Take the difference in daily closing values to account for the non-stationary nature of the data set.
tsDiff <- diff(train)

plot(tsDiff,
     main = "Apple First Difference Time Series (2018-2020)",
     xlab = "Year",
     ylab = "Closing Values")

adf.test(tsDiff) #Run the ADF test again to test for stationary data

# Augmented Dickey-Fuller Test
# data:  tsDiff
# Dickey-Fuller = -7.1262, Lag order = 7, p-value = 0.01
# alternative hypothesis: stationary

# p-value < 0.05, so we reject the null hypothesis and accept the alternative hypothesis.

# Plot ACF and PACF charts
ggAcf(train)
ggPacf(train)

# This means that our transformed data is stationary,
# and we can use a certain type of ARIMA model that accounts for these differences when forecasting.

# -------------------------------------------------------------------
# FORECASTING WITH ARIMA
# -------------------------------------------------------------------
model = auto.arima(train)
forecast = forecast(model, h=365)

# Plotting actual vs. fitted and forecasted
test_forecast(actual = aapl_adjusted_ts,
              forecast.obj = forecast,
              test = test)

# -------------------------------------------------------------------
# CHECKING OUR MODEL ACCURACY
# -------------------------------------------------------------------
round(accuracy(forecast, test))

#              ME RMSE MAE MPE MAPE MASE ACF1 Theil's U
# Training set  0    1   1   0    1    0    0        NA
# Test set     29   31  29  24   24    2    1        12


# We see that the RMSE of the test set is much higher than the training set's.
# This means that we have badly over fit the data and should adjust the model.
