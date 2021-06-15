# MscFE 690 Capstone (Jun 2021)
# WorldQuant University
# What Can Be Inferred from Crypto Correlations?
# Author: María Guinda > mariaguinda@gmail.com

# Period: 2017 - 2021
# Crypto Source: coinmarketcap.com
# Equity Source: Yahoo Finance

# Libraries
library(crypto) # for downloading crypto data
library(rtsdata) #to obtain data from Yahoo
library(fBasics) #Basic stats
library(PerformanceAnalytics)
library(tseries) #Time series analysis (ADF test, etc)
library(testit) # To test warnings and error
library(quantmod)

# STEP 0: Defining input variables for the model ####
start.date <- 20170101   #’yyyymmdd’ In-sample; 20170101. Out-sample; 2019 - 100d data = 20180923
end.date <- 20210615   #’yyyymmdd’ In-sample; 201901001. Out-sample; 20200120


# STEP 1: Loading the data ####

# Collecting crypto data
data <- crypto_history(coin = coin1 , start_date = start.date , end_date = end.date) #repeat for each coin using loop

# Collecting equity data


# STEP 2: Treat the data ####

# Make two groups of data. Just crypto and crypto+equities
# Clean missing values

# STEP 3: Define periods ####



# STEP 4: Compute correlations ####


correlation <- function(x) {
  result <- cor(x[, 1], x[, 2])
  return(result)
}

corr.ret <- rollapply(rets, n, correlation, by.column = FALSE)
plot(corr.ret, main= "Return correlation")

corr.close <- rollapply(closes, n, correlation, by.column = FALSE)
plot(corr.close, main= "Price correlation")



# STEP 5: Conduct PCA ####

# STEP 6: Build portfolio and analyze results ####

summary(portfolio.dailyperf)
charts.PerformanceSummary(portfolio.dailyperf, main= "Performance Summary")

Return.annualized(portfolio.dailyperf)
PerformanceAnalytics::Return.cumulative(portfolio.dailyperf)
maxDrawdown(portfolio.dailyperf)
PerformanceAnalytics::sd.annualized(portfolio.dailyperf)
SharpeRatio.annualized(portfolio.dailyperf, Rf = 0)


