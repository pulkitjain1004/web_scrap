# Created by Pulkit Jain
# Creation Date: 2/18/2018
# Purpose: Coding Assesment for Big Data Federation. Analysis of Gold/Silver Prices.

# Clear Workspace
# rm(list = ls(all = TRUE)) 

library("magrittr")
library("XML")
library("rvest")
library("dplyr")
library("ggplot2")
library("astsa")
# library("httr")
# library("RCurl")
source("source_1.R")


# Part I
# Read Data from web and save as an external file

url_silver <- "http://www.investing.com/commodities/silver-historical-data"
url_gold <- "http://www.investing.com/commodities/gold-historical-data"

getData(url_gold, url_silver)

# Part II

# Read data from saved file
# Function to provide mean and variance for a given range of dates
# To get mean and variance from a date range, substitute values in the function
# getCommodityPrice("stDate", "endDate", "commodity")

getCommodityPrice("2018-01-25", "2018-02-14", "Gold")

# Part III
# Historical Analysis

# Execute only one of them

commodity <- "Gold"
commodity <- "Silver"

data_prices <- as.data.frame(readData(commodity)) %>% select(Price)

ggplot(data = data_prices, aes(x = 1:nrow(data_prices),y = data_prices)) + 
  geom_line() +
  # ylim(1000, (max(data_prices)+100)) +
  labs(x = "Day",
       y = "Price (USD)",
       title = paste0("Time Series Plot of Commodity ",commodity," Price"))

# Convert in time-series
data_prices_ts <- ts(data_prices)


# Regress on time
trModel <- lm(data_prices_ts ~ c(1:length(data_prices_ts)))
summary(trModel)
par(mfrow = c(2,2))
plot(trModel, main = paste0("Diagnostics Plot for ",commodity))
graphics.off()

# Now the series looks stationary
# trModel$residuals for detrended series

# Correlation Plots
lag1.plot(data_prices_ts,6)
# For Gold: strong correlation with lag 1, a bit with lag 2.

acf(trModel$residuals, main = paste0("Auto-correlation plot for de-trended ",commodity))
pacf(trModel$residuals, main = paste0("Partial-correlation plot for de-trended ",commodity))

# Differenced data
# Differencing to remove upward trend
# Check relation to predict difference

plot(diff(data_prices_ts, lag = 1), 
     main = paste0("Plot for Differenced ",commodity))
abline(h = 0, col = "red", lty = 3)

acf( diff(data_prices_ts, lag = 1), 
     main = paste0("Auto-correlation plot for Differenced ",commodity) )
pacf( diff(data_prices_ts, lag = 1), 
      main = paste0("Partial-correlation plot for Differenced ",commodity) )

# ARCH GARCH
acf( (diff(data_prices_ts, lag = 1))^2, 
     main = paste0("ARCH plot for Differenced ",commodity) )
pacf( (diff(data_prices_ts, lag = 1))^2 , 
      main = paste0("GARCH plot for Differenced ",commodity) )


# Do we need log?
plot(data_prices_ts, 
     main = paste0("Plot for ",commodity))
plot(log(data_prices_ts), 
     main = paste0("Plot for Log ",commodity))

plot(diff(log(data_prices_ts), lag = 1), ylim = c(-0.1, 0.1), 
     main = paste0("Plot for Differenced Log ",commodity))
abline(h = 0, col = "red", lty = 3)

acf(log(data_prices_ts), 
    main = paste0("Auto-correlation plot for Log ",commodity))
pacf(log(data_prices_ts), 
     main = paste0("Partial-correlation plot for Log ",commodity))


# Multi variate series

hist_data <- read.csv("hist_data.csv", header = T, stringsAsFactors = F)
# file hist_data.csv has been manually copied, saved from investing.com

str(hist_data)
# Convert Platinum, Gold and SnP to numbers
hist_data$Platinum <- gsub(",", "", hist_data$Platinum)
hist_data$Platinum <- as.numeric(hist_data$Platinum)

hist_data$Gold <- gsub(",", "", hist_data$Gold)
hist_data$Gold <- as.numeric(hist_data$Gold)

hist_data$SnP <- gsub(",", "", hist_data$SnP)
hist_data$SnP <- as.numeric(hist_data$SnP)

# Convert to time series data
hist_data$CRB <- ts(hist_data$CRB)
hist_data$Platinum <- ts(hist_data$Platinum)
hist_data$Silver <- ts(hist_data$Silver)
hist_data$Gold <- ts(hist_data$Gold)
hist_data$SnP <- ts(hist_data$SnP)
hist_data$USDX <- ts(hist_data$USDX)
hist_data$Inf. <- ts(hist_data$Inf.)

# Make them Stationary
ts.plot(diff(log(hist_data$Gold)), main = "Stationary time-series for Gold", ylab = "diff( log (Value))" )
ts.plot(diff(log(hist_data$CRB)), main = "Stationary time-series for CRB", ylab = "diff( log( Value))")
ts.plot(diff(log(hist_data$Inf.)), main = "Stationary time-series for Inflation", ylab = "diff( log( Value))")
ts.plot(diff(log(hist_data$SnP)), main = "Stationary time-series for SnP 500", ylab = "diff( log( Value))")
ts.plot(diff(log(hist_data$USDX)), main = "Stationary time-series for USDX", ylab = "diff( log( Value))")
ts.plot(diff(log(hist_data$Silver)), main = "Stationary time-series for Silver", ylab = "diff( log( Value))")
ts.plot(diff(log(hist_data$Platinum)), main = "Stationary time-series for Platinum", ylab = "diff( log( Value))")

# Get CCF Plots
ccf(diff(log(hist_data$Gold)), diff(log(hist_data$CRB)), lag.max = 15, main = "CCF for Gold vs CRB")
ccf(diff(log(hist_data$Gold)), diff(log(hist_data$Inf.)), lag.max = 15, main = "CCF for Gold vs Inflation")
ccf(diff(log(hist_data$Gold)), diff(log(hist_data$SnP)), lag.max = 15, main = "CCF for Gold vs SnP")
ccf(diff(log(hist_data$Gold)), diff(log(hist_data$USDX)), lag.max = 15, main = "CCF for Gold vs USDX")
ccf(diff(log(hist_data$Gold)), diff(log(hist_data$Silver)), lag.max = 15, main = "CCF for Gold vs Silver")
ccf(diff(log(hist_data$Gold)), diff(log(hist_data$Platinum)), lag.max = 15, main = "CCF for Gold vs Platinum")

# Check Significance positive lags p-values in linear regression
# Lags: CRB(+7), Inflation(+2), SnP(+3), USDX(+6), Silver(+4), Platinum(+6)
dat_modif = ts.intersect(dfGold = diff(log(hist_data$Gold)), 
                         dfCRB = stats::lag(diff(log(hist_data$CRB)), 7),
                         dfInf = stats::lag(diff(log(hist_data$Inf.)), 2), 
                         dfSnP = stats::lag(diff(log(hist_data$SnP)), 3),
                         dfUSDX = stats::lag(diff(log(hist_data$USDX)), 6), 
                         dfSilv = stats::lag(diff(log(hist_data$Silver)), 4),
                         dfPlat = stats::lag(diff(log(hist_data$Platinum)), 6) )

# Fitting linear regression to see what variables are significant
lm1 = lm(dfGold~., data = dat_modif)
summary(lm1)
par(mfrow = c(2,2))
plot(lm1)
graphics.off()

