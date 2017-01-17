# VAR model
rm(list=ls());gc()
library(quantmod)
library(tseries)
library(TTR)
library(xts)
library(lattice)
library(timeSeries)
library(PerformanceAnalytics)
library(zoo)
library(IBrokers)
library(vars)

AUDUSD = prepareForexData(ib.duration = "8 Y", ib.barsize = "1 day", Cur1 = "AUD", Cur2 = "USD",
                          oanda.granularity = 'D', QuandlSymbol1 = 'AUS', QuandlSymbol2 = 'USA', ibAcc = 'paper')

SPY = AUDUSD$Quandl.EquityIndex$SP500
ASX = AUDUSD$Quandl.EquityIndex$ASX
x1 = cbind(SPY, ASX)
y1 = data.frame(x1)
ord.choice = VAR(y1, lag.max = 13)
summary(ord.choice, equation = "ASX")

var3.fit = VAR(x1~ar(3))
