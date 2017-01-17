# PCA and Factor Mode
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

Index = AUDUSD$Quandl.EquityIndex
pca.cov = princomp(Index)
names(pca.cov)
summary(pca.cov)
pca.cov$loadings
screeplot(pca.cov)
pca.corr=princomp(Index, cor = T)
summary(pca.corr)
pca.corr$loadings





