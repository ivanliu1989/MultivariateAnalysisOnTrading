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
source('R/ibForexMidData.R')
USD.Symbols = read.csv('data/USD_Related_FX.csv')

fx.symbols = list()
for(n in 1:nrow(USD.Symbols)){
    cur1 = USD.Symbols[n, 1]
    cur2 = USD.Symbols[n, 2]
    sym = paste0(USD.Symbols[n, 1], USD.Symbols[n, 2])
    print(sym)
    
    fx.symbols[[sym]] = ibForexMidData(ib.duration = "10 Y", ib.barsize = "1 day", Cur1 = cur1, Cur2 = cur2, ibAcc = 'paper')
    
    if(cur1 == 'USD'){
        fx.symbols[[sym]] = 1/fx.symbols[[sym]]
    }
    
    index(fx.symbols[[sym]]) = as.Date(index(fx.symbols[[sym]]))
    
    if(n == 1){
        fx.symbols.dt = Cl(fx.symbols[[sym]])
    }else{
        fx.symbols.dt = merge(fx.symbols.dt, Cl(fx.symbols[[sym]]))   
    }
}

names(fx.symbols.dt) = names(fx.symbols)
save(fx.symbols, fx.symbols.dt, file = 'data/all_usd_related_forex.RData')
load('data/all_usd_related_forex.RData')

# Select most important pairs
# fx.symbols.dt = na.omit(fx.symbols.dt)
fx.symbols.dt = log(fx.symbols.dt)
cols = c('AUDUSD', 'USDRUB', 'USDMXN', 'KRWUSD', 'GBPUSD', 'USDCAD', 'USDSEK', 'USDHUF') # 'NZDUSD', 'USDNOK', 

# Eigenvector and Johansen
johansen.lookback = 252
hurst.lookback = 64
t = nrow(fx.symbols.dt)
dat = fx.symbols.dt[(t-johansen.lookback):t, cols]
jc.res=JohansenCointegrationTest(dat, type = "eigen", ecdet = 'const', K = 2)
ols.fit = lm(AUDUSD~. , dat)
summary(ols.fit)

# Spreads
eigenv = jc.res$jc.test@V[, 1]
spreads = dat %*% t(data.frame(eigenv))[1:8]
spreads = as.xts(spreads, order.by = index(dat))
chart_Series(spreads)

adf.res=adf.test(na.omit(spreads)) #0.06999
hurst.res=as.numeric(tail(HurstExponentTest(spreads, hurst.lookback)$hurstKY, 1)) # > 0.5 trending | < 0.5 mean reverting
half.life <- HalfLifeMeanReversion(spreads)$half.life.round
