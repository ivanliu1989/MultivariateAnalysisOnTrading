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
load('data/all_usd_related_forex.RData')
cols = c('AUDUSD', 'USDRUB', 'GBPUSD', 'USDCAD', 'USDHUF')

# settings
johansen.lookback = c(63, 126, 252, 378)
hurst.lookback = c(20, 40)
threshold = c(1, 1.2, 1.5)
adf.threshold = c(0.2, 0.3, 0.4)
jc.threshold = c(0.1, 0.15, 0.2)
hurst.threshold = c(0.499, 0.6)
stoploss = c(-0.05, 0.1)
rnd = 0
for(johansen.lookback in johansen.lookback){
    for(hurst.lookback in hurst.lookback){
        for(threshold in threshold){
            for(adf.threshold in adf.threshold){
                for(jc.threshold in jc.threshold){
                    for(hurst.threshold in hurst.threshold){
                        for(stoploss in stoploss){
                            rnd = rnd+1
                            # Backtesting -------------------------------------------------------------
                            dat.all = tail(fx.symbols.dt[, cols], 504)
                            inLong = FALSE
                            inShort = FALSE
                            initAssets = 1000000
                            posiRate = 0.05
                            initPortf = c(rep(0,21), rep(FALSE, 5), 0, initAssets, 0, initAssets)
                            initPortf = data.frame(t(initPortf))
                            
                            for(r in (johansen.lookback+1):nrow(dat.all)){
                                
                                goLong = FALSE
                                goShort = FALSE
                                closePos = FALSE
                                
                                dat = dat.all[(r-johansen.lookback):r, cols]
                                dat.log = log(dat)
                                nfx = 5
                                
                                # stats tests
                                jc.res=JohansenCointegrationTest(dat.log, type = "eigen", ecdet = 'const', K = 2)
                                ols.fit = lm(AUDUSD~. , dat.log)
                                ols.r2 = summary(ols.fit)$r.squared
                                # print(ols.r2)
                                
                                # Spreads
                                hedgeRatio = c(1, -ols.fit$coefficients[-1])
                                spreads = dat %*% t(data.frame(hedgeRatio))[1:nfx]
                                spreads = as.xts(spreads, order.by = index(dat))
                                # chart_Series(spreads)
                                spreads = na.omit(spreads)
                                
                                # stats tests 2
                                adf.res=adf.test(spreads) #0.06999
                                hurst.res=as.numeric(tail(HurstExponentTest(spreads, hurst.lookback)$hurstKY, 1)) # > 0.5 trending | < 0.5 mean reverting
                                half.life <- HalfLifeMeanReversion(spreads)$half.life.round
                                
                                # signals
                                adf.signal=adf.res$p.value
                                hurst.signal=hurst.res
                                jc.signal=as.numeric(((jc.res$p.value[1]-jc.res$r.1)/((jc.res$p.value[3]-jc.res$p.value[1])/10)+10)/100)
                                
                                # positioning
                                zScore=tail(zscores(spreads),1)
                                numUnits=-tail(zScore,1)
                                hedgeRatio = t(data.frame(hedgeRatio))
                                sizing = abs(numUnits)/1
                                posUnits = initAssets * posiRate
                                sizingVal = as.numeric(posUnits * sizing)
                                # hedgedMktVal = sizingVal / as.numeric(scale(as.numeric(hedgeRatio),center = F))
                                positions = data.frame(round(sizingVal * hedgeRatio))
                                prices = tail(dat,1)
                                names(positions) = paste0('POS.', cols)
                                # units = round(as.numeric(posUnits * sizing) / hedgeRatio * prices)
                                # units * prices * hedgeRatio
                                
                                # stats arbitragy triggers
                                if(zScore >= threshold & adf.signal <= adf.threshold & jc.signal <= jc.threshold & hurst.signal <= hurst.threshold ){
                                    goLong = FALSE
                                    goShort = TRUE # buy
                                    closePos = FALSE
                                }else if(zScore <= -threshold & adf.signal < adf.threshold){
                                    goLong = TRUE # sell
                                    goShort = FALSE
                                    closePos = FALSE
                                }else if(abs(zScore) < 0.1 | adf.signal < adf.threshold | unrealizedPnL <= stoploss){
                                    goLong = FALSE
                                    goShort = FALSE
                                    closePos = TRUE # sell OR buy
                                }
                                
                                # strategy logic
                                if(r == (johansen.lookback+1)){
                                    lstRcd = initPortf
                                    lstPos = lstRcd[12:16]
                                }else{
                                    lstRcd = Order.Book[r-johansen.lookback,]
                                    lstPos = lstRcd[12:16]
                                }
                                
                                if(goLong){
                                    pos.fnl = positions + lstPos
                                    mkt.fnl = pos.fnl * data.frame(prices)
                                    inLong = TRUE
                                    inShort = FALSE
                                }else if(goShort){
                                    pos.fnl = -positions + lstPos
                                    mkt.fnl = pos.fnl * data.frame(prices)
                                    inLong = FALSE
                                    inShort = TRUE
                                }else if(closePos & (inLong | inShort)){
                                    pos.fnl = lstPos-lstPos
                                    mkt.fnl = pos.fnl * data.frame(prices)
                                    inLong = FALSE
                                    inShort = FALSE
                                }else{
                                    pos.fnl = lstPos
                                    mkt.fnl = pos.fnl * data.frame(prices)
                                }
                                names(mkt.fnl) = paste0('MKT.', cols)
                                sumMktValue = sum(mkt.fnl)
                                actualValue = lstRcd[28] - sumMktValue
                                
                                unrealizedPnL = sum((data.frame(diff(tail(dat,2))[2,]) * lstPos)) / sum((data.frame(tail(dat,2)[1,]) * abs(lstPos)))
                                unrealizedPnL = round(ifelse(is.nan(unrealizedPnL), 0, unrealizedPnL), 5)
                                
                                
                                # PnL = sumMktValue + actualValue + unrealizedPnL
                                PnL = round(sum((data.frame(diff(tail(dat,2))[2,]))) / sum((data.frame(tail(dat,2)[1,]))),5)
                                
                                orderBook = data.frame(tail(dat,1), adf.signal, jc.signal, hurst.signal, half.life, zScore, ols.r2, 
                                                       pos.fnl, mkt.fnl, inLong, inShort, goLong, goShort, closePos, 
                                                       sumMktValue, actualValue, unrealizedPnL, PnL)
                                names(orderBook) = c(paste0('P.', names(dat)), 'ADF', 'Johansen', 'Hurst', 'H.L', 'ZScore', 'ols.r2', 
                                                     paste0('Pos.', names(dat)), paste0('Mkt.', names(dat)),
                                                     'inLong', 'inShort', 'goLong', 'goShort', 'closePos', 
                                                     'sumMktValue', 'actualValue', 'unrealizedPnL', 'PnL')
                                if(r == (johansen.lookback+1)){
                                    names(initPortf) = names(orderBook)
                                    Order.Book = rbind(initPortf, orderBook)
                                }else{
                                    Order.Book = rbind(Order.Book,orderBook)
                                }
                                
                                # cat(paste0('\n', unrealizedPnL, ' | ', PnL))
                            }
                            
                            Order.Book = as.xts(Order.Book[-1,], order.by = as.Date(rownames(Order.Book[-1,])))
                            ret = Order.Book[,c('unrealizedPnL', 'PnL', 'ADF')]
                            pnl = sum(ret$unrealizedPnL)
                            sharpe = (mean(ret[ret[,1]!=0, 1])-mean(ret[ret[,1]!=0,2]))/sd(ret[ret[,1]!=0,1])
                            trades = length(ret[,1]!=0)
                            
                            rcd = data.frame(johansen.lookback, hurst.lookback, threshold, adf.threshold, jc.threshold, hurst.threshold, stoploss, trades, pnl, sharpe)
                            print(rcd)
                            if(rnd == 1){
                                rcd.all = rcd
                            }else{
                                rcd.all = rbind(rcd.all, rcd)    
                            }
                        }
                    }
                }
            }
        }
    }
}

