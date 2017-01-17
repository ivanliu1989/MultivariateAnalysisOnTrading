ibForexMidData = function(ib.duration = "10 Y", ib.barsize = "1 day", Cur1 = "AUD", Cur2 = "USD", ibAcc = 'paper'){
    library(IBrokers)
    library(RQuantAPI)
    
    instrument = paste0(Cur1, '_', Cur2)
    if(ibAcc == 'live'){
        tws <- twsConnect(port = 7496, clientId = 999)
    }else{
        tws <- twsConnect(port = 7497, clientId = 998)
    }
    
    # IB OHLC -----------------------------------------------------------------
    ccy <- reqContractDetails(tws, twsCurrency(Cur1, Cur2))[[1]]$contract
    cols = c('Open', 'High', 'Low', 'Close')
    
    IB.MID <- reqHistoricalData(conn = tws, Contract = ccy, barSize = ib.barsize,
                                duration = ib.duration, useRTH = "1", whatToShow='MIDPOINT')
    IB.MID = IB.MID[, 1:4]
    IB.MID = IB.MID[!duplicated(index(IB.MID)), ]
    colnames(IB.MID) = paste0('IB.M.', cols)
    
    twsDisconnect(tws)
    return(IB.MID)
}