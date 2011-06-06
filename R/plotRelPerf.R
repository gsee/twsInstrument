
plotRelPerf <- function(symbols,prefer=NULL, env=.GlobalEnv) {
    x <- xts()
    if (length(symbols) <= 1) stop('vector of symbol names required')    
    for (symbol in symbols) { 
        x <- cbind(x,cumsum(ROC(getPrice(na.omit(get(symbol,pos=env)),prefer=prefer),na.pad=FALSE)))
    }
    ts.plot(x,col=rainbow(NCOL(x)))
}

#symbols <- c('SPY','DIA','QQQ')
#getSymbols(symbols)
#plotRelPerf(symbols)

