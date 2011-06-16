define_futures.IB <- function(symbol, exch, expiries=as.Date(Sys.time()), ...) {
    symout <- NULL    
    for (expiry in expiries) {
        symout <- c(symout, twsInstrument(twsFUT(symbol,exch,expiry,...),output='symbol'))
    }
    symout
}

