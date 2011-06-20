define_futures.IB <- function(symbol, exch, expiries=as.Date(Sys.time()), currency="USD", include_expired = "0", ...) {
    symout <- NULL    
    for (expiry in expiries) {
        symout <- c(symout, twsInstrument(twsFUT(symbol, exch, expiry, currency=currency, include_expired=include_expired, ...),output='symbol'))
    }
    symout
}

define_futures  <- function(symbol, exch, expiries=as.Date(Sys.time()), currency="USD", include_expired="0", src='IB', ...) {
    do.call(paste("define_futures.",src,sep=""),list(symbol=symbol,exch=exch,expiries=expiries, currency=currency, include_expired=include_expired, ...))
}

