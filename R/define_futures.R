define_futures.IB <- function(symbols, exch, expiries=as.Date(Sys.time()), currency="USD", include_expired = "1", ...) {
    symout <- NULL    
    for (expiry in expiries) {
        for (symbol in symbols) {        
            symout <- c(symout, twsInstrument(twsFUT(symbol, exch, expiry, currency=currency, include_expired=include_expired, ...),output='symbol'))
        }    
    }
    symout
}

define_futures  <- function(symbols, exch, expiries=as.Date(Sys.time()), currency="USD", include_expired="1", src='IB', ...) {
    do.call(paste("define_futures.",src,sep=""),list(symbols=symbols,exch=exch,expiries=expiries, currency=currency, include_expired=include_expired, ...))
}

#' Create future_series ids
#'
#' @param root character vector of names of contract roots
#' @param month numeric vector of months (3 letter abbreviations also work)
#' @param year numeric vector of years
#' @return character vector of primary_ids for future_series instruments. 
#' @author Garrett See
#' @examples
#' future_id(root=c("ES","YM"), month=c(3,6,9,12), year=2010:2011)
#' #getSymbols(future_id("VX",1:12,10:11),src='cfe')
#' @export
future_id <- function(root, month, year) {
    if (!("package:qmao" %in% search() || require("qmao",quietly=TRUE))) {
        stop("Please install qmao before using this function.")
    }
    r <- ifelse(missing(root), "", paste(root,"_",sep=""))
    m <- suppressWarnings(if (all(!is.na(as.numeric(month)))) { 
            M2C()[month] 
        } else if (toupper(month) %in% toupper(C2M())) {
            M2C(month)
        } else month)
    y <- if (all(nchar(year) == 4)) {
            substr(year,3,4)
        } else if (all(nchar(year) == 1)) {
            paste("1",year,sep="")
        } else year
#    my <- paste(m,y,sep='')
    suff <- as.character(do.call(rbind, lapply(m, paste, y, sep="")))
    do.call(c, lapply(root, paste, suff, sep="_"))
}


