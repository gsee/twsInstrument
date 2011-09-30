#' futures contract contstructor
#' 
#' Define one or many futures contracts for a given underlying.
#' 
#' \code{define_futures} is currently just a wrapper that calls
#' \code{define_futures.IB}
#' 
#' \code{define_futures.IB} loops through all of the symbols and expiries
#' calling twsInstrument.  The exchange has to be the same for all of the
#' futures you are trying to define.  If you want to define futures from
#' different exchanges, you must make a separate call for each.
#' 
#' The name of the future contract will usually be the value of \sQuote{local}
#' (retrieved with a call to reqContractDetails) with an underscore separating
#' the root symbol and the suffix_id.  If \code{symbol} is not found in
#' \sQuote{local} (e.g. \sQuote{VIX} is not in \sQuote{VXU1}) the suffix_id
#' will be of format MMMYY (e.g. JUN11)
#' 
#' @aliases define_futures.IB define_futures
#' @param symbols chr name of underlying
#' @param exch chr name of exchange (\sQuote{GLOBEX},\sQuote{CFE},
#' \sQuote{eCBOT}, etc)
#' @param expiries vector of type chr. YYYYMM or YYYYMMDD
#' @param currency denominating currency. default "USD"
#' @param include_expired is the contract expired? \sQuote{0} for no,
#' \sQuote{1} for yes.
#' @param src currently only sQuoteIB
#' @param \dots any other parameters to pass to twsFuture: \sQuote{primary},
#' \sQuote{currency}, \sQuote{local}, \sQuote{multiplier}, \sQuote{conId}
#' @return names of instruments that were created. Called for side-effect
#' @author Garrett See
#' @seealso twsInstrument, twsFuture, future, future_series, instrument
#' @examples
#' 
#' \dontrun{
#' define_futures("ES","GLOBEX",c("201109","201112"))
#' define_futures("ES","GLOBEX","201103",include_expired="1")
#' 
#' months <- c(paste("0",1:9,sep=""),paste(10:12))
#' define_futures("VIX","CFE",paste("2010",months,sep=""),include_expired="1")
#' 
#' define_futures("YM","eCBOT","201109")
#' 
#' define_futures(c("NKD","NQ"), "GLOBEX", "201109")
#' }
#' @export
#' @rdname define_futures.IB
define_futures.IB <- function(symbols, exch, expiries=as.Date(Sys.time()), currency="USD", include_expired = "1", ...) {
    symout <- NULL    
    for (expiry in expiries) {
        for (symbol in symbols) {        
            symout <- c(symout, twsInstrument(twsFUT(symbol, exch, expiry, currency=currency, include_expired=include_expired, ...),output='symbol'))
        }    
    }
    symout
}

#' @export
#' @rdname define_futures.IB
define_futures  <- function(symbols, exch, expiries=as.Date(Sys.time()), currency="USD", include_expired="1", src='IB', ...) {
    do.call(paste("define_futures.",src,sep=""),list(symbols=symbols,exch=exch,expiries=expiries, currency=currency, include_expired=include_expired, ...))
}



#' Create future_series ids...
#' 
#' @param root character vector of names of contract roots
#' @param month numeric vector of months (3 letter abbreviations also work)
#' @param year numeric vector of years
#' @return character vector of primary_ids for future_series instruments, OR,
#' if \code{root} is missing, a character vector of suffix_ids.
#' @author Garrett See
#' @examples
#' future_id(root=c("ES","YM"), month=c(3,6,9,12), year=2010:2011)
#' #getSymbols(future_id("VX",1:12,10:11),src='cfe')
#' future_id('VX',M2C(),10)
#' #future_id('VX',paste(M2C(),collapse=','),10)
#' @export
future_id <- function(root, month, year) {
    m <- suppressWarnings(if (all(!is.na(as.numeric(month)))) { 
            M2C()[month] 
        } else if (toupper(month) %in% toupper(C2M())) {
            M2C(month)
        } else if(!identical(integer(0), grep(",",month))) {
            #if month is a comma-delimited string of month codes (like $month_cycle)
            strsplit(month, ",")[[1]]
        } else month)
    y <- if (all(nchar(year) == 4)) {
            substr(year,3,4)
        } else if (all(nchar(year) == 1)) {
            paste("1",year,sep="")
        } else year
#    my <- paste(m,y,sep='')    
    suff <- as.character(do.call(rbind, lapply(m, paste, y, sep="")))
    if (missing(root))
        suff
    else do.call(c, lapply(root, paste, suff, sep="_"))
}


