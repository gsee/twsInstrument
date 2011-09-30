#' exchange_rate class constructor
#' 
#' Quickly define several exhange rate instruments.
#' 
#' If nothing is given, 11 currency pairs will be defined: "EUR.USD",
#' "USD.JPY", "GBP.USD", "USD.CHF", "AUD.USD", "USD.CAD", "NZD.USD", "USD.HKD",
#' "USD.SGD", "GBP.JPY", "EUR.JPY" If \code{pairs} is given, it should be a
#' vector of pair names formatted in the same manner as above. If \code{pairs}
#' is not given, but \code{base_currencies} and \code{quote_currencies} are,
#' then it will make all base/quote pairs possible using those. If only
#' \code{counter_currencies} are given, then if only one currency exists in the
#' .instrument environment, it will be used as the counter_currency. Otherwise,
#' "USD" will be used.
#' 
#' @aliases define_FX define_exchange_rates
#' @param pairs chr names of currency pairs to be defined. Each currency pair
#' name should be the base currency and base currency, separated by a period
#' e.g. EUR.USD Optional.
#' @param counter_currencies chr names of first currencies of pairs to define.
#' Optional
#' @param second_currencies chr names of second currencies of pairs to define.
#' Optional
#' @param use.IB call update_instruments.IB ?
#' @param verbose if TRUE, it will print to screen every currency and
#' exchange_rate that it defines.
#' @return Called for side-effect
#' @note Currently, this function is using a dot to separate the 2 currencies
#' in the exchange_rate primary_id. FinancialInstrument and blotter do not use
#' (expect) a separator. In the near future, either this function will create
#' primary_ids for exchange_rates that look like "EURUSD", or
#' FinancialInstrument/blotter will be updated to expect primary_ids that look
#' like "EUR.USD"
#' @author Garrett See
#' @seealso exchange_rate, ls_exchange_rates(), twsInstrument
#' @examples
#' 
#' \dontrun{
#' #make EUR.USD, EUR.JPY, GBP.USD, GBP.JPY
#' define_FX(counter_currencies=c("EUR","GBP"), second_currencies=c("USD","JPY")) 
#' 
#' define_FX(c("USD.CHF","NZD.USD))
#' 
#' define_FX()
#' }
#' @export
define_FX <- define_exchange_rates <- function(pairs, counter_currencies=NULL, second_currencies=NULL, use.IB=TRUE, verbose=FALSE) {
    if (missing(pairs)) {
        if (is.null(counter_currencies) && is.null(second_currencies) ) { #nothing is given
            pairs <- c("EURUSD","USDJPY","GBPUSD","USDCHF","AUDUSD","USDCAD","NZDUSD","USDHKD","USDSGD","GBPJPY","EURJPY")
        } else {
            pairs <- NULL
            if (is.null(second_currencies)) {
                if (length(ls_currencies())==1) {
                    second_currencies <- ls_currencies()[1]
                } else second_currencies <- "USD"
            }
            for (qc in second_currencies) {
                pairs <- c(pairs, paste(counter_currencies, qc, sep=""))
            }
        }    
    }
    lastc <- function(x) substr(x,nchar(x),nchar(x))
    if (!(all(nchar(pairs) == 6))) { #not formatted like "EURUSD"    
        if (all(do.call(c,lapply(strsplit(pairs,"/"),length)) == 2)) { #EUR/USD
            pairs <- gsub("/","",pairs)
        } else if (all(do.call(c,lapply(strsplit(pairs,"\\."),length)) == 2)) { #EUR.USD
            pairs <- gsub("\\.","",pairs)
        } else if (all(nchar(pairs) == 4)
                    && all(do.call(c, lapply(pairs,lastc)) == ".")) {#EUR.
            pairs <- paste(substr(pairs,1,3),"USD",sep="")
        } else stop("Invalid pair.")
    }
    for (pair in pairs) {
        Ccy <- substr(pair,4,6) #strsplit(pair,"\\.")[[1]][2]
        cCcy <- substr(pair,1,3) #strsplit(pair,"\\.")[[1]][1]
        for (ccy in c(Ccy,cCcy)) {
            tmp <- try(getInstrument(ccy,silent=TRUE))
            if (inherits(tmp,'try-error') || !inherits(tmp,'currency')) {
                if (verbose) cat(paste("Created currency ", ccy, ". \n", sep=""))             
                currency(ccy)
            }
        }
        if (verbose) cat('Created exchange_rate ', pair, "\n", sep="")
        instrument.tws(primary_id=pair, 
                currency=Ccy,
                multiplier=1,
                tick_size=0.01,
                counter_currency = cCcy,
                type = c("exchange_rate","currency"), assign_i = TRUE)            
    }
    if (use.IB) update_instruments.IB(pairs)
}

