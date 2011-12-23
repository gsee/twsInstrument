#' futures contract constructor
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


#' Extract root \code{future} from \code{future_series}
#' 
#' Construct a \code{future} from a \code{future_series}
#' 
#' Use this if you have defined a future_series object using \code{define_futures}, 
#' or deleted the root \code{future} and you would like to define the root.
#' @param x \code{\link[FinancialInstrument]{future_series}}, or name of 
#' \code{\link[FinancialInstrument]{future_series}} from which to extract \code{\link[FinancialInstrument]{future}}
#' @param future_id Can be any string to use as \code{primary_id} of the newly created \code{\link[FinancialInstrument]{future}}.
#' Alternatively, it can be \dQuote{root}, \dQuote{exchange_id}, or \dQuote{marketName}.  
#' If \code{future_id} is missing, or if it is \dQuote{root}, it will become the root as determined by a call to
#' \code{\link[FinancialInstrument]{parse_id}}.  If it is \dQuote{exchange_id}, or \dQuote{marketName} and either
#' of those fields exist in the \code{future_series}, the value of that field will be used as the \code{future_id}
#' @param assign_i should the future instrument be stored in the \code{.instrument} environment?
#' @param overwrite TRUE/FALSE if an instrument already exists by the same name, should it be overwritten? (Default=FALSE)
#' @param identifiers list of identifers to add to the \code{future}
#' @param ... any other parameters to pass through to \code{instrument}
#' @return a \code{\link[FinancialInstrument]{future}} object unless called with \code{assign_i=TRUE}
#' in which case the \code{future} will be stored and only the \code{primary_id} will be returned.
#' @author Garrett See
#' @seealso \code{\link[FinancialInstrument]{instrument}}, \code{\link[FinancialInstrument]{future}},
#' \code{\link[FinancialInstrument]{future_series}}
#' @examples
#' \dontrun{
#' s <- front_future("ES")
#' rm_futures("ES") #delete the root that front_future automatically created
#' extract_future(s)
#' extract_future(s, extra_field="custom") # can add any extra arbitrary fields
#'
#' currency(c("USD", "AUD"))
#' define_futures("AUD", "GLOBEX", "201112", include_expired=1)
#' extract_future("AUD_Z1") # not assigned
#' extract_future("AUD_Z1", assign_i=TRUE) #root stored in "..AUD"
#' getInstrument("AUD") #Oh no, that's the curreny
#' getInstrument("AUD", type='future') #specify type to get the root
#'
#' extract_future("AUD_Z1", future_id='X6A', assign_i=TRUE, identifiers=list(CME='6A'))
#' getInstrument("6A")
#'
#' # The next line would replace "AUD" the currency with a future
#' # extract_future("AUD_Z1", future_id='AUD', assign_i=TRUE, identifiers=list(CME='6A'), overwrite=TRUE)
#' }
#' @export
extract_future <- function(x, future_id, assign_i=FALSE, overwrite=FALSE, identifiers=list(), ...) {
    instr <- if (is.instrument(x)) {
        x
    } else getInstrument(x, type='future_series')
    parsed_root <- parse_id(instr$primary_id)$root
    if (missing(future_id)) {
        future_id <- parsed_root
    } else if (future_id == "exchange_id" || future_id == "marketName") {
        if (!is.null(instr$marketName)) {
            future_id <- instr$marketName
        } else if (!is.null(instr$exchange_id)) {
            future_id <- instr$exchange_id
        } else {
            warning('future_series does not contain ', future_id, " parsing primary_id for root instead.")
            future_id <- parsed_root <- parse_id(instr$primary_id)$root
        }
    }
    args <- list()
    args$primary_id <- future_id
    # stuff that gets passed through instrument dots
    if (!is.null(instr$exchange)) args$exchange = instr$exchange
    if (!is.null(instr$marketName)) {
        args$exchange_id = instr$marketName
        args$identifiers <- c(identifiers, list(exchange_id=args$exchange_id))
    }
    if (future_id != parsed_root) {
        args$identifiers <- c(identifiers, args$identifiers, root_id=parsed_root)
    }
    if (!is.null(instr$longName)) args$description = instr$longName
    if (!is.null(instr$priceMagnifier)) args$priceMagnifier = instr$priceMagnifier
    if (!is.null(instr$timeZoneId)) args$timeZoneId = instr$timeZoneId
    if (!is.null(instr$tradingHours) && instr$tradingHours != '') args$tradingHours = instr$tradingHours
    if (!is.null(instr$liquidHours) && instr$liquidHours != '') args$liquidHours = instr$liquidHours
    if (!is.null(instr$electronic_start)) args$electronic_start = instr$electronic_start
    if (!is.null(instr$electronic_end)) args$electronic_end = instr$electronic_end
    if (!is.null(instr$primary_start)) args$primary_start = instr$primary_start
    if (!is.null(instr$primary_end)) args$primary_end = instr$primary_end
    args$defined.by = paste(unique(c(instr$defined.by, 'IBx')), collapse=';')
    args$updated <- if (!is.null(instr$updated)) {
        instr$updated
    } else Sys.time()
    if (!is.null(instr$underlying_id)) args$underlying_id = instr$underlying_id
    args <- c(args, list(...))
    # end dots
    args$currency <- instr$currency
    args$multiplier <- instr$multiplier
    args$tick_size <- instr$tick_size

    out <- if (assign_i && !overwrite) {
        do.call(future, args)
    } else {
        args$assign_i <- assign_i
        args$type <- instr$type[-1]  #args$type = 'future'
        do.call(instrument, args)
    }
    out
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


