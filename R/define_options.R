#' make primary_ids for options
#' 
#' create a primary_id for options. The only difference between the ids this generates, and
#' those of the Option Symbology Initiative (OSI) is that these have an underscore separating 
#' the primary_id from the suffix_id. The format for a 125 strike call on SPY expiring in 
#' December of 2011 would be \sQuote{SPY_111217C120}.  The first 6 digits of the suffix correspond 
#' to the expiration YYMMDD where DD is calculated as the Saturday following the 3rd Friday of the month.
#'
#' @param underlying_id chr vector of names of underlyings.
#' @param strike vector of strike prices
#' @param month numeric vector of months (partial matches to \code{month.name} will also work)
#' @param year numeric vector of years (can be 1 digit, 2 digit, or 4 digit years)
#' @param right \sQuote{C} or \sQuote{P} (\sQuote{call} and \sQuote{put} will also work)
#' @return chr vector of primary_ids (or suffix_ids if underlying_id is missing, NULL, or \dQuote{}) for option_series
#' @author Garrett See
#' @note does not support weekly or EOM options
#' @TODO support weekly and EOM options
#' @seealso future_id, build_series_symbols, build_spread_symbols
#' @examples
#' option_id("SPY",125,'Sep',2011,'C')
#' option_id("SPY",130,1:12,11,'C')
#' option_id(c("SPY","DIA"),seq(120,130,5))
#' @export
option_id <- function(underlying_id, 
                    strike,
                    month, 
                    year,
                    right=c("C","P"))
{
    if (missing(month)) month=format(Sys.Date(),"%m")
    if (missing(year)) year=format(Sys.Date(),"%Y")  
    m <- suppressWarnings(if (all(!is.na(as.numeric(month)))) {
            as.numeric(month)
         } else pmatch(toupper(month),toupper(month.name)))
    y <- if (all(nchar(year) == 2)) {
                as.numeric(year)+2000
            } else if (all(nchar(year) == 1)) {
                as.numeric(year)+2010
            } else as.numeric(year)
    DT <- as.Date(paste(outer(y,sprintf("%02d",m),paste,sep="-"),01,sep="-"),format='%Y-%m-%d')
    #for each of these dates, get expiration Saturday
    ExpSat <- sort(as.Date(sapply(DT, 
                                  FUN=function(x) { 
                                        DS <- x+0:22; 
                                        ds <- which(weekdays(DS)=="Friday")[3]+1; 
                                        DS[ds]})))
    right <- toupper(gsub("call","C",right,ignore.case=TRUE)) #replace 'call' and 'put' with "C" and "P"
    right <- toupper(gsub("put","P",right,ignore.case=TRUE))
    suff <- as.character(outer(format(ExpSat,"%y%m%d"),right,paste,sep=""))
    SUFF <- do.call(c,lapply(strike, FUN=function(x) paste(suff,x,sep="")))
    #sapply(underlying_id,FUN=function(x) paste(x,SUFF,sep="_"))
    if (missing(underlying_id) || is.null(underlying_id) || underlying_id=="") {
         do.call(c, lapply(underlying_id,FUN=function(x) paste(x,SUFF,sep="")))
    } else do.call(c, lapply(underlying_id,FUN=function(x) paste(x,SUFF,sep="_")))
}


#' define option_series with IB
#'
#' define option_series instruments using IBrokers
#'
#' a wrapper for twsInstrument to define multiple options contracts.
#' 
#' @param underlying_id vector of underlying_ids
#' @param strike vector of strike prices
#' @param expires vector of expiration dates of format YYYYMM
#' @param right \code{"C"}, \code{"P"}, or \code{c("C","P")}
#' @param currency name of currency
#' @param multiplier contract multiplier (usually 100 for equity options)
#' @param include_expired \dQuote{0} if you do not want to define expired contracts
#' @param \dots other arguments to pass through to \code{\link{twsInstrument}}
#' @return called for side-effect
#' @author Garrett See
#' @note the date in the primary_id will correspond to Expiration Saturday, however, the Date in the 
#' $expires slot (and in $IB$expiry) will correspond to the Friday prior, which is the last day trading occurs.
#' @seealso \code{\link{define_options}}, \code{\link{option_series.yahoo}}
#' @examples
#' \dontrun{
#' define_options.IB('SPY',stike=125)
#' define_options.IB(c("SPY","DIA"),strike=seq(120,130,5))
#' define_options('GOOG',600,c('201109','201112'),'C',src='IB')
#' }
#' @export
define_options.IB <- 
function(underlying_id, 
            strike,
            expires,
            right=c("C","P"),
            currency="USD", 
            multiplier=100,
            include_expired='1',
            ...) 
{
    if (missing(expires)) expires <- format(Sys.Date(),"%Y%m")
    symout <- NULL
    for (id in underlying_id) {
        for (expiry in expires) {
            expiry <- gsub("-","",expiry)
            if (nchar(expiry) == 8) {
                expiry <- as.Date(expiry,format='%Y%m%d')            
                if (weekdays(expiry) == "Saturday") expiry <- format(expiry - 1,"%Y%m%d")
            }
            for (k in strike) {
                for (rt in right) {
                    symout <- c(symout, 
                                twsInstrument(twsOPT(local="",
                                                    expiry=expiry,
                                                    strike=k, 
                                                    right=switch(rt, C=,c=,call=,Call=,CALL='C',
                                                                     P=,p=,put=,Put=,PUT='P'), 
                                                    symbol=id, 
                                                    multiplier=as.character(multiplier),
                                                    include_expired=include_expired),
                                                    output='symbol', ...))
                }
            }
        }
    }
    symout
}

#' create twsInstruments for options on a given underlying
#'
#' define option_series instruments.
#'
#' wrapper to call either \code{\link{define_options.IB}} or \code{\link{define_options.yahoo}}
#' @return called for side-effect
#' @author Garrett See
#' @seealso \code{\link{define_options.IB}}, \code{\link{define_options.yahoo}}
#' option_series.yahoo,
#' option, option_series, twsInstrument
#' @param \dots arguments to pass to other methods
#' @param src where to get option_series metadata; currently only \dQuote{IB} and \dQuote{yahoo} are implemented
#' @examples
#' \dontrun{
#' define_options("SPY",strike=125, src='IB')
#' define_options("SPY",src='yahoo')
#' }
define_options <- function(..., src='IB') { 
    do.call(paste('define_options',src,sep="."), list(...))
}

#' define option_series instruments using yahoo
#' @param symbol chr name of underlying instrument
#' @param currency chr name of currency
#' @param multiplier contract multiplier (usually 100 for equity options)
#' @param tick_size minimum price change
#' @return called for side-effect
#' @author Garrett See
#' @seealso option_series.yahoo, \code{\link{define_options}}, \code{\link{define_options.IB}}
#' @examples
#' \dontrun{
#' define_options("SPY",src='yahoo')
#' }
#' @export
define_options.yahoo <- function(symbol, currency="USD", multiplier=100, tick_size=NULL) { 
#    primary_id <- paste(".",symbol,sep="")
    
#    if (!is.instrument(primary_id)) {
#        option(primary_id=primary_id, currency=currency,
#              multiplier=multiplier, identifiers=identifiers, 
#              underlying_id=symbol)
#    }
    opts <- getOptionChain(symbol)
    ids <- NULL
    for (r in c(rownames(opts$calls),rownames(opts$puts)) ) {
        si <- gsub(symbol,"",r)
		#id <- paste(primary_id,suffix_id,sep="_")
        expiry <- paste('20',substr(si,1,6),sep="")
        right <- substr(si,7,7)
        strike <- as.numeric(substr(si,8,15))/1000
#        local <- paste(symbol, si, sep="   ")      
		clean.si <- paste(expiry,right,strike,sep="")		
		primary_id <- paste(symbol, "_", clean.si, sep="")
#		ids <- c(ids, primary_id)
        #create option spec if we need to.
		tmpInstr <- try(getInstrument(paste('.',symbol,sep=""),silent=TRUE),silent=TRUE)
		if (!inherits(tmpInstr, "option")) {
			warning(paste('Created option specs for root',paste('.',symbol,sep="")))
            instrument.tws(primary_id=paste('.',symbol,sep=""), currency=currency,
					multiplier=multiplier, tick_size=tick_size, identifiers=NULL,
                    type = "option", underlying_id=symbol, assign_i = TRUE)			
            #option(primary_id=paste('.',symbol,sep=""), currency=currency,
			#		multiplier=multiplier, tick_size=tick_size, 
			#		underlying_id=symbol)		
		}
        instrument.tws(primary_id=primary_id, 
				suffix_id=clean.si, 
				#first_trade=first_traded, 
				currency=currency, 
				multiplier=multiplier, 
				tick_size=tick_size, 
				expiry=expiry, 
				right=right,	
				strike=strike, 
				underlying_id=symbol, 
				type = c("option_series","option"), 
				defined.by='yahoo', assign_i=TRUE)    
#		option_series(primary_id=primary_id, suffix_id=si, exires=expiry, currency=currency,
#                        callput = switch(right,C='call',P='put'))
    }
#    if (use.IB) {
#        update_instruments.IB(ids)
#    }
}


