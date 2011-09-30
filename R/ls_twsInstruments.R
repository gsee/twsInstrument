#TODO: create FX class to use instead of exchange.rate

#TODO: ls_instruments()[match(c('GS','SEE'),ls_instruments())]




#' display the names of or delete twsInstruments.
#' 
#' ls functions return the names of all the instruments of the class implied by
#' the function name. rm functions remove the instruments of the class implied
#' by the function name
#' 
#' \code{rm_twsInstruments}, and \code{rm_non_twsInstruments} will not delete
#' currencies unless the keep.currencies argument is FALSE.
#' 
#' For the rm functions, x can be a vector of instrument names, or nothing.  If
#' x is missing, all instruments of the relevant type will be removed.
#' 
#' ls_yahoo and ls_IB look for instruments with \sQuote{yahoo} or \sQuote{IB}
#' in the defined.by slot, respectively Equivalently,
#' \code{ls_defined.by("yahoo")} or \code{ls_defined.by("IB")} could be used.
#' Note that being defined.by IB does does not necessarily mean that the
#' instrument is a twsInstrument (i.e. the details may have been updated by IB,
#' without an IB slot being added.)
#' 
#' Often is is useful to nest these functions.
#' 
#' @aliases ls_twsInstruments ls_non_twsInstruments ls_defined.by ls_yahoo
#' ls_IB rm_twsInstruments rm_non_twsInstruments
#' @param pattern an optional regular expression.  Only names matching
#' \code{pattern} are returned.
#' @param match return only exact matches?
#' @param x For the rm_ functions: x is what to remove. if not supplied all
#' instruments of relevent class will be removed For \code{ls_defined.by} x is
#' the string describing how the instrument was defined.
#' @param keep.currencies If TRUE, currencies will not be deleted.
#' @return ls functions return vector of character strings corresponding to
#' instruments of requested type rm functions are called for side-effect
#' @author Garrett See
#' @seealso ls_instruments, ls_currencies, ls_instruments_by, ls, rm,
#' twsInstrument, instrument, stock, future, option, currency
#' @examples
#' 
#' \dontrun{
#' rm_instruments(keep.currencies=FALSE) #remove everything from .instrument
#' 
#' # First, create some instruments
#' currency('USD')
#' currency('EUR')
#' currency('JPY')
#' #stocks
#' stock("S","USD")
#' stock('SPY','USD',1)
#' twsInstrument('XOM') #requires Interactive Brokers
#' twsInstrument(option(primary_id='XOM', currency='USD', multiplier=100,
#'     tick_size=0.01, expiry='201106', right='P', strike=80, underlying_id='XOM'))
#' 
#' # Now, the examples
#' ls_twsInstruments() #all instruments containing IB slot with twsContract object
#' ls_non_twsInstruments()
#' 
#' ls_defined.by('IB')
#' ls_IB() #same as above
#' ls_defined.by('IB', ls_options())
#' 
#' ls_yahoo()
#' 
#' rm_twsInstruments()
#' rm_yahoo()
#' rm_instruments() #remove all but currencies
#' rm_currencies()
#' }
#' @export
#' @rdname ls_twsInstruments
ls_twsInstruments <- function(pattern=NULL, match=TRUE) {
    symbols <- ls_instruments(pattern, match)
    tmp_symbols <- NULL            
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
        if (inherits(tmp_instr, 'twsInstrument') || !is.null(tmp_instr$IB) ) {
            tmp_symbols <- c(tmp_symbols,instr)
        }    
    }
    tmp_symbols
}

#' @export
#' @rdname ls_twsInstruments
ls_non_twsInstruments <- function(pattern=NULL) {
    symbols <- ls_instruments(pattern, match)
    tmp_symbols <- NULL            
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
        if (!inherits(tmp_instr, 'twsInstrument') && is.null(tmp_instr$IB) ) {
            tmp_symbols <- c(tmp_symbols,instr)
        }    
    }
    tmp_symbols
}


#TODO, make a ls_defined.by function that lets the user provide the defined.by string similar to ls_by_currency function.
# e.g. ls_defined.by(x="yahoo", pattern=NULL)

# should it be ls_yahoo, ls_defined.by.yahoo, or ls_src? something else?

#' @export
#' @rdname ls_twsInstruments
ls_yahoo <- function(pattern=NULL) {
#instruments defined by yahoo
    symbols <- ls_instruments(pattern) #TODO: other functions should be updated to get symbols like this too   
    tmp_symbols <- NULL
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
        if ( is.instrument(tmp_instr) && !is.null(tmp_instr$defined.by) )  {        
            dby <- unlist(strsplit( tmp_instr$defined.by,";"))    
            if (any(dby == "yahoo" )) 
                tmp_symbols <- c(tmp_symbols, instr)
        }
    }
    tmp_symbols
}

#' @export
#' @rdname ls_twsInstruments
ls_IB <- function(pattern=NULL) {
#instruments defined by IB
    symbols <- ls_instruments(pattern)   
    tmp_symbols <- NULL
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
        if ( is.instrument(tmp_instr) && !is.null(tmp_instr$defined.by) )  {        
            dby <- unlist(strsplit( tmp_instr$defined.by,";"))    
            if (any(dby == "IB" )) tmp_symbols <- c(tmp_symbols,instr)
        }
    }
    tmp_symbols
}

#ls_dsvector <- function(pattern=NULL) {
#	ls_defined.by('dsvector',pattern=pattern)
#}

#ls_hand <- function(pattern=NULL) {
#instruments defined by hand
#The should probably be renamed; 
#currenty, defined.by hand means define_stocks was called with a vector of tickers.
#but, actually the stock function is much closer to "defining by hand".
#	symbols <- ls_instruments(pattern)
#	tmp_symbols <- NULL
#	for (symbol in symbols) {
#		tmp_instr <- try(get(symbol, pos=.instrument),silent=TRUE)
#		if (is.instrument(tmp_instr) && !is.null(tmp_instr$defined.by) ) {
#			dby <- unlist(strsplit( tmp_instr$defined.by,";"))
#			if (any(dby == "hand" )) tmp_symbols <- c(tmp_symbols,symbol)
#		}
#	}
#	tmp_symbols
#}

#' @export
#' @rdname ls_twsInstruments
ls_defined.by <- function(x, pattern=NULL) {
	symbols <- ls_instruments(pattern)
	tmp_symbols <- NULL
	for (symbol in symbols) {
		tmp_instr <- try(get(symbol, pos=.instrument),silent=TRUE)
		if (is.instrument(tmp_instr) && !is.null(tmp_instr$defined.by) ) {
			dby <- unlist(strsplit( tmp_instr$defined.by,";"))
			if (any(dby == x)) tmp_symbols <- c(tmp_symbols,symbol)
		}
	}
	tmp_symbols
}

#TODO: add error checking: check to see if .instrument exists 


#' @export
#' @rdname ls_twsInstruments
rm_twsInstruments <- function(x, keep.currencies=TRUE) {
    if (missing(x)) {
        x <- ls_twsInstruments()
    }
    if (keep.currencies && !is.null(x)) {
        if(any(is.na(match(x,ls_currencies())))) { #are any of them not a currency
            if (!all(is.na(match(x,ls_currencies())))) #are some of them a currency
                x <- x[-match(ls_currencies(),x)] #then take them out
        } else stop('Use keep.currencies=FALSE to delete a currency')    
    }
    rm(list=x,pos=.instrument) 
}

#' @export
#' @rdname ls_twsInstruments
rm_non_twsInstruments <- function(x, keep.currencies=TRUE) {
    if (missing(x)) {
        x <- ls_non_twsInstruments()
    }
    if (keep.currencies && !is.null(x)) {
        if(any(is.na(match(x,ls_currencies())))) { #are any of them not a currency
            if (!all(is.na(match(x,ls_currencies())))) #are some of them a currency
                x <- x[-match(ls_currencies(),x)] #then take them out
        } else stop('Use keep.currencies=FALSE to delete a currency')    
    }
    rm(list=x,pos=.instrument)  
}













