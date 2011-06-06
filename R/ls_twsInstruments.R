#TODO: create FX class to use instead of exchange.rate

#TODO: ls_instruments()[match(c('GS','SEE'),ls_instruments())]


ls_twsInstruments <- function(pattern=NULL) {
    if (!is.null(pattern)) {
        symbols <- ls(.instrument, all.names=TRUE, pattern=pattern)
    } else symbols <- ls(.instrument, all.names=TRUE)
    tmp_symbols <- NULL            
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
        if (inherits(tmp_instr, 'twsInstrument') || !is.null(tmp_instr$IB) ) {
            tmp_symbols <- c(tmp_symbols,instr)
        }    
    }
    tmp_symbols
}
ls_non_twsInstruments <- function(pattern=NULL) {
    if (!is.null(pattern)) {
        symbols <- ls(.instrument, all.names=TRUE, pattern=pattern)
    } else symbols <- ls(.instrument, all.names=TRUE)
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













