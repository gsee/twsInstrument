
ls_instruments <- function(pattern=NULL, match=TRUE) {
    if (length(pattern) > 1 && !match) {
        warning("Using match because length of pattern > 1.")
        #should I use match?
        #or, ignore pattern and return everything?
        #or, do multiple ls calls and return unique
        match <- TRUE    
    }    
    if (!is.null(pattern) && match) {   #there's a pattern and match is TRUE
        symbols <- ls(.instrument, all.names=TRUE)
        symbols <- symbols[match(pattern,symbols)]
    } else if (!match && length(pattern) == 1) { # pattern is length(1) and don't match
        symbols <- ls(.instrument, all.names=TRUE, pattern=pattern)
    } else if (is.null(pattern)) {  #no pattern
        symbols <- ls(.instrument, all.names=TRUE)
    } # else pattern length > 1 & don't match
        
    tmp_symbols <- NULL            
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
        if (is.instrument(tmp_instr))  {    
            tmp_symbols <- c(tmp_symbols,instr)
        }    
    }
    tmp_symbols
}


ls_stocks <- function(pattern=NULL) {
    symbols <- ls_instruments(pattern)
    tmp_symbols <- NULL            
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
        if (inherits(tmp_instr, 'stock') && inherits(tmp_instr, 'instrument')) {
            tmp_symbols <- c(tmp_symbols,instr)
        }    
    }
    tmp_symbols
}

ls_options <- function(pattern=NULL) {
    symbols <- ls_instruments(pattern)    
    tmp_symbols <- NULL            
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
        if (inherits(tmp_instr, 'option') && inherits(tmp_instr, 'instrument')) {
            tmp_symbols <- c(tmp_symbols,instr)
        }    
    }
    tmp_symbols
}

ls_futures <- function(pattern=NULL) {
    symbols <- ls_instruments(pattern)
    tmp_symbols <- NULL            
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
        if (inherits(tmp_instr, 'future') && inherits(tmp_instr, 'instrument')) {
            tmp_symbols <- c(tmp_symbols,instr)
        }    
    }
    tmp_symbols
}

ls_currencies <- function(pattern=NULL) {
    symbols <- ls_instruments(pattern)
    tmp_symbols <- NULL            
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
        if (inherits(tmp_instr, 'currency') && inherits(tmp_instr, 'instrument')) {
            tmp_symbols <- c(tmp_symbols,instr)
        }    
    }
    tmp_symbols
}
ls_non_currencies <- function(pattern=NULL, includeFX=TRUE) {
    symbols <- ls_instruments(pattern)
    tmp_symbols <- NULL            
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
        if (!inherits(tmp_instr, 'currency') || 
                (inherits(tmp_instr, 'exchange_rate') && includeFX) ) {
            tmp_symbols <- c(tmp_symbols,instr)
        }
    }
    tmp_symbols
}

ls_exchange_rates <- function(pattern=NULL) {
    if (!is.null(pattern)) {
        symbols <- ls(.instrument, all.names=TRUE, pattern=pattern)
    #This could use ls_currencies instead of ls_instruments, but currency class may be
    #subject to change
    } else symbols <- ls(.instrument, all.names=TRUE)
    tmp_symbols <- NULL            
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
        if (inherits(tmp_instr, 'exchange_rate') && inherits(tmp_instr, 'instrument')) {
            tmp_symbols <- c(tmp_symbols,instr)
        }    
    }
    tmp_symbols
}

ls_bonds <- function(pattern=NULL) {
    symbols <- ls_instruments(pattern)
    tmp_symbols <- NULL            
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
        if (inherits(tmp_instr, 'bond') && inherits(tmp_instr, 'instrument')) {
            tmp_symbols <- c(tmp_symbols,instr)
        }    
    }
    tmp_symbols
}

ls_funds <- function(pattern=NULL) {
    symbols <- ls_instruments(pattern)
    tmp_symbols <- NULL            
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
        if (inherits(tmp_instr, 'fund') && inherits(tmp_instr, 'instrument')) {
            tmp_symbols <- c(tmp_symbols,instr)
        }    
    }
    tmp_symbols
}

ls_spreads <- function(pattern=NULL) {
    symbols <- ls_instruments(pattern)
    tmp_symbols <- NULL            
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
        if (inherits(tmp_instr, 'spread') && inherits(tmp_instr, 'instrument')) {
            tmp_symbols <- c(tmp_symbols,instr)
        }    
    }
    tmp_symbols
}

ls_guaranteed_spreads <- function(pattern=NULL) {
    symbols <- ls_instruments(pattern)
    tmp_symbols <- NULL            
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
        if (inherits(tmp_instr, 'guaranteed_spread') && inherits(tmp_instr, 'instrument')) {
            tmp_symbols <- c(tmp_symbols,instr)
        }    
    }
    tmp_symbols
}

ls_synthetics <- function(pattern=NULL) {
    if (!is.null(pattern)) {
        symbols <- ls(.instrument, all.names=TRUE, pattern=pattern)
    } else symbols <- ls(.instrument, all.names=TRUE)
    tmp_symbols <- NULL            
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
        if (inherits(tmp_instr, 'synthetic') && inherits(tmp_instr, 'instrument')) {
            tmp_symbols <- c(tmp_symbols,instr)
        }    
    }
    tmp_symbols
}


# should it be ls_yahoo, ls_defined.by.yahoo, or ls_src? something else?
#ls_yahoo <- function(pattern=NULL) {
#instruments defined by yahoo
#    symbols <- ls_instruments(pattern) #TODO: other functions should be updated to get symbols like this too   
#    tmp_symbols <- NULL
#    for (instr in symbols) {
#        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
#        if ( is.instrument(tmp_instr) && !is.null(tmp_instr$defined.by) )  {        
#            dby <- unlist(strsplit( tmp_instr$defined.by,";"))    
#            if (any(dby == "yahoo" )) 
#                tmp_symbols <- c(tmp_symbols, instr)
#        }
#    }
#    tmp_symbols
#}

#ls_IB <- function(pattern=NULL) {
#instruments defined by IB
#    symbols <- ls_instruments(pattern)   
#    tmp_symbols <- NULL
#    for (instr in symbols) {
#        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
#        if ( is.instrument(tmp_instr) && !is.null(tmp_instr$defined.by) )  {        
#            dby <- unlist(strsplit( tmp_instr$defined.by,";"))    
#            if (any(dby == "IB" )) tmp_symbols <- c(tmp_symbols,instr)
#        }
#    }
#    tmp_symbols
#}


#ls_defined.by <- function(x, pattern=NULL) {
#	symbols <- ls_instruments(pattern)
#	tmp_symbols <- NULL
#	for (symbol in symbols) {
#		tmp_instr <- try(get(symbol, pos=.instrument),silent=TRUE)
#		if (is.instrument(tmp_instr) && !is.null(tmp_instr$defined.by) ) {
#			dby <- unlist(strsplit( tmp_instr$defined.by,";"))
#			if (any(dby == x)) tmp_symbols <- c(tmp_symbols,symbol)
#		}
#	}
#	tmp_symbols
#}


ls_derivatives <- function(pattern=NULL) {
    symbols <- ls_instruments(pattern)
    #there is currently no derivative class    
    #but check for it in case someone made one    
    tmp_symbols <- NULL
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
         if (inherits(tmp_instr, 'derivative') || 
                 inherits(tmp_instr, 'option') ||
                 inherits(tmp_instr, 'future') ) {
             tmp_symbols <- c(tmp_symbols,instr)
         }    
    }
    tmp_symbols
}

ls_non_derivatives <- function(pattern=NULL) {
    symbols <- ls_instruments(pattern)
    #there is currently no derivative class
    #but check for it in case someone made one    
    tmp_symbols <- NULL
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
         if (!inherits(tmp_instr, 'derivative') && 
                 !inherits(tmp_instr, 'option') &&
                 !inherits(tmp_instr, 'future') ) {
             tmp_symbols <- c(tmp_symbols,instr)
         }    
    }
    tmp_symbols
}


ls_calls <- function(pattern=NULL) {
   if (!is.null(pattern)) {
        symbols <- ls_options(pattern=pattern)
    } else symbols <- ls_instruments()    
	tmp_symbols <- NULL
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
		if (is.instrument(tmp_instr) && inherits(tmp_instr, 'option')) {
			if (!is.null(tmp_instr$callput)) {
				right <- tmp_instr$callput
			}else if(!is.null(tmp_instr$right)) {
				right <- tmp_instr$right
			} else right <- FALSE
			if (right == "C" || right == "Call" ||
				right == "call" || right == "c") {
					tmp_symbols <- c(tmp_symbols,instr)
			}			
		}
    }
    tmp_symbols
}

ls_puts <- function(pattern=NULL) {
	if (!is.null(pattern)) {
        symbols <- ls_options(pattern=pattern)
    } else symbols <- ls_instruments()    
	tmp_symbols <- NULL
    for (instr in symbols) {
        tmp_instr <- try(get(instr, pos = .instrument),silent=TRUE)
		if (is.instrument(tmp_instr) && inherits(tmp_instr, 'option')) {
			if (!is.null(tmp_instr$callput)) {
				right <- tmp_instr$callput
			}else if(!is.null(tmp_instr$right)) {
				right <- tmp_instr$right
			} else right <- FALSE
			if (right == "P" || right == "Put" ||
				right == "put" || right == "p") {
					tmp_symbols <- c(tmp_symbols,instr)
			}			
		}
    }
    tmp_symbols
}


#TODO: add error checking: check to see if .instrument exists 

rm_instruments <- function(x, keep.currencies=TRUE) {
    if (missing(x)) {
       x <- ls_instruments()       
    } 
    if (keep.currencies && !is.null(x)) {
        if(any(is.na(match(x,ls_currencies())))) { #are any of them not a currency
            if (!all(is.na(match(x,ls_currencies())))) #are some of them a currency
                x <- x[-match(ls_currencies(),x)] #then take them out of to-be-removed
        } else stop('Use keep.currencies=FALSE to delete a currency')    
    }

    rm(list=x,pos=.instrument)
}

rm_stocks <- function(x) {
    if (missing(x)) {
        x <- ls_stocks()
    }
    rm(list=x,pos=.instrument)
}

rm_options <- function(x) {
    if (missing(x)) {
        x <- ls_options()
    }
    rm(list=x,pos=.instrument)
}

rm_futures <- function(x) {
    if (missing(x)) {
        x <- ls_futures()
    }
    rm(list=x,pos=.instrument)
}

rm_currencies <- function(x) {
    if (missing(x)) {
        x <- ls_currencies()
    }
    rm(list=x,pos=.instrument)
}   

rm_exchange_rates <- function(x) {
    if (missing(x)) {
        x <- ls_currencies()
    }
    rm(list=x,pos=.instrument)
}

rm_bonds <- function(x) {
    if (missing(x)) {
        x <- ls_bonds()
    }
    rm(list=x,pos=.instrument)
}

rm_funds <- function(x) {
    if (missing(x)) {
        x <- ls_funds()
    }
    rm(list=x,pos=.instrument)
}

rm_spreads <- function(x) {
    if (missing(x)) {
        x <- ls_spreads()
    }
    rm(list=x,pos=.instrument)
}

rm_synthetics <- function(x) {
    if (missing(x)) {
        x <- ls_synthetics()
    }
    rm(list=x,pos=.instrument)
}


rm_derivatives <- function(x) {
    if (missing(x)) {
        x <- ls_derivatives()
    }
    rm(list=x,pos=.instrument)
}

rm_non_derivatives <- function(x, keep.currencies=TRUE) {
    if (missing(x)) {
        x <- ls_non_derivatives()
    }
    if (keep.currencies && !is.null(x)) {
        if(any(is.na(match(x,ls_currencies())))) { #are any of them not a currency
            if (!all(is.na(match(x,ls_currencies())))) #are some of them a currency
                x <- x[-match(ls_currencies(),x)] #then take them out of to-be-removed
        } else stop('Use keep.currencies=FALSE to delete a currency')    
    }
    rm(list=x,pos=.instrument) 
}


