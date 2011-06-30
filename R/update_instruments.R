
#update_instruments.csv <- function() {
#
#}

#update_instruments.df <- function() {
#
#}

update_instruments.all <- function(symbols='all', ...) {
#    arg <- list(...)
#    if (is.list(arg[["..."]])) {
#        if (length(arg) == 1) 
#            arg <- arg[["..."]]
#        else {
#            targ <- arg[["..."]]
#            arg[["..."]] <- NULL
#            arg <- c(arg, targ)
#        }
#    }
    update_instruments.yahoo(symbols)
    update_instruments.IB(symbols,...)
#    update_instruments.yahoo
}

#update_instruments.all('SPY')
#getInstrument('SPY')

#TODO: Add support for indexes
update_instruments.yahoo <- function(symbols=c('stocks','all'), verbose=FALSE ) {
    if (is.null(symbols) || is.na(symbols) || missing(symbols)) symbols <- 'stocks'
    sym.options <- c('all','stocks')
    symkey <- sym.options[pmatch(symbols,sym.options)]
    symkey <- na.omit(symkey)[1]
    if (!is.na(symkey)) {
	    if (symkey == 'all' || symkey == 'stocks' || is.null(symbols)){
            if (symkey == 'all') warning('yahoo can only update stocks.')            
            symbols <- ls_stocks()
        } 
    }
    #make sure it's a vector of instrument names
    if (!is.character(symbols)) {
        if (verbose) cat('No stocks found to update.\n') 
        return(NULL) #stop('symbols must be a vector of instrument names, or one of "all", "stocks"')    
    }
    yahoo.syms <- paste(symbols, collapse=";")
	if (is.null(yahoo.syms) || length(yahoo.syms) == 0) 
        stop('error with symbol names; no symbols supplied?')
    yahooStuff <- getQuote.yahoo(yahoo.syms,
					  what=yahooQF(c("Name", 
                        "Stock Exchange",
					    "Market Capitalization",
					    "Average Daily Volume", 
                        "Earnings/Share", 
					    "EPS Estimate Current Year", 
					    "EPS Estimate Next Year", 
            			"Book Value", "EBITDA",	
                        "52-week Range")))  
#    sym.length <- length(unlist(strsplit(symbols,";")))    	
    #see yahooQF for available whats
	for (i in 1:length(symbols)) {
        instr <- getInstrument(symbols[i])
		#Only update stocks from yahoo		
		if (inherits(instr,'stock')) {
		    instr$name=as.character(yahooStuff[i,2])
			instr$exchange=as.character(yahooStuff[i,3])
			instr$market.cap=yahooStuff[i,4]
			instr$avg.volume=as.numeric(yahooStuff[i,5])
			instr$EPS=as.numeric(yahooStuff[i,6])
			instr$EPS.current.year.est = as.numeric(yahooStuff[i,7])
			instr$EPS.next.year.est = as.numeric(yahooStuff[i,8])
			instr$book.value=as.numeric(yahooStuff[i,9])
			instr$EBITDA=yahooStuff[i,10]
			instr$range.52wk=yahooStuff[i,11]
	#		instr$IB=twsSTK(as.character(symdesc[i,1]),'SMART'),

			#FIXME: The lapply strips the names of the twsContract 
			#(and presumably any other lists in the instrument)
			if (!is.null(instr$IB)) tmpIB <- instr$IB  #unstable fix           
            instr <- lapply(instr,FUN=function(x) {gsub('N/A','',x)})		    
			if (!is.null(instr$IB)) instr$IB <- tmpIB  #unstable fix          

            tclass <- unique(c(class(instr),'stock','instrument'))
            class(instr) <- tclass        
            db <- instr$defined.by
		    if (!is.null(db)) {
		        db <- unlist(strsplit(db,";"))
		        db <- unique(c(db,"yahoo"))
		        db <- paste(db,collapse=";") 
		    } else db <- "yahoo"
			instr$defined.by=db 
		    instr$updated=Sys.time()
            
		    assign(symbols[i], instr, pos=.instrument)
		}
    }        
}

update_instruments.IB <- function(symbols=c('all','stocks','futures','options','currencies'),
            addIBslot=TRUE, updateInstrument=TRUE, assign_i=TRUE, assign_c=TRUE) 
{
    sym.options <- c('all','stocks','futures','options','cash')
    symkey <- sym.options[pmatch(symbols,sym.options)]
    symkey <- na.omit(symkey)[1]
    if (!is.na(symkey)) {
        switch(symkey, 
                all={symbols <- ls_instruments()}, 
                stocks={symbols <- ls_stocks()}, 
                futures={symbols <- ls_futures()}, 
                options={symbols <- ls_options()}, 
                currencies={symbols <- ls_exchange_rates()} ) #end symbols switch    
    }
    #make sure it's a vector of instrument names
    if (is.null(symbols)) {
        #e.g., if symbols=ls_stocks() and there are no stocks
        warning(paste(deparse(substitute(symbols)), 
                'does not appear to contain any symbols.') )
        return()    
    }
    if (!is.character(symbols)) 
        stop('symbols must be a vector of instrument names, or one of "all", "all.symbols"')    
    for (symbol in symbols) {
        #TODO: If there is a problem with clientId, make note of it, and don't use it again
        #FIXME: passing tws to buildIBcontract doesn't work/isn't implemented correctly. 
        try(buildIBcontract(symbol,addIBslot=addIBslot,updateInstrument=updateInstrument,
            output='nothing', assign_i=assign_i, assign_c=assign_c)) 
	}       
}


