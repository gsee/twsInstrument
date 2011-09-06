
#update_instruments.csv <- function() {
#
#}

#update_instruments.df <- function() {
#
#}

update_instruments.all <- function(Symbols='all', ...) {
    tu <- try(update_instruments.yahoo(Symbols))
    updated <- if (!inherits(tu, 'try-error')) tu
    tu <- try(update_instruments.TTR(Symbols, ...))
    updated <- if (!inherits(tu, 'try-error')) unique(c(updated, tu))
    tu <- try(update_instruments.IB(Symbols,...))
    updated <- if (!inherits(tu, 'try-error')) unique(c(updated, tu))
    updated
}

#update_instruments.all('SPY')
#getInstrument('SPY')

#TODO: Add support for indexes
update_instruments.yahoo <- function(Symbols=c('stocks','all'), verbose=FALSE ) {
    if (is.null(Symbols) || is.na(Symbols) || !hasArg(Symbols)) Symbols <- 'stocks'
    sym.options <- c('all','stocks')
    symkey <- sym.options[pmatch(Symbols,sym.options)]
    symkey <- na.omit(symkey)[1]
    if (!is.na(symkey)) {
	    if (symkey == 'all' || symkey == 'stocks' || is.null(Symbols)){
            if (symkey == 'all') warning('yahoo can only update stocks.')            
            Symbols <- ls_stocks()
        } 
    }
    #make sure it's a vector of instrument names
    if (!is.character(Symbols)) {
        if (verbose) cat('No stocks found to update.\n') 
        return(NULL) #stop('Symbols must be a vector of instrument names, or one of "all", "stocks"')    
    }
    yahoo.syms <- paste(Symbols, collapse=";")
	if (is.null(yahoo.syms) || length(yahoo.syms) == 0) 
        stop('error with symbol names; no Symbols supplied?')
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
#    sym.length <- length(unlist(strsplit(Symbols,";")))    	
    #see yahooQF for available whats
    for (i in 1:length(Symbols)) {
        instr <- getInstrument(Symbols[i])
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
		        db <- rev(unique(c("yahoo", rev(db))))
		        db <- paste(db,collapse=";") 
		    } else db <- "yahoo"
			instr$defined.by=db 
		    instr$updated=Sys.time()
            
		    assign(Symbols[i], instr, pos=.instrument)
		}
    }        
    Symbols
}

update_instruments.IB <- function(Symbols=c('all','stocks','futures','options','currencies'),
            addIBslot=TRUE, updateInstrument=TRUE, include_expired='1', assign_i=TRUE, assign_c=TRUE) 
{
    sym.options <- c('all','stocks','futures','options','cash')
    symkey <- sym.options[pmatch(Symbols,sym.options)]
    symkey <- na.omit(symkey)[1]
    if (!is.na(symkey)) {
        switch(symkey, 
                all={Symbols <- ls_instruments()}, 
                stocks={Symbols <- ls_stocks()}, 
                futures={Symbols <- ls_futures()}, 
                options={Symbols <- ls_options()}, 
                currencies={Symbols <- ls_exchange_rates()} ) #end Symbols switch    
    }
    #make sure it's a vector of instrument names
    if (is.null(Symbols)) {
        #e.g., if Symbols=ls_stocks() and there are no stocks
        warning(paste(deparse(substitute(Symbols)), 
                'does not appear to contain any Symbols.') )
        return()    
    }
    if (!is.character(Symbols)) 
        stop('Symbols must be a vector of instrument names, or one of "all", "all.symbols"')    
    symout <- NULL
    for (symbol in Symbols) {
        #TODO: If there is a problem with clientId, make note of it, and don't use it again
        #FIXME: passing tws to buildIBcontract doesn't work/isn't implemented correctly. 
        tu <- try(buildIBcontract(symbol,addIBslot=addIBslot,updateInstrument=updateInstrument,
            output='symbol', include_expired=include_expired, assign_i=assign_i, assign_c=assign_c))        
        symout <- if (!inherits(tu, 'try-error')) c(symout, tu) 
	}    
    symout   
}

#' update metadata for stocks
#'
#' update stock metadata using the \code{stockSymbols} function from TTR.
#' 
#' If \code{Symbols} is a character vector, those \code{Symbols} will be updated or 
#' defined if they do not already exist.
#' If \code{Symbols} is NULL all stocks found with \code{stockSymbols} will be updated/defined.
#' If \code{Symbols} is \dQuote{stocks} or \dQuote{all} all stocks that are already defined wil be updated.
#' @param Symbols names of instruments to update.
#' @param exchange \dQuote{AMEX}, \dQuote{NASDAQ}, or \dQuote{NYSE}
#' @return names of instruments that were updated/defined
update_instruments.TTR <- function(Symbols = c("stocks", "all"), exchange=c("AMEX","NASDAQ","NYSE")) {
    if (!("package:TTR" %in% search() || require("TTR", quietly = TRUE))) {
        stop("Please install TTR before using this function.")
    }
    if (!suppressWarnings(is.currency("USD"))) currency("USD")
    df <- stockSymbols(exchange=exchange)    
    if (!is.null(Symbols) && !(any(Symbols == c("stocks","all")))) {
        df <- df[match(Symbols,df$Symbol),]
        if (all(is.na(df))) {
            warning(paste(paste(Symbols,collapse=","), "not found among those listed on", paste(exchange,collapse=", ")))
            return(invisible(NULL))        
        }
    } else if (!is.null(Symbols)) df <- df[match(ls_stocks(),df$Symbol),]
    cat('defining stocks...\n')
    symout <- NULL    
    for (i in 1:nrow(df)) {
        primary_id <- as.character(df$Symbol[i])
        instr <- try(getInstrument(primary_id, silent = TRUE), silent = TRUE) 
        args <- list()
        arg <- as.list(df[i, ])
        arg$defined.by <- 'TTR'
        if (is.instrument(instr) && !inherits(instr, 'stock')) {
            #make a unique primary_id
            primary_id <- make.names(c(instr$primary_id, 
                                    ls(.instrument)),unique=TRUE)[-match(ls(.instrument),
                                        make.names(c(instr$primary_id, ls(.instrument)),unique=TRUE))]            
            warning(paste("instrument",instr$primary_id,
                          "is already defined, but not as stock.",
                          "A new instrument", primary_id ,"will be created"))
        } else if (is.instrument(instr)) {
            db <- instr$defined.by
		    if (!is.null(db)) {
		        db <- unlist(strsplit(db,";"))
		        db <- rev(unique(c("TTR", rev(db))))
		        db <- paste(db,collapse=";") 
		    } else db <- "TTR"
			arg$defined.by=db 
        }
        arg$primary_id <- primary_id
        arg$currency <- "USD"
        arg$updated <- Sys.time()
        symout <- c(symout, do.call("stock", arg))
    }
    symout 
}


