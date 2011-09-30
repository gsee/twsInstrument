#update_instruments.csv <- function() {
#
#}

#update_instruments.df <- function() {
#
#}

#' @export
#' @rdname update_instruments.IB
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
#' @export
#' @rdname update_instruments.IB
update_instruments.yahoo <- function(Symbols=c('stocks','all'), verbose=FALSE ) {
    if (is.null(Symbols) || is.na(Symbols) || !hasArg(Symbols)) Symbols <- 'stocks'
    sym.options <- c('all','stocks')
    symkey <- sym.options[pmatch(Symbols,sym.options)]
    symkey <- na.omit(symkey)[1]
    if (!is.na(symkey)) {
	    if (symkey == 'all' || symkey == 'stocks' || is.null(Symbols)){
            if (symkey == 'all') warning('yahoo can only update stocks.')            
            Symbols <- unique(c(ls_stocks(), ls_instruments_by('src','yahoo')))
        } 
    }
    #make sure it's a vector of instrument names
    if (!is.character(Symbols)) {
        if (verbose) cat('No stocks found to update.\n') 
        return(NULL) #stop('Symbols must be a vector of instrument names, or one of "all", "stocks"')    
    }
    yahoo.syms <- Symbols
    for (i in 1:length(Symbols)) {
        tmp_instr <- try(getInstrument(Symbols[i]),silent=TRUE)
        yahoo.syms[i] <- if (!inherits(tmp_instr, 'try-error') 
                            && !is.null(tmp_instr$src) 
                            && any(names(tmp_instr$src) == 'name')) 
                         { tmp_instr$src$name } else Symbols[i]
    }
    yahoo.syms <- paste(yahoo.syms, collapse=";")
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
        noNA <- function(x) {
            if (x == 'N/A' || is.na(x)) {NULL} else {x}
        }
        instr <- getInstrument(Symbols[i])
		#Only update stocks from yahoo		
		if (inherits(instr,'stock') || any(instr$src == 'yahoo')) {
		    instr$name=noNA(as.character(yahooStuff[i,2]))
			instr$exchange=noNA(as.character(yahooStuff[i,3]))
			instr$market.cap=noNA(yahooStuff[i,4])
			instr$avg.volume=noNA(suppressWarnings(as.numeric(yahooStuff[i,5])))
			instr$EPS=noNA(suppressWarnings(as.numeric(yahooStuff[i,6])))
			instr$EPS.current.year.est = noNA(suppressWarnings(as.numeric(yahooStuff[i,7])))
			instr$EPS.next.year.est = noNA(suppressWarnings(as.numeric(yahooStuff[i,8])))
			instr$book.value=noNA(suppressWarnings(as.numeric(yahooStuff[i,9])))
			instr$EBITDA=noNA(yahooStuff[i,10])
			instr$range.52wk=noNA(yahooStuff[i,11])
	#		instr$IB=twsSTK(as.character(symdesc[i,1]),'SMART'),

            tclass <- unique(c(class(instr),'instrument'))
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



#' updates instrument metadata with data from IB or yahoo
#' 
#' Adds/updates information in instrument with data downloaded from IB or yahoo
#' 
#' These are basically wrappers for buildIBcontract. With these functions you
#' can update some or all instruments' information from yahoo or IB, or both.
#' 
#' if you call update_instruments.IB with one of \sQuote{all} or
#' \sQuote{stocks}, \sQuote{futures}, \sQuote{options}, \sQuote{currencies}, it
#' is the same as calling it with the relevant ls_ function (e.g. ls_stocks()).
#' Therefore, functionality can be extended by using ls_ functions instead of a
#' descriptive string.
#' 
#' @aliases update_instruments.IB update_instruments.yahoo 
#' update_instruments.TTR update_instruments.all
#' @param Symbols can be a vector of instrument names, or, can be \sQuote{all}
#' or \sQuote{stocks} or, for update_instruments.TTR, can be NULL in which case
#' all stocks found with \code{stockSymbols} will be defined, or, for
#' update_instruments.IB, can also be \sQuote{futures}, \sQuote{options},
#' \sQuote{currencies}
#' @param exchange character vector of names of exchanges. Used in \sQuote{TTR}
#' method. Can be \dQuote{AMEX}, \dQuote{NASDAQ}, or \dQuote{NYSE}
#' @param addIBslot Boolean. should an IB slot be added to the instrument,
#' making it a twsInstrument?
#' @param updateInstrument Boolean. Should data outside the IB slot also be
#' updated?
#' @param include_expired Should expired contracts be included in
#' reqContractDetails call? "0" for no, "1" for yes (default).
#' @param assign_i should the instrument be stored in .instrument environment.
#' @param assign_c If a new currency is discovered, should it be created
#' @param ... anything to pass through it update_instruments.IB
#' @param verbose be verbose?
#' @return called for side-effect
#' @author Garrett See
#' @seealso twsInstrument, define_stocks, getIBEquities, instrument, stock,
#' future, option, currency
#' @references Yahoo! Finance \url{finance.yahoo.com} YahooQuote
#' \url{http://dirk.eddelbuettel.com/code/yahooquote.html} gummy-stuff.org
#' \url{www.gummy-stuff.org/Yahoo-data.htm} InteractiveBrokers
#' \url{www.interactivebrokers.com} IB API
#' \url{http://interactivebrokers.com/php/apiUsersGuide/apiguide.htm}?
#' @examples
#' 
#' \dontrun{	
#' 	stock('GS',currency('USD'))
#'     update_instruments.yahoo('GS')
#' 	getInstrument('GS')
#' 	update_instruments.IB('GS') 
#' }
#' @export
#' @rdname update_instruments.IB
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
                futures={Symbols <- ls_future_series()}, 
                options={Symbols <- ls_option_series()}, 
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
    #take future roots out of the list
    Symbols <- Symbols[!Symbols %in% ls_futures()[!ls_futures() %in% ls_future_series()]]
    #take option roots out of the list    
    Symbols <- Symbols[!Symbols %in% ls_options()[!ls_options() %in% ls_option_series()]]
    #take out non-FX currencies
    Symbols <- Symbols[!Symbols %in% ls_currencies()[!ls_currencies() %in% ls_exchange_rates()]]
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

#' @export
#' @rdname update_instruments.IB
update_instruments.TTR <- function(Symbols = c("stocks", "all"), exchange=c("AMEX","NASDAQ","NYSE")) {
    if (!("package:TTR" %in% search() || require("TTR", quietly = TRUE))) {
        stop("Please install TTR before using this function.")
    }
    if (!suppressWarnings(is.currency("USD"))) currency("USD")
    df <- stockSymbols(exchange=exchange)    
    if (!is.null(Symbols) && !(any(Symbols == c("stocks","all")))) {
        cols <- try( match(Symbols,df$Symbol) )
        if (!inherits(cols, 'try-error')) {
            df <- df[cols,]
        } else {
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


