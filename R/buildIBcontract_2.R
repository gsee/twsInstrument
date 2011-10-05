## Given a symbol, conId (chr or numeric), twsInstrument, instrument, 
## instrument name, or twsContract, these functions will create a 
## twsInstrument with as much information as can be found.

#implemented: STK,OPT,FUT,CASH,IND
#not implemented: FOP

#---------------------------------------------------#
# FinancialInstrument.type  |  twsContract.sectype  #
#    stock                  |       STK             #
#    synthetic              |       IND             #
#    option_series          |       OPT             #
#    future_series          |       FUT             #
#    exchange_rate          |       CASH            #
#---------------------------------------------------#

#' @export
#' @rdname buildIBcontract
is.twsInstrument <- function(x) {
    if (inherits(x, 'twsInstrument')) {
        TRUE    
    } else if (inherits(x, 'instrument') && 
                !is.null(x$IB) && 
                inherits(x$IB, 'twsContract') ) {
        TRUE 
    } else FALSE
}

#Any of these will create a twsInstrument object

#' @export
#' @rdname buildIBcontract
Contr_From_Instr <- function(instrument, tws=NULL, 
		addIBslot=FALSE, updateInstrument=FALSE, 
		output=c('contract','symbol','nothing','instrument'), 
		include_expired="0", assign_i=FALSE, assign_c=TRUE, verbose=TRUE, silent=FALSE) 
{
    if (is.numeric(output))
        output <- c('contract','symbol','nothing','instrument')[output]
    buildIBcontract(symbol=instrument, tws=tws, addIBslot=addIBslot,
        updateInstrument=updateInstrument, output=output[1], 
        include_expired=include_expired, 
        assign_i=assign_i, assign_c=assign_c, verbose=verbose, silent=silent)
}

#' @export
#' @rdname buildIBcontract
Instr_From_Contr<- function(contract, tws=NULL, 
		addIBslot=FALSE, updateInstrument=TRUE, 
		output=c('instrument','symbol','nothing','contract'), 
		include_expired="0", assign_i=FALSE, assign_c=TRUE, verbose=TRUE, silent=FALSE) 
{
    if (is.numeric(output)) 
        output=c('instrument','symbol','nothing','contract')[output]
    buildIBcontract(symbol=contract, tws=tws, addIBslot=addIBslot,
        updateInstrument=updateInstrument, output=output[1],
        include_expired=include_expired, 
        assign_i=assign_i, assign_c=assign_c, verbose=verbose, silent=silent)
}

#' @export
#' @rdname buildIBcontract
twsInstrument <- function(symbol, tws=NULL, 
        addIBslot=TRUE, updateInstrument=TRUE, 
        output=c('symbol','nothing','instrument','contract'), 
        include_expired="0", assign_i=TRUE, assign_c=TRUE, verbose=TRUE, silent=FALSE)
{
    if (is.numeric(output)) 
        output <- c('nothing','symbol','instrument','contract')[output]
    buildIBcontract(symbol=symbol, tws=tws, addIBslot=addIBslot,
            updateInstrument=updateInstrument, output=output, 
            include_expired=include_expired, 
            assign_i=assign_i, assign_c=assign_c, verbose=verbose, silent=silent)
} 


#' buildIBcontract and wrappers
#' 
#' create twsInstrument, or create twsContracts using previously defined
#' FinancialInstruments, or create FinancialInstruments from previously defined
#' twsContract
#' 
#' see ?\sQuote{twsInstrument-package} for \code{\link{twsInstrument-package}} help page.
#'
#' Using metadata that has already been defined for an instrument, create a
#' twsContract object and fill in any missing information. Can either add an IB
#' slot to the instrument, or update the entire instrument creating slots as
#' needed.
#' 
#' buildIBcontract is the main function; the rest are wrappers.
#' 
#' symbol can be the name of an instrument, an instrument, a twsContract,
#' twsInstrument or a conId (unique numeric contract identifier used by
#' Interactive Brokers).  Using the information given, it will create an
#' instrument and a twsContract.  It will then make a call to
#' reqContractDetails to fill in any missing information. (If you give it a
#' string and there is no instrument by that name, then: (a) If the string ends
#' in a period, it will treat it as a currency pair using "USD" as the base
#' currency. (b) if the string can be coerced to numeric, it will be used as
#' the conId in a call to \code{\link{getContract}} (which in turn calls
#' \code{\link{reqContractDetails}} to get a twsContract object.  (c)
#' Otherwise, it will be passed to \code{\link{instrument.auto}} which will try
#' to create an instrument that can be updated.  If the string is something
#' other than the name of a \code{FUT}, \code{OPT}, or \code{CASH} -- for
#' example, if it is the name of a \code{STK} or \code{IND} -- it will be
#' assumed that it is a \code{STK}.  If the request for contract details fails,
#' it will be tried again as an \code{IND} (Note that if you want an index, but
#' request a stock, it is more likely that you will get a stock of a different
#' currency than that the request will fail. Therefore, you should always
#' define your instruments first.  Wrapping the symbol with something like
#' \code{synthetic('SPX','USD')} or \code{stock('SPY','USD')} will ensure that
#' you get the \code{sectype} of \code{twsContract} you are after).
#' 
#' if addIBslot is TRUE it will store the contract in the IB slot of the
#' instrument (creating the slot if necessary.)  If updateInstrument is TRUE it
#' will add/replace information directly in the instrument object.
#' 
#' It is recommended that you do not pass it a twsconn object, in which case it
#' will create a temporary one. If you pass it a twsconn object you are more
#' likely to encounter errors.  If you pass a connected twsconn object it will
#' be disconnected after the request.
#' 
#' twsInstrument is a wrapper that will create a twsInstrument classed object.
#' By default, It creates a twsContract and an instrument (if necessary) and
#' places the twsContract in the IB slot of the instrument. The twsInstrument
#' class is automatically added to any instrument that has an IB slot.
#' 
#' Instr_From_Contr is a wrapper to create an instrument using a twsContract.
#' It does not create an IB slot or store the twsContract in the instrument by
#' default, and therefore does not add the twsInstrument class to the
#' instrument.
#' 
#' Contr_From_Instr is a wrapper to create a twsContract using an instrument.
#' By default, it does not make any changes to the instrument, and therefore
#' does not add the twsInstrument class to it.
#' 
#' Instr_From_Contr and Contr_From_Instr are essentially the same functions,
#' but with different default outputs.
#' 
#' \code{ouput} should be a character string describing what to return. Valid
#' values are c(\sQuote{"nothing}, \sQuote{symbol}, \sQuote{instrument},
#' \sQuote{contract}
#' 
#' @aliases buildIBcontract twsInstrument Instr_From_Contr Contr_From_Instr
#' is.twsInstrument
#' @param symbol An instrument, The name of an instrument, a twsContract, or a
#' conId.
#' @param tws twsconn object. Not required.
#' @param addIBslot Boolean. Should an IB slot be created in the instrument
#' object?)
#' @param updateInstrument Should all the attributes of the instrument be
#' updated with the information retrieved from IB?
#' @param output what should be returned. one of
#' 'nothing','symbol','instrument','contract'
#' @param include_expired is the requested contract expired?
#' @param assign_i boolean. Should the instrument be stored in the instrument
#' environment?
#' @param assign_c boolean. If a currency isn't defined, should it be?
#' @param verbose be verbose?
#' @param silent silence warnings?
#' @param instrument for wrapper, alias for symbol
#' @param contract for wrapper, alias for symbol
#' @param x what to test for is.twsInstrument
#' @return Usually called for its side-effect. It will return what is defined
#' by the output argument. See details.
#' @author Garrett See
#' @seealso instrument, twsContract, addIBslot,
#' @examples
#' 
#' 
#' \dontrun{
#' 
#' twsInstrument('SPY') #assumes it's a stock
#' 
#' #Now something that isn't denominated in USD
#' twsInstrument(twsFUT(symbol='NIY',exch='GLOBEX',expiry='201109',
#'                      currency='JPY'))
#' 
#' #As a wrapper for instrument wrappers
#' Contr_From_Instr(synthetic('SPX','USD'))
#' Instr_From_Contr(twsFUT(symbol='ES',exch='GLOBEX',expiry='201112', currency='USD'))
#' 
#' buildIBcontract(twsSTK("GOOG"),updateInstrument=TRUE, addIBslot=FALSE, output='nothing')
#' 
#' stock('AAPL','USD')
#' buildIBcontract('AAPL', updateInstrument=FALSE) #uses instrument
#' 
#' ls(.instrument,all.names=TRUE)
#' 
#' }
#' @export
#' @rdname buildIBcontract
buildIBcontract <- function(symbol, tws=NULL, 
		addIBslot=FALSE, updateInstrument=FALSE, 
		output=c('contract','instrument','symbol','nothing'), 
		include_expired="0", assign_i=FALSE, assign_c=TRUE, verbose=TRUE, silent=FALSE)
{
    #TODO: Allow for vector of symbols, instruments, or contracts
    if (is.xts(symbol)) stop('symbol can be the name of an xts object, but not the object itself.')    
    if (!is.list(symbol) && (is.numeric(symbol) || !is.na(suppressWarnings(as.numeric(symbol))))) symbol <- getContract(symbol)    
    primary_id <- NULL
    right <- NULL    
    contract <- NULL
    instr <- NULL    
    ambiguous <- FALSE #Is it unclear what type of instrument/contract we're making
    if (is.twsContract(symbol)){ #then make an instr
        #also create instrument (but it will only be assigned if updateInstrument==TRUE) &| assign_i==TRUE
        #need at least primary_id, currency, multiplier, tick_size, identifiers, type
                
	    contract <- symbol
        symbol <- contract$symbol

        #make sure the currency is defined for this product
		tmpccy <- try(getInstrument(contract$currency, silent=TRUE),silent=TRUE)
		if (inherits(tmpccy, 'try-error') || !is.instrument(tmpccy) ) {
            if (assign_c)	{	    
                currency(contract$currency)
		        if (!silent) warning(paste("Creating currency ", contract$currency))   
            } else stop (paste(contract$currency, 'cannot be found, and assign_c=FALSE'))
		}        
	    #primary_id <- symbol
        identifiers <- list(conId=contract$conId, local=gsub(" ","",contract$local))
        identifiers <- identifiers[!identifiers %in% c("0","")]                    
        instr <- switch(contract$sectype, 
                IND={
                    primary_id <- contract$symbol
                    instrument.tws(primary_id=primary_id, exchange=contract$exch, currency=contract$currency, multiplier=1,
                                    tick_size=NULL, identifiers=identifiers, type='synthetic', assign_i=FALSE)
                },
                STK={
                    primary_id <- contract$symbol                    
                    #stock(primary_id=primary_id, currency=contract$currency, exchange=contract$exchange)
                    instrument.tws(primary_id=primary_id, currency=contract$currency,multiplier=1,
                                tick_size=0.01, identifiers=identifiers, type='stock', assign_i=FALSE)
                }, 
                OPT={
#                   primary_id <- paste('.', contract$symbol,sep="")
                    if (!is.null(contract$local) && contract$local != "") {
                        ylocal <- gsub("   ","",contract$local)#take out the triple space
                        si <- gsub(contract$symbol,"",ylocal) #suffix_id
					    #id <- paste(primary_id,suffix_id,sep="_")
					    expiry <- substr(si,1,6)
					    right <- substr(si,7,7)
					    strike <- as.numeric(substr(si,8,15))/1000
					    #local <- paste(symbol, si, sep="   ")      
					    primary_id <- paste(contract$symbol, "_", expiry, right, strike, sep="")
                    } else {
                        if (any(nchar(contract$expiry) == c(6,8))) {
                            m <- substr(contract$expiry,5,6)
                            y <- substr(contract$expiry,1,4)
                        } else if (!identical(integer(0),grep('-',contract$expiry))) {
                            ss <- strsplit(contract$expiry,"-")[[1]]
                            m <- ss[2]
                            y <- ss[1]    
                        }
                        expiry <- paste(y,sprintf("%02d",as.numeric(m)),sep="")                        
                        right <- contract$right    
                        strike <- contract$strike
                        primary_id <- option_id(underlying_id=contract$symbol, strike=strike, 
                                                month=m, year=y, right=right)
                    }
                    callput <- switch(right, C=,c=,call='call', P=,p=,put='put')
                    #option(primary_id=primary_id, currency=contract$currency,
                    #    multiplier=contract$multiplier, expires=contract$expiry, right=contract$right,
                    #    strike=contract$strike, exchange=contract$exchange, underlying_id=contract$symbol)
                    instrument.tws(primary_id=primary_id, currency=contract$currency,
                        multiplier=as.numeric(contract$multiplier), tick_size=NULL, 
                        identifiers=identifiers, expires=contract$expiry, right=right, callput=callput,
                        strike=contract$strike, exchange=contract$exch, type=c('option_series','option'), 
                        underlying_id=contract$symbol, assign_i=FALSE)
                }, 
                FUT={
                    primary_id <- symbol
                    instrument.tws(primary_id=primary_id, currency=contract$currency, 
                        multiplier=as.numeric(contract$multiplier), tick_size=NULL,
                        identifiers=identifiers, expires=contract$expiry, 
                        exchange=contract$exch, type=c('future_series','future'), 
                        underlying_id=contract$symbol, assign_i=FALSE) #maybe shouldn't specify exchange here
                }, 
                CASH={ 
                    if (contract$local == "") {
                        primary_id <- paste(contract$symbol, contract$currency, sep="") #will be contract$local after update
                    } else primary_id <- contract$local
                    #exchange_rate(primary_id=primary_id, currency=contract$currency, second_currency=contract$symbol)
                    instrument.tws(primary_id=primary_id, currency=contract$currency, multiplier=1, 
                                tick_size=0.01, identifiers=identifiers, counter_currency=contract$symbol, 
                                type=c("exchange_rate","currency"), assign_i=FALSE)
                    #currency(primary_id=contract$symbol, currency=contract$currency, exchange=contract$exch, type='currency')
                }) #End switch on sectype
         #TODO: Implement for bonds and other instruments        
         #if (updateInstrument==FALSE) warning(paste('Created ', primary_id, ' instrument because it could not be found.',sep=''))    
    } #End if (is.twsContract(symbol))
#If it was a twsContract, we 
#(1) copied it to a temporary contract (contract)
#(2) figured out the symbol, and primary_id
#(3) built a temporary instrument (instr)


    if (is.instrument(symbol)) { #assign to instr, and redefine symbol
        instr <- symbol
    	primary_id <- instr$primary_id #TODO: check for suffix_id
        if (!is.null(instr$underlying_id)) { 
            symbol <- instr$underlying_id
        } else if (!is.null(instr$counter_currency)) {
            symbol <- instr$counter_currency
        } else symbol <- instr$primary_id
	#if it was an instrument, we copied it to instr, figured out the primary_id, and the symbol 
    } else if (!is.twsContract(symbol)) { 
	#not an instrument or contract 
	#(if it was initially a twsContract or instrument, 
	#symbol has been overwritten with symbol name)
        if (length(symbol) > 1) { 
            #TODO: allow for vector of symbols, instruments, twsContracts, or twsInstruments 
	        #TODO2: allow for named lists.
            stop('symbol must be an instrument, twsInstrument, twsContract, or the name of an instrument')
        } else {
	#we'll get here if the symbol argument given was a string (e.g. "SPY") or a twsContract
            if (is.null(primary_id)) { #i.e. if it wasn't a twsContract
				#if it has a "_" in it, then split into primary and suffix?				
				primary_id <- symbol
			}             
            if (is.null(instr) && length(primary_id) ==1 )  {
		#instr will be null if we were given a string.
                instr <- try(getInstrument(primary_id,silent=TRUE),silent=TRUE)
            }
        }
    }
####
#Now unless we were given a string, we have value for symbol, primary_id, instr, and contract. 
#if it was a string, we only have primary_id, and probably have instr
#(if we got instr from a string, it may not have succeeded. we'll check for that next.)

    if (inherits(instr,'try-error') || !is.instrument(instr)) {
        #TODO: allow for EUR/USD format also.
        pid <- parse_id(symbol)
        if ( (nchar(symbol) == 6 ) && any(pid$type == 'root') && !identical(integer(0), grep(symbol, toupper(symbol))) ) { #6 letters, all uppercase
            ccys <- c(substr(symbol, 1, 3),substr(symbol,4,6))         
            contract <- twsCASH(ccys[1], ccys[2])
            primary_id=paste(contract$symbol, contract$currency, sep="") #consistent with blotter, but I think it should be sep="."
            instr <- instrument.tws(primary_id=primary_id, 
                        currency=contract$currency, 
                        multiplier=1, 
                        tick_size=0.01, 
                        identifiers=NULL, 
                        counter_currency=contract$symbol,
                        type=c("exchange_rate","currency"), assign_i=FALSE)
        } else if (!identical(grep('\\.',symbol), integer(0)) && nchar(symbol) == 7) {
            #if it has 7 characters and one of them is a period, treat it as an FX pair (e.g. EUR.USD)            
            ccys <- strsplit(symbol, "\\.")[[1]]            
            contract <- twsCASH(ccys[1], ccys[2])
            primary_id=paste(contract$symbol, contract$currency, sep="") #consistent with blotter, but I think it should be sep="."
            instr <- instrument.tws(primary_id=primary_id, 
                        currency=contract$currency, 
                        multiplier=1, 
                        tick_size=0.01, 
                        identifiers=NULL, 
                        counter_currency=contract$symbol,
                        type=c("exchange_rate","currency"), assign_i=FALSE)
        } else if (nchar(symbol) == 4 && substr(symbol,4,4) == "." ) {
            contract <-twsCASH(substr(symbol,1,3))
            #instr <- Instr_From_Contr(contract)
            primary_id=paste(contract$symbol, contract$currency, sep="") #consistent with blotter, but I think it should be sep="."
            instr <- instrument.tws(primary_id=primary_id, 
                        currency=contract$currency, 
                        multiplier=1, 
                        tick_size=0.01, 
                        identifiers=NULL, 
                        counter_currency=contract$symbol, 
                        type=c("exchange_rate","currency"), assign_i=FALSE)        
        } else { 
            #warning(paste("Unable to find or infer instrument, ",
            #        symbol, ".\n  Trying with type = \"stock\"", sep=""))
            ccys <- ls_currencies()
            instr.name <- instrument.auto(primary_id=primary_id, silent=TRUE)
            instr <- getInstrument(instr.name) #workaround because instrument.auto calls wrappers that don't allow assign_i=FALSE
            rm_instruments(instr.name)
            ccys <- ls_currencies()[!ls_currencies() %in% ccys]
            if (!identical(ccys, character(0))) rm_currencies(ccys)
            instr$currency <- "" # since we don't know it, we'll have to let IB guess for us (IB may be wrong!)
        } 
    }


	if (is.instrument(instr) && !is.twsContract(instr$IB) && is.null(contract)) { #make contract
	    primary_id <- instr$primary_id
        pid <- parse_id(instr$primary_id)
	    #figure out sectype
	    if (is.null(instr$sectype) ) {
            if (is.null(instr$type)) { #future_series or option_series created with instrument.auto when no root existed
                instr$multiplier <- ""
                if (any(pid$type == 'future')) {
                    instr$type <- 'future_series' #a future object would parse out to 'root', not 'future'
                } else if (any(pid$type == 'option')) {
                    instr$type <- 'option_series' #an option object would parse out to 'root', not 'option'
                } else if (any(pid$type == 'root')) {
                    instr$type <- 'stock'
                    ambiguous <- TRUE
                    #if (!silent) warning(paste(instr$primary_id, "is of an ambiguous format. ",
                    #            "Trying with type = \"stock\""))
                    instr$multiplier <- 1
                }
            } else pid <- NULL
			#currencies don't have type by FinancialInstrument default. #FIXME: They do now; this can be updated
            if (inherits(instr,'currency') || 
				(!is.null(instr$type) && any(instr$type == 'currency') ) ) {
                sectype <- "CASH"
                if (!is.null(instr$counter_currency)) {
                    symbol <- instr$counter_currency
                } else if (!is.null(instr$second_currency)) {
                    symbol <- instr$second_currency
                } else if (nchar(instr$primary_id) == 6) { #TODO: make sure it's 6 letters
                    symbol <- substr(instr$primary_id,1,3)
                } else if (nchar(instr$primary_id) == 7) { 
                    symbol <- strsplit(instr$primary_id,"\\.")[[1]][1]                 
                } else if (nchar(instr$primary_id) == 7) { #e.g. if it was EUR/USD, then the last line didn't change it
                    symbol <- strsplit(instr$primary_id,"/")[[1]][1]
                } else symbol <- instr$primary_id
           } else if (inherits(instr,'option') || 
                any(instr$type == "option") || 
                any(instr$type == "option_series") ||
                any(instr$type == "OPT") ) {
                    sectype <- "OPT"
                    if (!is.null(instr$underlying_id) 
                           && !instr$underlying_id == "") {
                        symbol <- instr$underlying_id
                    } else symbol <- instr$root_id
                #TODO: treat option and option_series differently
            } else if (inherits(instr,'future_series') || 
                any(instr$type == "future_series") ||
                any(pid$type == "SSF")) {
                    sectype <- "FUT" 
                    if(is.null(instr$root_id)) instr$root_id <- parse_id(primary_id)$root
                    if(is.null(instr$suffix_id)) instr$suffix_id <- parse_id(primary_id)$suffix        
                    symbol <- instr$root_id
            } else if (inherits(instr,'future') ||
                any(instr$type == 'future')) {
                    sectype <- 'FUT'
                    symbol <- primary_id <- gsub("\\.","",primary_id)
            } else if (inherits(instr,'stock') || inherits(instr, 'fund') ||
                any(instr$type == "stock") || any(instr$type == 'fund') ||
                any(instr$type == "STK")) {
                    sectype <- "STK"                
            } else if (inherits(instr,'synthetic') ||
                any((instr$type == 'synthetic')) ||
                any((instr$type == 'IND'))) {
                    sectype <- 'IND'
            } else { 
                stop(paste('Cannot determine sectype; ', symbol , 
                    ' does not appear to be a stock, ',
                    'option, future or currency.', sep=""))
            }     
        } else sectype = instr$sectype
        
        if (is.null(contract$conId) || contract$conId == 0) { #This if statement isn't necessary
            conId <- 0        
            pid <- parse_id(primary_id)
            if ((any(instr$type == "future_series") || any(instr$type == "option_series") ) && is.null(instr$expires) && is.null(instr$expiry)) {
                if (!silent) warning("Expiry not defined for future or option... Inferring from id.") 
                instr$expires <- format(as.Date(paste(pid$month,pid$year,15),origin='1970-01-01',format='%b%Y%d'),format='%Y%m')
            }
            if (any(instr$type == "option_series") ) {
				if (is.null(instr$strike)) {
					if (!silent) warning("strike is not defined for option... Inferring from id.")
                    instr$strike <- pid$strike
                } 				
                if (!is.null(instr$callput)) {
					right <- switch(instr$callput, call=,c=,C="C",put=,p=,P="P")
				} else if (!is.null(instr$right)) {
					right <- switch(instr$right, call=,c=,C="C",put=,p=,P="P") #instr$right
				} else {
                    right <- pid$right
					if (!silent) warning("right of option is neither call nor put... Inferring from id.")
				    right <- instr$right
				}
			}        
		} else conId <- contract$conId


        #set multiplier
        multiplier <- ""
        if (sectype == "STK") { # || sectype == "CASH") {
            exchange <- 'SMART'
        } else if (sectype == "CASH") {
            exchange <- "IDEALPRO"
        } else if (sectype == 'IND') {
            exchange <- instr$exchange
        } else {
            multiplier <- instr$multiplier            
            exchange <- instr$exchange #Should exchange and primary both be the same ?        
        }
        primary <- "" # should this be instr$exchange ?

        if (is.null(instr$expires) && !is.null(instr$expiry)) {
            expiry <- paste(instr$expiry)
        } else if (!is.null(instr$expires)) {
            expiry <- paste(instr$expires) 
        } else expiry <- ""
		IBexpiry <- expiry
        if(sectype == "FUT") {
            if (nchar(IBexpiry) == 10) {
                IBexpiry <- format(as.Date(IBexpiry, origin='1970-01-01', format="%Y-%m-%d"),"%Y%m")
            } else if (nchar(IBexpiry) == 7) IBexpiry <- gsub("-","",IBexpiry)
        } 
        #IB uses the Friday before expiration Saturday for expiry
        #except for EOM options.
		if (!is.null(IBexpiry) && is.character(IBexpiry) && IBexpiry != "") {
            if (nchar(IBexpiry) == 8) {
			    expdate <- as.Date(IBexpiry, origin='1970-01-01', format="%Y%m%d")
			    if (weekdays(expdate) == "Saturday") {
				    IBexpiry <- format(expdate - 1,"%Y%m%d")
			    }
            } else if (nchar(IBexpiry) == 10) {
                expdate <- as.Date(IBexpiry, origin='1970-01-01', format="%Y-%m-%d")
                if (weekdays(expdate) == "Saturday") {
                    IBexpiry <- format(expdate - 1, "%Y%m%d")
                }
            }
		} else IBexpiry = ""

        strike <- instr$strike

        currency <- instr$currency
#        right <- instr$right
        #TODO: try to add other info here: local,etc.

        #change the NULL values to empty character strings
        if (is.null(primary) || primary == "N/A") primary <- ""
        if (is.null(exchange)) {
            if (sectype == 'STK') {
                exchange <- 'SMART' 
            } else if (sectype == 'CASH') {
                exchange <- 'IDEALPRO'
            } else exchange <- ''
        }        
        if (is.null(expiry)) expiry <- ""
        if (is.null(strike)) strike <- ""
        if (is.null(right)) right <- ""

		#local <- paste(symbol, TODO				
		#create/get initial contract
        #FIXME: Should exch="" here? or instr$exchange? or 'SMART'?        
        contract <- twsContract(conId=conId, symbol=symbol, exch=exchange,primary=primary,
                        sectype=sectype, expiry=IBexpiry, strike=strike, currency=currency,
                        right=right, local="", multiplier=multiplier, combo_legs_desc="",
                        comboleg="", include_expired=include_expired) 
    } else if (is.null(contract) && is.instrument(instr)) contract <- instr$IB
        #done getting twsContract object
####################################################################
    #Establish a connection, and download contract details from IB. on error: disconnect
	if ( (contract$sectype != "CASH") || 
         ((contract$sectype == "CASH") && !is.instrument(instr)) ||
          (is.instrument(instr) && instr$currency != instr$primary_id) ) {
        #|| (contract$symbol != contract$currency) ) {		
        # || is.exchange_rate(instr) #no function for this
		tryCatch(
		{    
            tryCatch(
            {
                if (is.null(tws) || (is.twsConnection(tws) && !isConnected(tws)) ) 
                    tws <- try(twsConnect(100),silent=TRUE)
                if (inherits(tws,'try-error')) tws <- try(twsConnect(101),silent=TRUE) #try another clientId
                if (inherits(tws,'try-error')) tws <- try(twsConnect(102),silent=TRUE) #3rd time's the charm
                if (inherits(tws,'try-error')) tws <- twsConnect(150) #a last attempt for an available clientId
            }, finally={ 
                if (isConnected(tws)) {
                    if (verbose) cat(paste('Connected with clientId ', tws$clientId, '.\n',sep=""))    
                    if (tws$clientId == 150) warning("IB Trader Workstation should be restarted.")                    
                    #request that IB fill in missing info.                
                    details <- try(suppressWarnings(reqContractDetails(tws,contract)),silent=TRUE)
                    if (length(details) == 0) {
                        if ( (contract$include_expired == 0 ||
                                contract$include_expired == "0" ||
                                !isTRUE(contract$include_expired)) && 
                             (is.null(contract$sectype) ||
                                (!is.null(contract$sectype) && any(contract$sectype == c("FUT","OPT","FOP","BAG")))) ) 
                        {
                            if (verbose) cat("Trying to resolve error in contract details. Using include_expired=1\n")              
                            contract$include_expired <- "1"
                            details <- try(suppressWarnings(reqContractDetails(tws,contract)),silent=TRUE)
                        }
                    }   
                    if (length(details) == 0) {
                        if ( is.null(contract$sectype) || (!is.null(contract$sectype) && (contract$sectype == 'STK')))
                        {
                            if (verbose) cat("Trying to resolve error in contract details. Using sectype='IND'\n")
                            contract$sectype <- 'IND'
                            contract$exch <- ""
                            details <- try(suppressWarnings(reqContractDetails(tws,contract)), silent=TRUE)
                        }
                    }
                    if (length(details) > 0 && !is.instrument(getInstrument(details[[1]]$contract$currency,type='currency',silent=TRUE))) {
                        if (verbose) cat("Checking to see if other 'type's have a pre-defined currency.\n")                
                        tmpcontract <- contract
                        tmpcontract$sectype <- switch(tmpcontract$sectype, STK='IND', 'STK')
                        tmpcontract$exch <- switch(tmpcontract$sectype, IND="", STK=,OPT="SMART", CASH='IDEALPRO')
                        tmpdetails <-  try(suppressWarnings(reqContractDetails(tws,tmpcontract)), silent=TRUE)
                        if (length(tmpdetails) > 0 && is.instrument(getInstrument(tmpdetails[[1]]$contract$currency,type='currency',silent=TRUE))) {
                            details <- tmpdetails
                            instr$type <- switch(details[[1]]$contract$sectype, STK='stock',IND='synthetic',OPT='option',FUT='future',CASH='exchange_rate')
                        }
                    }               
                } else cat('Could not connect to tws.')
            }) #end nested tryCatch  
	    },finally=twsDisconnect(tws)) #End outer tryCatch 

        if (length(details) == 0) {
            uc <- contract
            details <- NULL
            addIBslot <- FALSE                        
            stop(paste('Could not create valid twsContract.\n', 
                contract$symbol, ' may not be a valid ', contract$sectype, 
                '.\nDisconnected.\n', sep=""))
        } else { 
            details <- details[[1]]
		    uc <- details[["contract"]] #updated contract
            uc$include_expired <- contract$include_expired #FIXME: IBrokers:::reqContractDetails overwrites include_expired	
            if (uc$sectype != 'FUT' && uc$sectype != 'OPT') uc$include_expired <- ""    
            if (verbose) {
                cat(paste('Request complete: ',
                        paste(uc$symbol, uc$sectype, uc$currency), '.\nDisconnected.\n', sep=""))
            }
        }
    } else {
		warning(paste(primary_id, 'is not a tradeable currency pair.'))
		addIBslot = FALSE		
		uc <- contract
		details <- NULL	
	}
    if(ambiguous && !silent) warning(paste(instr$primary_id, "is of an ambiguous format. Make sure the type is what you wanted.")) 
    if (any(parse_id(gsub(" ","",uc$local))$type == "SSF") && !any(parse_id(primary_id)$type == "SSF") && !silent) {
        warning('Returning SSF. If this is not what you want make sure your expiration month is valid.')
    }  
    #make sure the currency is defined for this product
    tmpccy <- try(getInstrument(uc$currency, silent=TRUE),silent=TRUE)
    if ( (inherits(tmpccy, 'try-error') || !inherits(tmpccy,'currency') )
          && assign_c) {
    #FIXME: is.currency calls getInstrument on whatever is passed to it, but
    #getInstrument throws a ton of warnings if you pass something with length > 1
    #So, 2 problems: is.currency is FALSE if you don't pass it a string.
    #and, getInstrument doesn't check length of pattern before grep'ing    
        if (!is.null(uc) && !is.null(uc$currency)) {
            currency(uc$currency)
            if(!silent) warning(paste("Creating currency ", uc$currency))   
        }
    }
    #If the instrument doesn't exist, create it, unless assign_i==FALSE    
    #tmpinstr <- try(getInstrument(primary_id),silent=TRUE)
    if (is.null(instr) || inherits(instr, 'try-error') || !is.instrument(instr)) {    
    #chances are, you got here by giving symbol a name instead of an instrument or contract
#    if (inherits(tmpinstr,'try-error') || !is.instrument(tmpinstr)) {
        updateInstrument <- TRUE
        cat(paste("Attempting to create instrument", primary_id,'.\n'))
        instr <- NULL #this line shouldn't be necessary, but it doesn't hurt
    } else {
        if (addIBslot && !updateInstrument) {
            instr$IB <- uc       
            if (!is.null(instr$type)) 
                tclass <- unique(c('twsInstrument', instr$type, 'instrument'))                     
            class(instr) <- tclass
            if (assign_i) {
                assign(primary_id, instr, pos=.instrument)        
            }    
        }    
    }


    if (updateInstrument) { # && assign_i) {
        instr$primary_id <- primary_id
        instr$currency <- uc$currency
        instr$identifiers <- unique(c(instr$identifiers, list(conId=uc$conId, local=gsub(" ","",uc$local))))
        instr$local <- uc$local
        instr$IB.primary.exch <- uc$primary
        instr$exchange <- uc$exch #ok to overwrite 'SMART' ?         
        switch(uc$sectype,
            IND={
                instr$type <- unique(c(instr$type,'synthetic'))
                instr$multiplier <- 1
            }, 
            STK={
                instr$type <- unique(c(instr$type,'stock'))
                instr$multiplier <- 1
            }, 
            OPT={
                instr$type <- unique(c(instr$type,'option')) 
                instr$multiplier <- as.numeric(uc$multiplier)
                instr$expires <- if (nchar(uc$expiry) == 8) {
                        paste(substr(uc$expiry,1,4),substr(uc$expiry,5,6),substr(uc$expiry,7,8),sep="-")
                    } else uc$expiry
                instr$strike <- uc$strike
                instr$right <- uc$right
            }, 
            FUT={
                instr$type <- unique(c(instr$type,'future'))
                instr$multiplier <- as.numeric(uc$multiplier)
                instr$expires <- if (nchar(uc$expiry) == 8) { 
                        paste(substr(uc$expiry,1,4),substr(uc$expiry,5,6),substr(uc$expiry,7,8),sep="-")
                    } else uc$expiry
                iblocal <- uc$local
                si <- if (is.null(instr$suffix_id)) {
                        parse_id(gsub(" ","",iblocal))$suffix
                    } else instr$suffix_id
                primary_id <- paste(contract$symbol,si,sep="_")
                primary_id <- gsub(" ","",primary_id)
                instr$primary_id <- primary_id
                instr$suffix_id <- gsub(" ","",si)
            },
            CASH={
                instr$type <- unique(c(instr$type,'currency'))
                instr$multiplier <- 1            
            },{} ) #End switch on sectype
        #the rest of these may not work in the future, because, as I understand it, 
        #the IB API event that provides them has been deprecated.
		if (uc$sectype != "CASH" || (instr$currency != instr$primary_id)) {
		#we don't have this info for non-tradeable base currency			    
			instr$tick_size <- as.numeric(details$minTick)        
		    instr$longName <- details$longName
		    instr$industry <- details$industry
		    if (!is.null(details) && details$contractMonth != "") 
                instr$contract_month <- details$contractMonth
		    instr$category <- details$category
		    instr$subcategory <- details$subcategory
		    instr$timeZoneId <- details$timeZoneId
		}  #End deprecated

        tmptype <- switch(instr$type[1],
                future=c('future_series','future'),
                option=c('option_series','option'),
                instr$type)
        if (addIBslot) {
			instr$IB <- uc 
			tclass <- unique(c(tmptype,'twsInstrument','instrument'))		
        } else tclass <- unique(c(tmptype,"instrument")) 
        #update info about where & when the instrument was updated
        instr$defined.by <- paste(c(instr$defined.by, "IB"), collapse=";")
        db <- instr$defined.by
	    if (!is.null(db)) {
	        db <- unlist(strsplit(db,";"))
	        db <- rev(unique(c("IB", rev(db))))
	        db <- paste(db,collapse=";") 
	    } else db <- "IB"
        instr$updated <- Sys.time()    
        
        class(instr) <- tclass 
		#Put instr back in the .instrument environment        
        if (assign_i || output=='nothing') {
                if (!is.null(instr$primary_id) && 
                    instr$primary_id != "" &&
                    instr$currency != "" &&
                    instr$multiplier != "")
            assign(primary_id, instr, pos=.instrument)        
                    
        }    
    }

    switch (output[1], 
            contract =, 
            contr =,
            Contract = {uc}, 
            instrument =,
            Instrument =,
            twsInstrument =, 
            instr={instr},
            symbol =,
            symbols =,
            Symbol =,
            primary_id = {primary_id},
            nothing={invisible()} )
}





