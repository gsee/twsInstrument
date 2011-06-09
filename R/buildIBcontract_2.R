#given a symbol, twsInstrument, instrument, instrument name, or twsContract,
#these functions will create a twsInstrument with as much information
#as can be found.

#implemented: STK,OPT,FUT,CASH
#not implemented: IND,FOP

is.twsInstrument <- function(x) {
    if (inherits(x, 'twsInstrument')) {
        TRUE    
    } else if (inherits(x, 'instrument') && !is.null(x$IB)) {
        TRUE 
    } else FALSE
}

#Any of these will create a twsInstrument object

Contr_From_Instr <- function(instrument, tws=NULL, 
		addIBslot=FALSE, updateInstrument=FALSE, 
		output=c('contract','symbol','nothing','instrument'), 
		include_expired="0", assign_i=FALSE, assign_c=TRUE) 
{
    if (is.numeric(output))
        output <- c('contract','symbol','nothing','instrument')[output]
    buildIBcontract(symbol=instrument, tws=tws, addIBslot=addIBslot,
        updateInstrument=updateInstrument, output=output[1], 
        include_expired=include_expired, 
        assign_i=assign_i, assign_c=assign_c)
}

Instr_From_Contr<- function(contract, tws=NULL, 
		addIBslot=FALSE, updateInstrument=TRUE, 
		output=c('instrument','symbol','nothing','contract'), 
		include_expired="0", assign_i=TRUE, assign_c=TRUE) 
{
    if (is.numeric(output)) 
        output=c('instrument','symbol','nothing','contract')[output]
    buildIBcontract(symbol=contract, tws=tws, addIBslot=addIBslot,
        updateInstrument=updateInstrument, output=output[1],
        include_expired=include_expired, 
        assign_i=assign_i, assign_c=assign_c)
}

twsInstrument <- function(symbol, tws=NULL, 
        addIBslot=TRUE, updateInstrument=TRUE, 
        output=c('nothing','symbol','instrument','contract'), 
        include_expired="0", assign_i=TRUE, assign_c=TRUE)
{
    if (is.numeric(output)) 
        output <- c('nothing','symbol','instrument','contract')[output]
    buildIBcontract(symbol=symbol, tws=tws, addIBslot=addIBslot,
            updateInstrument=updateInstrument, output=output, 
            include_expired=include_expired, 
            assign_i=assign_i, assign_c=assign_c)
} 


buildIBcontract <- function(symbol, tws=NULL, 
		addIBslot=FALSE, updateInstrument=FALSE, 
		output=c('contract','instrument','symbol','nothing'), 
		include_expired="0", assign_i=FALSE, assign_c=TRUE)
{
    #TODO: Allow for vector of symbols, instruments, or contracts
    if (is.xts(symbol)) stop('symbol can be the name of an xts object, but not the object itself.')    
    primary_id <- NULL
    right <- NULL    
    contract <- NULL
    instr <- NULL        
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
		        warning(paste("Creating currency ", contract$currency))   
            } else stop (paste(contract$currency, 'cannot be found, and assign_c=FALSE'))
		}        
	    #primary_id <- symbol
        #TODO: Include conId as an identifier
        #FIXME: these instrument constructor wrappers assign_i=TRUE; 
        #       I need to replace wrappers with direct call to instrument    
        instr <- switch(contract$sectype, 
                STK={
                    primary_id <- contract$symbol                    
                    #stock(primary_id=primary_id, currency=contract$currency, exchange=contract$exchange)
                    instrument(primary_id=primary_id, currency=contract$currency,multiplier=1,
                                tick_size=0.01, identifiers=NULL, type='stock', assign_i=FALSE)
                }, 
                OPT={
#                   primary_id <- paste('.', contract$symbol,sep="")
					ylocal <- gsub("   ","",contract$local)#take out the triple space
					si <- gsub(contract$symbol,"",ylocal)
					#id <- paste(primary_id,suffix_id,sep="_")
					expiry <- substr(si,1,6)
					right <- substr(si,7,7)
					strike <- as.numeric(substr(si,8,15))/1000
					#local <- paste(symbol, si, sep="   ")      
					primary_id <- paste(".", contract$symbol, "_", expiry, right, strike, sep="")
		
                    #option(primary_id=primary_id, currency=contract$currency,
                    #    multiplier=contract$multiplier, expires=contract$expiry, right=contract$right,
                    #    strike=contract$strike, exchange=contract$exchange, underlying_id=contract$symbol)
                    instrument(primary_id=primary_id, currency=contract$currency,
                        multiplier=as.numeric(contract$multiplier), tick_size=NULL, 
                        identifiers=NULL, expires=contract$expiry, right=contract$right, 
                        strike=contract$strike, exchange=contract$exchange, type='option', 
                        underlying_id=contract$symbol, assign_i=FALSE)

                }, 
                FUT={
                    primary_id <- symbol
                    #future(primary_id=primary_id, currency=contract$currency, 
                    #    multiplier=as.numeric(contract$multiplier), expires=contract$expiry, 
                    #    exchange=contract$exch, underlying_id=contract$symbol) #maybe shouldn't specify exchange here
                    instrument(primary_id=primary_id, currency=contract$currency, 
                        multiplier=as.numeric(contract$multiplier), tick_size=NULL,
                        expires=contract$expiry, exchange=contract$exch, type='future', 
                        underlying_id=contract$symbol, assign_i=FALSE) #maybe shouldn't specify exchange here
                }, 
                CASH={ 
                    if (contract$local == "") {
                        primary_id <- paste(contract$symbol, contract$currency, sep=".") #will be contract$local after update
                    } else primary_id <- contract$local
                    #exchange_rate(primary_id=primary_id, currency=contract$currency, second_currency=contract$symbol)
                    instrument(primary_id=primary_id, currency=contract$currency, multiplier=1, 
                                tick_size=0.01, identifiers=NULL, second_currency=contract$symbol, 
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
#(if we got instr from a string, it may not have succeed. we'll check for that next.)

    if (inherits(instr,'try-error') || !is.instrument(instr)) {
        warning(paste("Unable to find or infer instrument, ",
                symbol, ".\n  Trying with type = \"stock\"", sep=""))
#        contract <- twsSTK(primary_id)        
        contract <- twsContract() #blank twsContract shell
        contract$symbol <- primary_id #symbol #fill in the only info we have
        contract$sectype <- "STK"
        #instr is still not an instrument!
		instr <- NULL #Have to reset it because getInstrument gave it a value of FALSE
    }


	if (is.instrument(instr) && is.null(instr$IB) && is.null(contract)) { #make contract
	    primary_id <- instr$primary_id
	    #figure out sectype
	    if (is.null(instr$sectype) ) {
			#currencies don't have type by FinancialInstrument default                
            if (inherits(instr,'currency') || 
				(!is.null(instr$type) && instr$type[1] == 'currency') ) {
                sectype <- "CASH"                
            } else if (inherits(instr,'option') || 
                instr$type[1] == "option" || 
                instr$type[1] == "OPT") {
                    sectype <- "OPT"
                    if (!is.null(instr$underlying_id) 
                           && !instr$underlying_id == "") {
                        symbol <- instr$underlying_id
                    }
            } else if (inherits(instr,'future') || 
                instr$type[1] == "future") {
                    sectype <- "FUT"
            } else if (inherits(instr,'stock') || 
                (instr$type[1] == "stock") || 
                (instr$type[1] == "STK")) {
                    sectype <- "STK"                
            } else { 
                stop(paste('Cannot determine sectype; \n', symbol , 
                    'does not appear to be a stock, \noption, future or currency.',
                    sep=""))
            }     
        } else sectype = instr$sectype
        
        if (is.null(contract$conId) || contract$conId == 0) {     
            conId <- 0        
            if ((sectype == "FUT" || sectype == "OPT") && is.null(instr$expires) && is.null(instr$expiry) )
                warning("Expiry not defined for future or option.")   
            if (sectype == "OPT") {
				if (is.null(instr$strike))
					warning("strike is not defined for option.")
 				if (!is.null(instr$callput)) {
					right <- instr$callput
				} else if (!is.null(instr$right)) {
					right <- instr$right
				} else {
					warning("right of option is neither call nor put.")
				    right <- instr$right
				}
			}        
		} else conId <- contract$conId

        #set multiplier
        if (sectype == "STK") { # || sectype == "CASH") {
            multiplier <- ""
            exchange <- 'SMART'
        } else {
            multiplier <- instr$multiplier            
            exchange <- instr$exchange #Should exchange and primary both be the same ?        
        }
        primary <- instr$exchange #should this be "" ?
        if (is.null(instr$expires) && !is.null(instr$expiry)) {
            expiry <- instr$expiry
        } else expiry <- instr$expires
		IBexpiry <- expiry
        #IB uses the Friday before expiration Saturday for expiry
        #except for EOM options.
		if (!is.null(IBexpiry) && is.character(IBexpiry)) {
            if (length(IBexpiry) == 8) {
			    expdate <- as.Date(IBexpiry,format="%Y%m%d")
			    if (weekdays(expdate) == "Saturday") {
				    IBexpiry <- format(expdate - 1,"%Y%m%d")
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

### FIXME: How did I get here and have a NULL contract    
    #Establish a connection, and download contract details from IB. on error: disconnect
	if ( (contract$sectype != "CASH") || 
         (instr$currency != instr$primary_id) || 
         (contract$symbol != contract$currency) ) {		
        # || is.exchange_rate(instr) #no function for this
		tryCatch(
		{    
            tryCatch( 		        
            {
                if (missing(tws) || is.null(tws) || (is.twsConnection(tws) && !isConnected(tws)) ) 
                    tws <- try(twsConnect(),silent=TRUE)
                if (inherits(tws,'try-error')) tws <- try(twsConnect(2),silent=TRUE) #try another clientId
                if (inherits(tws,'try-error')) tws <- try(twsConnect(3),silent=TRUE) #3rd time's the charm
                if (inherits(tws,'try-error')) tws <- twsConnect(50) #a last attempt for an available clientId
            }, finally={ 
                if (isConnected(tws)) {
                    cat(paste('Connected with clientId ', tws$clientId, '.\n',sep=""))    
                    if (tws$clientId == 50) warning("IB Trader Workstation should be restarted.")                    
                    #request that IB fill in missing info.                
                    details <- try(reqContractDetails(tws,contract),silent=TRUE)                
                } else cat('Could not connect to tws.')
            }) #end nested tryCatch  
	    },finally=twsDisconnect(tws)) #End outer tryCatch 
        
        if (length(details) == 0) {
            uc <- contract
            details <- NULL
            addIBslot <- FALSE                        
            cat(paste('Could not create valid twsContract.\n', 
                contract$symbol, ' may not be a valid ', contract$sectype, 
                '. Disconnected.\n', sep=""))
        } else { 
            cat('Contract details request complete. Disconnected.\n')
            details <- details[[1]]
		    uc <- details[["contract"]] #updated contract
	    }
    } else {
		warning(paste(primary_id, 'is not a tradeable currency pair.'))
		addIBslot = FALSE		
		uc <- contract
		details <- NULL	
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
            warning(paste("Creating currency ", uc$currency))   
        }
    }

    #If the instrument doesn't exist, create it, unless assign_i==FALSE    
    #tmpinstr <- try(getInstrument(primary_id),silent=TRUE)
    if (is.null(instr) || inherits(instr, 'try-error') || !is.instrument(instr)) {    
    #chances are, you got here by giving symbol a name instead of instrument or contract
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
#	    if (is.null(instr$identifiers) || !is.list(instr$identifiers)) instr$identifiers <- list()
	    ##FIXME: implement identifiers
	    #if (is.list(instr$identifiers)) instr$identifiers <- c(IB=uc$local,,instr$identifiers)
	    #else instr$identifiers <- list(IB=uc$local)        
        instr$primary_id <- primary_id
        instr$local <- uc$local
#        instr$exchange <- uc$primary
        instr$exchange <- uc$exch #ok to overwrite 'SMART' ? 
        instr$currency <- uc$currency
        switch(uc$sectype, 
            STK={
                instr$type <- unique(c('stock',instr$type))
                instr$multiplier <- 1
            }, 
            OPT={
                instr$type <- unique(c('option',instr$type)) 
                instr$multiplier <- as.numeric(uc$multiplier)
                instr$expires <- uc$expiry
                instr$strike <- uc$strike
                instr$right <- uc$right
            }, 
            FUT={
                instr$type <- unique(c('future',instr$type))
                instr$multiplier <- as.numeric(uc$multiplier)
                instr$expires <- uc$expiry
                instr$strike <- uc$strike
                instr$right <- uc$right
            }, 
            CASH={
                instr$type <- unique(c('currency',instr$type))
                instr$multiplier <- 1            
            },{} ) #End switch on sectype
        #the rest of these may not work in the future, because, as I understand it, 
        #the IB API event that provides them has been deprecated.
		if (uc$sectype != "CASH" || (instr$currency != instr$primary_id)) {
		#we don't have this info for non-tradeable base currency			    
			instr$tick_size <- details$minTick        
		    instr$longName <- details$longName
		    instr$industry <- details$industry
		    if (!is.null(details) && details$contractMonth != "") 
                instr$contract_month <- details$contractMonth
		    instr$category <- details$category
		    instr$subcategory <- details$subcategory
		    instr$timeZoneId <- details$timeZoneId
		}  #End deprecated

        if (addIBslot) {
			instr$IB <- uc 
			tclass <- unique(c('twsInstrument',instr$type,'instrument'))
		} else tclass <- unique(c(instr$type,"instrument")) 
        #update info about where & when the instrument was updated
        instr$defined.by <- paste(c(instr$defined.by, "IB"), collapse=";")
        # ^ Maybe only unique defined.by should be kept?        
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


#contract <- twsContract(conId=0,symbol="XOM", sectype="STK",exch="SMART",primary="NYSE", 
#    expiry="",strike="",currency="USD", right="",local="",multiplier=1,
#    combo_legs_desc="",comboleg="",include_expired="0")
#reqContractDetails(tws,contract)


