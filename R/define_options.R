#  future('CL',currency='USD',multiplier=1000,tick_size=0.01,
#  exchange='CME',description=paste('Crude Light Futures, BBO'),underlying_id='CL')
#	for (i in 1:length(grep('CL',OR.symbols))) {	
#		future(paste('CL',i,sep='_'),currency='USD',multiplier=1000,tick_size=0.01,exchange='CME',description=paste('Crude Light Futures, BAM, Term ',i,sep=''),underlying_id='CL')
	#	future_series('CLM',paste(i))
#	}
  
#r <- 'SPY110630C00115000'
#symbol <- "SPY"


option_id <- function(underlying_id, expiry, right, strike) {
	#TODO: some checking to see what format expiry is (i.e. Date, character string, CCYYMMDD, CCYY-MM-DD,etc.)	
    #si <- gsub(symbol,"",r)
	#id <- paste(primary_id,suffix_id,sep="_")
    #expiry <- paste('20',substr(si,1,6),sep="")
	if (right == 'call' || right == 'Call' || right == 'CALL' || right == 'c') right<-'C'
	if (right == 'put' || right == 'Put' || right == "PUT" || right == 'p') right <- 'P'    
   
	paste(".", underlying_id, "_", expiry, right, strike, sep="")
}


define_options <- function(symbol, Exp, currency="USD", multiplier=100, tick_size=NULL, first_traded=NULL, src='yahoo') { 
    if (src == 'yahoo')	
    do.call('define_options.yahoo', list(symbol=symbol, currency=currency, 
		multiplier=multiplier, tick_size)) #,first_traded=first_traded,
}

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
			option(primary_id=paste('.',symbol,sep=""), currency=currency,
					multiplier=multiplier, tick_size=tick_size, 
					underlying_id=symbol)		
		}
        instrument(primary_id=primary_id, 
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

		




#rm_options()
#define_options(symbol='SPY')
#define_options(symbol='DIA')


#reqContractDetails(tws, twsOPT(local=paste('SPY','110630C00115000',sep="   "))) 


  
    
  
  
