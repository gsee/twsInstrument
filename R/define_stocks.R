define_stocks <-
function(x=SP500desc, currency='USD', file=NULL, use.yahoo=TRUE, use.IB=TRUE, addIBslot=TRUE) { #, ...) {	
    if (is.null(currency)) currency <- ''    
    if (currency != '' && length(currency) == 1) {
        tmpccy <- try(getInstrument(currency,silent=TRUE), silent=TRUE)      
        if (!inherits(tmpccy,'currency') || 
                ( inherits(tmpccy,'currency') && inherits(tmpccy, 'exchange_rate') ) ) 
        #i.e. if it isn't a currency, or it is an exchange_rate
        {
            warning(paste('Created currency',currency)) 
            currency(currency)
        }            
    } else if (!use.IB) stop("Must provide currency if use.IB is FALSE.")
    #the logical thing to do is create a stock without a currency, and let IB figure it out
    #but, FinancialInstrument requires a currency to be defined, so I either have to guess
    #the currency or throw an exception.  For now, I'll use a mix; if use.IB is FALSE, 
    #throw an error, but if use.IB is TRUE I'll use MY instrument.tws function, and then try
    #to get the currency from IB.
                           
	if (is.vector(x) & is.character(x)) {
        symbols <- x
        for (stk in symbols) {
            #stock(stk, currency, defined.by='hand', updated=Sys.time() )
            instrument.tws(primary_id=stk, currency=currency, multiplier=1, tick_size=0.01,
                        identifiers=NULL        , defined.by='hand', updated=Sys.time(),
                        type = "stock", assign_i = TRUE)
        }
    } else {
        if (is.null(file)) {
            if (is.data.frame(x)) {		
                symdesc <- x
	        	file <- paste(deparse(substitute(x)))	
            }
        } else symdesc <- read.csv(file=file)	
        if (is.data.frame(symdesc)) {
          	for (i in 1:length(symdesc[,1])) {
                instrument.tws(primary_id=as.character(symdesc[i,1]),
                    currency = currency,
                    multiplier = 1,
                    tick_size = 0.01,
                    identifiers = NULL,
	                description=as.character(symdesc[i,2]),
	                industry.division=as.character(symdesc[i,3]),
	                industry.group=as.character(symdesc[i,4]),
	                industry.sector=as.character(symdesc[i,5]), 
	                defined.by=file, 
                    updated=Sys.time(), 
                    type = "stock", 
                    assign_i = TRUE )
            }
        } 
        symbols <- as.character(symdesc[,1])
    }
    if (use.yahoo) {	
        update_instruments.yahoo(symbols)	    
    }
    if (use.IB) {
        update_instruments.IB(symbols,addIBslot=addIBslot)
    }
    
}



