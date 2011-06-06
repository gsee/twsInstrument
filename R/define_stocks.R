define_stocks <-
function(x=SP500desc, currency='', file=NULL, use.yahoo=TRUE, use.IB=TRUE, addIBslot=TRUE) { #, ...) {	
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
    #throw an error, but if use.IB is TRUE I'll use MY instrument function, and then try
    #to get the currency from IB.
                           
	if (is.vector(x) & is.character(x)) {
        symbols <- x
        for (stk in symbols) {
            stock(stk, currency, defined.by='hand', updated=Sys.time() )
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
                stock(as.character(symdesc[i,1]),'USD',1,
	                description=as.character(symdesc[i,2]),
	                industry.division=as.character(symdesc[i,3]),
	                industry.group=as.character(symdesc[i,4]),
	                industry.sector=as.character(symdesc[i,5]), 
	                defined.by=file, updated=Sys.time() )
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



