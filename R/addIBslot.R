addIBslot <- function (symbols, sectype="STK", exch = "SMART", primary = "", expiry = "",
    strike = "0.0", currency = "USD", right = "", local = "", multiplier = "", 
    include_expired = "0", conId = 0) 
    {
        if (is.xts(symbols)) stop("Provide vector of character names of xts object, not the objects themselves")
        for (symbol in symbols) {        
            instr <- try(getInstrument(symbol))
            if (inherits(instr,'try-error') || !is.instrument(instr)) {
                #TODO: create the instrument here with warning, and adding addIBslot class and type
                stop(paste('Create an instrument for ', symbol, ' first.',sep="")) 
            }
            instr$IB <- twsContract(conId=conId, symbol=symbol, sectype=sectype, exch=exch, 
                    primary=primary, expiry=expiry, strike=strike, currency=currency, 
                    right=right, local=local, multiplier=multiplier, 
                    combo_legs_desc=NULL, comboleg=NULL, include_expired=include_expired)
            tclass <- unique(c('twsInstrument', instr$type, 'instrument'))
            class(instr) <- tclass
            assign(symbol,instr,pos=.instrument)    
        }
    }

