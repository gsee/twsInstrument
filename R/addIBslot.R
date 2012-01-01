#' Create a twsContract and store it in a slot of an instrument
#' 
#' Creates a twsContract object, and slot in the corresponding instrument, and
#' places the object in the slot.
#' 
#' This is an old function that is not terribly useful anymore.  It should
#' probably be deprecated It is essentially a wrapper for twsContract, but it
#' doesn't check to make sure the twsContract is valid.
#' 
#' The instrument must be defined before calling this function.
#' 
#' adds twsInstrument class to the instrument
#' 
#' @param symbols vector of character names of FinancialInstrument instruments
#' @param sectype type of security
#' @param exch exchange
#' @param primary primary exchange of security
#' @param expiry expiry
#' @param strike strike price
#' @param currency requested currency
#' @param right right
#' @param local local security name
#' @param multiplier contract multiplier
#' @param include_expired should expired contracts be included
#' @param conId IB contract ID
#' @return Called for it's side effect
#' @note TODO: create instrument if it doesn't already exist
#' @author Garrett See
#' @seealso see instead: \code{\link{twsInstrument}} twsContract, instrument
#' @examples
#' 
#' \dontrun{
#' currency('USD')
#' stock('XOM','USD',1)
#' addIBslot('XOM')
#' 
#' # This is BETTER:
#' stock("CVX","USD")
#' instrument_attr("CVX","IB",getContract("CVX"))
#' # the above will add the twsInstrument class 
#' # if you have FinancialInstrument version 0.7.2 or greater
#' 
#' # So is this:
#' twsInstrument(stock("BP","USD")) #will update other details of instrument by default
#' }
#' @export
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
            assign(symbol,instr,pos=FinancialInstrument:::.instrument)    
        }
    }

