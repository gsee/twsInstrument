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

#' updates instrument metadata with data from IB
#' 
#' Adds/updates information in instrument with data downloaded from IB
#' 
#' These are basically wrappers for buildIBcontract. With these functions you
#' can update some or all instruments' information from IB.
#' 
#' if you call update_instruments.IB with one of \sQuote{all} or
#' \sQuote{stocks}, \sQuote{futures}, \sQuote{options}, \sQuote{currencies}, it
#' is the same as calling it with the relevant ls_ function (e.g. ls_stocks()).
#' Therefore, functionality can be extended by using ls_ functions instead of a
#' descriptive string.
#' 
#' @aliases update_instruments.IB update_instruments.all
#' @param Symbols can be a vector of instrument names, or, can be \sQuote{all}
#' or \sQuote{stocks} or, can also be \sQuote{futures}, \sQuote{options},
#' \sQuote{currencies}
#' @param addIBslot Boolean. should an IB slot be added to the instrument,
#' making it a twsInstrument?
#' @param updateInstrument Boolean. Should data outside the IB slot also be
#' updated?
#' @param include_expired Should expired contracts be included in
#' reqContractDetails call? "0" for no, "1" for yes (default).
#' @param assign_i should the instrument be stored in .instrument environment.
#' @param assign_c If a new currency is discovered, should it be created
#' @param ... pass through arguments for \code{\link{update_instruments.IB}} and 
#' \code{\link[FinancialInstrument]{update_instruments.TTR}}
#' @return called for side-effect
#' @author Garrett See
#' @seealso FinancialInstrument:::update_instruments.yahoo, twsInstrument, define_stocks
#' \url{www.interactivebrokers.com} IB API
#' \url{http://interactivebrokers.com/php/apiUsersGuide/apiguide.htm}?
#' @examples
#' \dontrun{	
#' 	stock('GS',currency('USD'))
#' 	update_instruments.IB('GS') 
#'  getInstrument("GS")
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


