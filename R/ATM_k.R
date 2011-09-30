#TODO: I would like to extend this so that 
#(1)you only need to give it an underlying
#(2)if you give it several underlyings, it will give you list or named vector of strikes
#(3)include an argument for expiration date



#' At-the-money strike
#' 
#' Returns the At-The-Money strike price of an option chain
#' 
#' Very preliminary version. Currently, you give it a bunch of names of
#' options, it gets the most recent price of the underlying using
#' \code{getQuote} and returns the strike price that is closest to the last
#' price. See notes.
#' 
#' @param options vector of option names that have been previously defined
#' @param verbose be verbose?
#' @return numeric
#' @note TODO: I would like to extend this so that (1)you only need to give it
#' an underlying (2)if you give it several underlyings, it will give you list
#' or named vector of strikes (3)include an argument for expiration date
#' @author Garrett See
#' @seealso ATMQuote ls_options, ls_underlyings, ls_strikes, ls_instruments_by
#' @examples
#' 
#' \dontrun{
#' option_series.yahoo('SPY')
#' ATM_k(ls_options())
#' ATM_k(ls_options('SPY', match=FALSE))
#' ATM_k(ls_options(ls_instruments_by('underlying_id','SPY')))
#' 
#' Exp <- ls_expiries(underlying_id='SPY', type='option')[1] #1st expiry of SPY options
#' ATM_k(ls_by_expiry(Exp))
#' }
#' @export
ATM_k <- function(options, verbose=TRUE) {
    underlying <- ls_underlyings(options)
    if (length(underlying) > 1) {
        warning(paste('More than one underlying found in options. ',
                        'Finding At-The-Money strike for ',
                        underlying[1], sep=""))
    }
    underlying <- underlying[1]
    expiries <- ls_expiries(underlying_id=underlying)
    if (length(expiries) > 1) {
        warning(paste(
                    'More than 1 expiration date found in options. \n  Using ',
                    expiries[1], ' to find At-The-Money Strike.', sep=""))
    }
    expiry <- expiries[1]

    if (verbose) cat(paste('Finding At-The-Money strike of the ', 
                    expiry, ' options on ', underlying, '\n', sep="")) 
    uLast <- getQuote(underlying)$Last
    strikes <- ls_strikes(options)    
    strikes[which.min(abs(uLast-strikes))]
}

