#TODO: I would like to extend this so that 
#(1)you only need to give it an underlying
#(2)if you give it several underlyings, it will give you list or named vector of strikes
#(3)include an argument for expiration date

ATM_k <- function(options, verbose=TRUE) {

    underlying <- ls_underlyings(options)
    if (length(underlying) > 1) {
        warning(paste('More than one underlying found in options. ',
                        'Finding At-The-Money strike for ',
                        underlying[1], sep=""))
    }

    underlying <- underlying[1]
    expiries <- ls_expiries(underlying)
    if (length(expiries) > 1) {
        warning(paste(
                    'More than 1 expiration date found in options. \n Using ',
                    expiries[1], ' to find At-The-Money Strike.', sep=""))
    }
    expiry <- expiries[1]

    if (verbose) cat(paste('Finding At-The-Money strike of the ', 
                    expiry, ' options on ', underlying, '\n', sep="")) 
    uLast <- getQuote(underlying)$Last
    strikes <- ls_strikes(options)    
    strikes[which.min(abs(uLast-strikes))]
   
}

