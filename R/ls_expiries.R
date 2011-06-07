
ls_expiries <- function(underlying_id=NULL, type='derivative') {
    if (is.null(underlying_id))
        underlying_id <- ls_underlyings()
    symbols <- do.call(eval(paste('ls_',type,"s",sep="")),args=list(pattern=NULL) ) #symbols == all derivatives by default
    dates <- NULL   
    underlyings <- NULL
    for (symbol in symbols) { 
        tmp_instr <- try(get(symbol,pos=.instrument),silent=TRUE)
        if (!is.null(tmp_instr$underlying_id) && any(tmp_instr$underlying_id==underlying_id)) { #the underlying_id of this instr mathces one of the one's we're interested in.
        underlying <- tmp_instr$underlying_id            
            if (is.null(tmp_instr$expires)) { #get value for expiry; may be in 'expires' or 'expiry' slot
                if (!is.null(tmp_instr$expiry)) {
                    expiry <- tmp_instr$expiry
                } else expiry <- NULL
            } else expiry <- tmp_instr$expires
        dates <- c(dates, expiry)
        if (!is.null(expiry)) underlyings <- c(underlyings, underlying)                
        }
        #ll <- list(expiry)
        #names(ll) <- underlying
        #dates <- c(dates, ll)
    }
    #cbind(underlyings,dates[-which(duplicated(underlyings))]) 
    expires <- dates[-which(duplicated(dates))]
    names(expires) <- underlyings[-which(duplicated(dates))]    
    expires
#    underlying_id <- underlyings[-which(duplicated(dates))]
#    names(underlying_id) <- dates[-which(duplicated(dates))]    
#    data.frame(underlying_id)    
}
ls_expires <- ls_expiries

#ls_instruments_by('expires','20110916')


