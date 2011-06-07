ls_underlyings <- function(pattern=NULL) {
    symbols <- ls_derivatives(pattern)
    tmp_symbols <- NULL
    for (symbol in symbols) {
        tmp_instr <- try(get(symbol,pos=.instrument),silent=TRUE)
        #if (is.instrument(tmp_instr))  
        if (!is.null(tmp_instr$underlying_id)) 
            tmp_symbols <- c(tmp_symbols,tmp_instr$underlying_id)
    }
    unique(tmp_symbols)    
}

