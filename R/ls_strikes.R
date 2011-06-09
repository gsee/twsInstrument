ls_strikes <- function(pattern=NULL) {
    symbols <- ls_options(pattern)
    tmp_symbols <- NULL
    for (symbol in symbols) {
        tmp_instr <- try(get(symbol,pos=.instrument),silent=TRUE)
        #if (is.instrument(tmp_instr))  
        if (!is.null(tmp_instr$strike)) 
            tmp_symbols <- c(tmp_symbols,tmp_instr$strike)
    }
    unique(tmp_symbols)    
}


