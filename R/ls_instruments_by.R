
ls_instruments_by <- function (what, value, pattern=NULL, match=TRUE, in.slot=NULL) {
    if (length(pattern) > 1 && !match) {
        warning("Using match because length of pattern > 1.")
        match <- TRUE
    }
    if (!is.null(pattern) && match) {   #there's a pattern and match is TRUE
        symbols <- ls(.instrument, all.names=TRUE)
        symbols <- symbols[match(pattern,symbols)]
    } else if (!match && length(pattern) == 1) { # pattern is length(1) and match is FALSE
        symbols <- ls(.instrument, all.names=TRUE, pattern=pattern)
    } else if (is.null(pattern)) {  #no pattern
        symbols <- ls(.instrument, all.names=TRUE)
    } # else pattern length > 1 & don't match
        
    tmp_symbols <- NULL 
    for (symbol in symbols) {
        tmp_instr <- try(get(symbol, pos = .instrument),silent=TRUE)
        #TODO: clean this up
        if (is.instrument(tmp_instr)) {
            if (
                if (is.null(value)) {
                    if (is.null(in.slot)) {
                        if (!inherits(try(tmp_instr[[what]],silent=TRUE), 'try-error') && !is.null(tmp_instr[[what]])) {TRUE} else {FALSE}
                    } else if (!inherits(try(tmp_instr[[in.slot]][[what]],silent=TRUE), 'try-error') && !is.null(tmp_instr[[in.slot]][[what]])) {TRUE} else {FALSE}
                } else if (is.null(in.slot)) {
                    if (!is.null(tmp_instr[[what]]) ) {TRUE} else {FALSE}
                } else { #!is.null(value) && !is.null(in.slot)
                    if (!is.null(tmp_instr[[in.slot]][[what]])) {TRUE} else {FALSE}
                }
            ) tmp_symbols <- c(tmp_symbols, symbol)
        }    
    }
    tmp_symbols
}

