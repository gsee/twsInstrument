#TODO: allow for more date formats. 

ls_by_expiry <- function(expiry, pattern=NULL, match=TRUE) {
    if (length(pattern) > 1 && !match) {
        warning("Using match because length of pattern > 1.")
        #should I use match even though it's TRUE?
        #or, ignore pattern and return everything?
        #or, do multiple ls calls and return unique
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
        if (is.instrument(tmp_instr) && !is.null(tmp_instr$expires) ) {
        	if (any(tmp_instr$expires == expiry) ){    
				tmp_symbols <- c(tmp_symbols,symbol)
    		}	        
        }    
    }

    tmp_symbols
}

#remove x by expiry
rm_by_expiry <- function(x,expiry) {
    if (missing(x)) {
        x <- ls_by_expiry(expiry)
    } else x <- ls_by_expiry(expiry,pattern=x)
    rm(list=x,pos=.instrument)
}
#rm_by_expiry(ls_options(),'20130119')


#TODO: ls_by_underlying
        #if (it's  %in% ls_derivatives())
