ls_by_currency <- function(currency, pattern=NULL, match=TRUE,show.currencies=FALSE) {
    if (length(pattern) > 1 && !match) {
        warning("Using match because length of pattern > 1.")
        #FIXME: should I use match?
        #or, ignore pattern and return everything?
        #or, do multiple ls calls and return unique
        match <- TRUE    
    }    

	if (!(currency %in% ls_currencies()) ) {
		warning(paste(currency, 'is not a defined currency', sep=" "))
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
        if (is.instrument(tmp_instr) && 
          tmp_instr$currency == currency ){    
            tmp_symbols <- c(tmp_symbols,symbol)
        }    
    }
    if (show.currencies) {
      tmp_symbols
    } else if (!is.null(tmp_symbols)) {
		ls_non_currencies(tmp_symbols) 
	} else NULL
}

rm_by_currency <- function(x,currency,keep.currencies=TRUE) {
    sc <- !keep.currencies #make show.currencies==opposite of keep
    if (missing(x)) {
        x <- ls_by_currency(currency,show.currencies=sc)
    } else x <- ls_by_currency(currency,pattern=x,show.currencies=sc)
    rm(list=x,pos=.instrument)
}

#AUD GBP CAD EUR JPY CHF HKD SEK NZD
ls_USD <- function(pattern=NULL,match=TRUE,show.currencies=FALSE) {
    ls_by_currency('USD',pattern,match,show.currencies) 
}
ls_AUD <- function(pattern=NULL,match=TRUE,show.currencies=FALSE) {
    ls_by_currency('AUD',pattern,match,show.currencies) 
}
ls_GBP <- function(pattern=NULL,match=TRUE,show.currencies=FALSE) {
    ls_by_currency('GBP',pattern,match,show.currencies) 
}
ls_CAD <- function(pattern=NULL,match=TRUE,show.currencies=FALSE) {
    ls_by_currency('CAD',pattern,match,show.currencies) 
}
ls_EUR <- function(pattern=NULL,match=TRUE,show.currencies=FALSE) {
    ls_by_currency('EUR',pattern,match,show.currencies) 
}
ls_JPY <- function(pattern=NULL,match=TRUE,show.currencies=FALSE) {
    ls_by_currency('JPY',pattern,match,show.currencies) 
}
ls_CHF <- function(pattern=NULL,match=TRUE,show.currencies=FALSE) {
    ls_by_currency('CHF',pattern,match,show.currencies) 
}
ls_HKD <- function(pattern=NULL,match=TRUE,show.currencies=FALSE) {
    ls_by_currency('HKD',pattern,match,show.currencies) 
}
ls_SEK <- function(pattern=NULL,match=TRUE,show.currencies=FALSE) {
    ls_by_currency('SEK',pattern,match,show.currencies) 
}
ls_NZD <- function(pattern=NULL,match=TRUE,show.currencies=FALSE) {
    ls_by_currency('NZD',pattern,match,show.currencies) 
}
