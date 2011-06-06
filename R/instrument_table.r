#buildHierarchy(ls_options(),levels=c('strike','blah'))

#instrument.table(ls_stocks('S'), excl_id='SE')
#that should get instruments "S" and "SEE"


#get a data.frame of all instruments' attributes
instrument.table <- function(symbols=NULL, exclude = NULL, attrs.of = NULL) {
#TODO check for numeric/character
    if (is.null(symbols)) symbols <- ls_instruments()
    if (is.null(attrs.of)) attrs.of <- ls_instruments()
   
    attr.names <- NULL
    for (symbol in attrs.of) {
        instr <- try(getInstrument(symbol,silent=TRUE),silent=TRUE)            
        if (!inherits(instr,'try-error') 
                && inherits(instr, 'instrument') ) {
            attr.names <- unique(c(attr.names, names(unlist(instr))))               
        }
    }
    if (length(exclude) > 1) 
        exclude <- paste(exclude, collapse='|')
    if (length(exclude == 1)) #i.e. if (!is.null(exclude))
        attr.names <- attr.names[-grep(exclude, attr.names, ignore.case=TRUE)]
    out <- buildHierarchy(symbols,attr.names)
  #FIXME: doesn't work if there is only 1 symbol.
    data.frame(out[,-1], row.names=as.character(out[,1]))
}

#This version works
#instrument.table
#function (exclude = NULL, attrs.of = NULL) 
#{
#    if (is.null(attrs.of)) {
#        lengths <- sapply(.instrument, length)
#        labels <- lapply(.instrument, names)
#        primary_ids <- names(labels)
#        attrs.of <- which.max(sapply(lengths, length))
#    }
#    levels <- labels[[attrs.of]]
#    if (length(exclude) > 1) 
#        exclude <- paste(exclude, collapse = "|")
#    if (length(exclude == 1)) 
#        levels <- levels[-grep(exclude, levels, ignore.case = TRUE)]
#    out <- buildHierarchy(primary_ids, levels)
#    data.frame(out[, -1], row.names = as.character(out[, 1]))
#}

#library(IBtools)
#data(SP500desc)
#define_stocks()
#it <- instrument.table()
#NROW(it);NCOL(it)

#TODO: make templates for options, futures, stocks, currencies, exchange_rates
#yahoo_ia


#IB_ia
#twsInstrument_ia

#stock_ia
#option_ia
#future_ia
#exchange_rate_ia








