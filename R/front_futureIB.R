front_future <- function(roots, currency='USD', underlying_id = NULL, addIBslot=TRUE, src='IB') {
    do.call(paste('front_future',src,sep="."),list(roots=roots,currency=currency, underlying_id=underlying_id, addIBslot=addIBslot))
}

front_future.IB <- function (roots, currency='USD', underlying_id = NULL, addIBslot=TRUE) 
{
    if (!is.character(roots)) stop("roots should be a chr vector.")    

    all.ids <- NULL
    for (root in roots) {
        contract <- twsContract()
        contract$symbol <- root
        contract$currency <- currency
        contract$sectype <- "FUT"
        instr <- Instr_From_Contr(contract,addIBslot=addIBslot,updateInstrument=TRUE,
                        output='instrument',assign_i=FALSE,assign_c=TRUE)

        if (is.null(instr$underlying_id) && is.null(underlying_id)) {
            warning("underlying_id should only be NULL for cash-settled futures")
        } else if (is.null(instr$underlying_id)) {
            instr$underlying_id <- underlying_id
        } else if (!is.null(underlying_id) && !exists(underlying_id, where = .instrument, inherits = TRUE)) {
            warning('underlying_id not defined.')    
        }
        #Do we need to create the root?
        tmproot <- try(get(root,pos=.instrument),silent=TRUE)
        if (!inherits(tmproot,'future')) {
            if (is.instrument(tmproot)) {
                warning(paste(instr$primary_id,
                        " already exists, but it is not futures specs.", sep=""))
                        #"Specs will be stored in ", instr$primary_id, "_fspecs", 
                        #sep="")
                store.to <- paste(instr$primary_id, 'fspecs', sep="_")
                cat(paste('Futures contract specs stored in ', store.to, "\n", sep=""))                
            } else store.to <- root
            instrument.tws(primary_id=store.to, currency=instr$currency, 
                    multiplier=instr$multiplier, tick_size=as.numeric(instr$tick_size),
                    indentifiers=instr$identifiers, type = "future",
                    underlying_id=instr$underlying_id, assign_i = TRUE)
             #future(primary_id=store.to, currency=instr$currency,
#                    multiplier=instr$multiplier, tick_size=as.numeric(instr$tick_size),
#                    identifiers=instr$identifiers, 
#                    underlying_id=instr$underlying_id)                       
        }

        instr$suffix_id <- gsub(root, "", instr$local)    
        id <- paste(root, instr$suffix_id, sep="_")   
        id <- gsub(" ","", id) 
        instr$primary_id <- id
        class(instr) <- c('future_series','future','twsInstrument','instrument')
        assign(id, instr, pos=.instrument)   
        #cat('assinging', id)
        all.ids <- c(all.ids, id)
    }
    all.ids
}

