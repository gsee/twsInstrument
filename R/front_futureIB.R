#' @export
#' @rdname front_future.IB
front_future <- function(roots, currency='USD', underlying_id = NULL, addIBslot=TRUE, src='IB') {
    do.call(paste('front_future',src,sep="."),list(roots=roots,currency=currency, underlying_id=underlying_id, addIBslot=addIBslot))
}



#' instrument class constructor for front futures contract
#' 
#' Create a future instrument for the front-month contract as well as specs for
#' the root. Looks up contract details of a futures contract from IB and
#' creates a futures instrument. The contract root will also be defined if it
#' hasn't been defined yet.
#' 
#' The contract details that IB gives are usually for the front month contract,
#' but there is no guarantee that the contract will actually be the front
#' contract.  (perhaps default_future would be a better name for this
#' function?)
#' 
#' This is not a very robust function.  \code{define_futures} is probably a
#' better alternative, but requires more user input.
#' 
#' If you find that \code{front_future.IB} is defining something different than
#' what you wanted (e.g. futures on Colgate Palmolive instead of Light Sweet
#' Crude) try providing the \code{underlying_id}
#' 
#' Currently, \code{front_future} is just a wrapper for \code{front_future.IB}.
#' I.e. \dQuote{IB} is the only functional value for \code{src}
#' 
#' using \code{root} and \code{currency} it will get the contract details of
#' the front month future with a call to reqContractDetails. Using that info,
#' it will create a future_series instrument, and a future instrument if it
#' doesn't exist yet.
#' 
#' @aliases front_future front_future.IB
#' @param roots vector of chr strings; contract roots.
#' @param currency name of currency; if more than one root, they all must have
#' the same currency
#' @param underlying_id name of underlying
#' @param addIBslot should an IB slot be added to the instrument; i.e. do you
#' want to create a twsInstrument.
#' @param src currently only dQuoteIB is supported
#' @return called for side-effect. Returns the name of any instruments that
#' were created.
#' @author Garrett See
#' @seealso define_futures, future, future_series, update_instruments.IB,
#' Instr_From_Contr, option_series.yahoo
#' @examples
#' 
#' \dontrun{
#' front_future('YM')
#' front_future('ES')
#' front_future('SPY')
#' 
#' #alternatively
#' #front_future(c("YM","ES","SPY"))
#' #or, 
#' #unlist(lapply(c("YM","ES","SPY"), front_future))
#' }
#' @export
#' @rdname front_future.IB
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

