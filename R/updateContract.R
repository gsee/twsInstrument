#' update the IB attribute of a \code{\link{twsInstrument}}
#'
#' If a Contract is no longer valid, this will request the new contract
#'
#' Interactive Brokers occationally changes the conId that is associated with
#' a given instrument (for example, after a split, they change the conId).
#' This function will check to make sure that the Contract that 
#' \code{\link{getContract}} finds is still valid.  If not, it will request
#' the new Contract and store it in the IB attribute of the instrument.
#'
#' This is a very simple function
#' @param x character primary_id of an instrument
#' @param add.identifier logical. If the conId has changed, should a
#'   \dQuote{old.conId} identifier be added to the instrument?
#' @param type type of instrument being updated. Passed to 
#'   \code{\link[FinancialInstrument]{getInstrument}}.  Only used if more than
#'   two or more instruments have \code{primary_id}s that are the same except
#'   for leading dots.
#' @param ... arguments to pass to \code{\link{update_instruments.IB}}.
#' @return character name of instrument
#' @author Garrett See
#' @examples
#' \dontrun{
#' ibak <- as.list(FinancialInstrument:::.instrument) #backup instrument envir
#' rm_instruments()
#' define_stocks("SPY")
#' contr <- getContract("SPY")
#' contr$conId <- "0000000"
#' instrument_attr("SPY", "IB", contr)
#' getContract("SPY") #bad conId
#' updateContract("SPY")
#' getInstrument("SPY") #updated
#' ## Restore previous instrument environment
#' rm_instruments(keep=FALSE)
#' loadInstruments(ibak)
#' }
#' @export
updateContract <- function(x, add.identifier=TRUE, type='instrument', ...) {
    stopifnot(is.character(x))
    instr <- getInstrument(x, type)
    if (!is.instrument(instr)) {
        stop(paste("Please define ", x, " first", sep=""))
    }
    oc <- instr[["IB"]]
    if (length(oc) == 0L) {
        oc <- try(getContract(x))
        if (inherits(oc, 'try-error')) {
            stop(paste("Cannot getContract for", x))
        }
    }
    tws <- ConnectIB(c(100:104, 150L))
    cdet <- suppressWarnings(try(reqContractDetails(tws, oc)))
    twsDisconnect(tws)
    if (!inherits(cdet, "try-error") && length(cdet) > 0L) cdet <- cdet[[1L]]
    if (!identical(cdet$contract, oc)) {
        suppressWarnings(instrument_attr(x, "IB", NULL))
        update_instruments.IB(x, ...)
        if (isTRUE(add.identifier)) {
            if(!identical(getContract(x)$conId, oc$conId)) {
               add.identifier(x, old.conId=oc$conId)
           }
        }   
    } else instrument_attr(x, "IB", oc)
    x
}

