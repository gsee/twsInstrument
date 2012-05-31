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
updateContract <- function(x, ...) {
    stopifnot(is.character(x))
    tws <- ConnectIB(c(100:104, 150)) 
    cdet <- suppressWarnings(reqContractDetails(tws, getContract(x)))
    twsDisconnect(tws)
    if (length(cdet) > 0) cdet <- cdet[[1L]]
    if (!identical(cdet$contract, getContract(x))) {
        suppressWarnings(instrument_attr(x, "IB", NULL))
        update_instruments.IB(x, ...)   
    }
    x
}

