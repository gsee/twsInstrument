#' methods to get conId
#' 
#' currently defined methods for default, character, twsInstrument, and
#' twsContract
#' 
#' 
#' @aliases conId conId.default conId.character conId.twsInstrument
#' conId.twsContract
#' @param x The name of a twsInstrument, or an object of type twsInstrument, or
#' twsContract.
#' @param ... other arguments
#' @return character conId
#' @note unless you call \code{conId} with a \code{twsContract} or
#' \code{twsInstrument}, it will call getContract which will, as a last resort,
#' download the contract details from IBrokers.
#' @author gsee
#' @seealso \code{\link{twsInstrument}}, \code{\link{twsContract}}
#' @examples
#' 
#' \dontrun{
#' stk <- twsInstrument("GOOG")
#' conId(stk)
#' conId(getInstrument(stk))
#' conId(getContract(stk))
#' 
#' conId(stock("AAPL","USD")) #uses reqContractDetails
#' }
#' @export
#' @rdname conId
conId <- function(x, ...)
{
    UseMethod('conId', x)
}

#' @export
#' @rdname conId
conId.twsContract <- function(x, ...) 
{
    return(x[["conId"]])
}

#' @export
#' @rdname conId
conId.twsInstrument <- function(x, ...)
{
    return(x[["IB"]][["conId"]])
}

#' @export
#' @rdname conId
conId.default <- conId.character <- function(x, ...)
{
    return(sapply(x, function(xx) conId(getContract(xx, ...))))
}

