#' methods to get conId
#' 
#' currently defined methods for default, character, twsInstrument, and
#' twsContract
#' 
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

#' twsContract class conId extractor
#'
#' @method conId twsContract
#' @S3method conId twsContract
#' @author Garrett See
#' @keywords internal
conId.twsContract <- function(x, ...) 
{
    if (x[["conId"]] != 0)
        return(x[["conId"]])
    return(Contr_From_Instr(x)[["conId"]])
}

#' twsContractDetails class conId extractor
#'
#' @method conId twsContractDetails
#' @S3method conId twsContractDetails
#' @author Garrett See
#' @keywords internal
conId.twsContractDetails <- function(x, ...)
{
    return(x[["contract"]][["conId"]])
}

#' twsInstrument class conId extractor
#'
#' @method conId twsInstrument
#' @S3method conId twsInstrument
#' @author Garrett See
#' @keywords internal
conId.twsInstrument <- function(x, ...)
{
    if (x[["IB"]][["conId"]]) != 0)
        return(x[["IB"]][["conId"]])
    return(Contr_From_Instr(x)[["conId"]])
}

#' instrument class conId extractor
#'
#' @method conId instrument
#' @S3method conId instrument
#' @author Garrett See
#' @keywords internal
conId.instrument <- function(x, ...)
{
    if (!is.null(x[["identifiers"]][["conId"]])) {
        return(x[["identifiers"]][["conId"]])
    } else if (!is.null(x[["conId"]])) {
        return(x[["conId"]])
    } else NextMethod("conId")
}


#' default conId extractor
#'
#' @method conId default
#' @S3method conId default
#' @author Garrett See
#' @keywords internal
conId.default <- conId.character <- function(x, ...)
{
    return(sapply(x, function(xx) conId(getContract(xx, ...))))
}

