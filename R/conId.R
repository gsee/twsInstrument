

conId <- function(x, ...)
{
    UseMethod('conId', x)
}

conId.twsContract <- function(x, ...) 
{
    return(x[["conId"]])
}

conId.twsInstrument <- function(x, ...)
{
    return(x[["IB"]][["conId"]])
}

conId.default <- conId.character <- function(x, ...)
{
    return(sapply(x, function(xx) conId(getContract(xx, ...))))
}

