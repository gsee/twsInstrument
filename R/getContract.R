#TODO: UseMethods -- twsInstrument, numeric/character, instrument
#TODO: make verbose/silent better


#' get a twsContract object
#' 
#' get a twsContract object
#' 
#' If \code{x} is a twsInstrument, or if it can find a twsInstrument with the
#' same name as \code{x} in the .instrument environment, it will return the
#' twsContract found in the $IB slot of that object.  If not, then if \code{x}
#' is numeric, or if it is a character string of numbers, it will treat that as
#' the conId in a newly created twsContract shell.  Then it will call
#' \code{\link{reqContractDetails}} to get the contract object.  Otherwise,
#' \code{x} will be passed to \code{\link{Contr_From_Instr}} which will use
#' what information it can to create a twsContract shell that can/will be
#' updated with \code{\link{reqContractDetails}}
#' 
#' @param x can be an instrument, twsInstrument, name of an instrument or
#' twsInstrument, or a numeric or character \sQuote{conId}
#' @param verbose be verbose?
#' @param silent silence warnings?
#' @param ...  any additional arguments to pass to
#' \code{\link{Contr_From_Instr}}
#' @return an object of class twsContract
#' @note To ensure you get the contract you're after, you should define your
#' instruments before using this function.
#' @author Garrett See
#' @seealso \code{\link{conId}}, \code{\link{define_stocks}},
#' \code{\link{define_options}}, \code{\link{define_futures}},
#' \code{\link{Contr_From_Instr}}, \code{\link{twsInstrument}},
#' \code{\link{getInstrument}}
#' @examples
#' 
#' \dontrun{
#' getContract("4082282") #uses reqContractDetails
#' getContract(4082282) #same
#' twsInstrument('SEE') 
#' getContract("SEE") # == .instrument$SEE$IB
#' getContract("DIA") # will use reqContractDetails if "DIA" is not already defined
#' instr <- getInstrument("SEE")
#' getContract(instr)
#' }
#' @export
getContract <- function(x, verbose=TRUE, silent=FALSE, ...) {
    #if (is.xts(x)) x <- deparse(substitute(x))
    if (inherits(x, 'twsContractDetails')) return(x$contract)
    instr <- if (is.instrument(x)) {x} else try(getInstrument(x, silent=TRUE))
    if (is.twsInstrument(instr)) return(instr$IB)
    tmpnum <- try(suppressWarnings(as.numeric(x)), silent=TRUE)
    if (any(is.na(tmpnum))) return(Contr_From_Instr(x, assign_c=FALSE, verbose=verbose, silent=silent, ...))
    else {
        tryCatch({
            tryCatch(
            {
                if ( (hasArg(tws) && is.twsConnection(tws) && !isConnected(tws))
                    || !hasArg(tws)) {tws <- try(ConnectIB(c(110:114, 150)), silent=TRUE)}
            }, finally={
                if (isConnected(tws)) {                
                    if (verbose) 
                        cat(paste('Connected with clientId ', tws$clientId, '.\n',sep=""))    
                    if (tws$clientId == 150) warning("IB Trader Workstation should be restarted.")                    
                    contract <- twsContract()
                    contract$conId <- paste(x)
                    contract$include_expired <- "1" #reqContractDetails will overwrite this (due to a bug it has), 
                                                    #but you need it here in case this contract is expired. 
                    #request that IB fill in missing info.
                    details <- try(suppressWarnings(reqContractDetails(tws,contract)),silent=TRUE)
                    if (!inherits(details,'try-error') && length(details)) {
                        details <- details[[1]]        
                    } else details <- NULL
                } else if (verbose) cat('Could not connect to tws.')
            }) #end nested tryCatch  
        },finally=twsDisconnect(tws)) #End outer tryCatch
        if (is.null(details)) {
            stop("Could not find twsContract with conId ", x)
        } else if (verbose) cat('Contract details request complete. Disconnected.\n')
        return(details$contract)
    }
}


