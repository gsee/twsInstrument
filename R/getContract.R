#TODO: UseMethods -- twsInstrument, numeric/character, instrument, xts
#TODO: make verbose/silent better


#' get a twsContract object
#' 
#' get a twsContract object
#' 
#' If \code{x} is a twsInstrument, or if there is a twsInstrument with the
#' same name as \code{x} in the .instrument environment, it will return the
#' twsContract found in the $IB field of that object.  If not, then if 
#' \code{x} is the name of a Symbol defined in the Symbol Lookup table 
#' (see \code{\link[quantmod]{setSymbolLookup}}) that has an "IBrokers" src,
#' and a twsContract object stored under "Contract", that twsContract will be returned.
#' Otherwise, \code{x} will be passed to \code{\link{Contr_From_Instr}} 
#' which will use what information it can to create a twsContract shell that can/will be
#' updated with \code{\link{reqContractDetails}}
#' 
#' If you want to force a request for contract details from IB (and ensure you are not 
#' getting a local copy of the twsContract), use \code{\link{Contr_From_Instr}}
#' which will return the twsContract that \code{\link[IBrokers]{reqContractDetails}} returns
#' without altering any local copies.  Alternatively, if you call \code{\link{rmContract}}
#' before calling \code{getContract}, it will be forced to look it up 
#' (by calling \code{\link{Contr_From_Instr}} for you).
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
#' @seealso \code{\link{conId}}, \code{\link{rmContract}}, 
#' \code{\link{define_stocks}},
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
#' getContract(instr$conId) # will connect to IB and reqContractDetails
#' }
#' @export
getContract <- function(x, verbose=TRUE, silent=FALSE, ...) {
    #if (is.xts(x)) x <- deparse(substitute(x))
    if (inherits(x, 'twsContractDetails')) return(x$contract)
    if (inherits(x, 'eventPortfolioValue')) return(x$contract)
    instr <- if (is.instrument(x)) {x} else try(getInstrument(x, silent=TRUE))
    if (is.twsInstrument(instr)) return(instr$IB)
    if (is.character(x)) { #IBrokers method of getSymbols implies there may be a 'twsContract' stored in Symbol Lookup table.
        SL <- getSymbolLookup()[[x]]
        if (!is.null(SL$Contract) && SL$src == 'IBrokers')
            if (is.twsContract(SL$Contract)) return(SL$Contract)   
    }
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
#' Remove local copies of twsContracts
#'
#' Remove any local copies of a twsContract so that
#' \code{getContract} will be forced to \code{\link[IBrokers]{reqContractDetails}}
#' @param x character name of a Symbol. Can also be an xts object. (See Details)
#' @param env environment that holds (non-required) xts data.
#' @return called for side-effect
#' @author Garrett See
#' @seealso \code{\link{getContract}}, \code{\link[quantmod]{setSymbolLookup}}, 
#' \code{\link{twsInstrument}}, \code{\link{instrument_attr}}
#' @examples
#' \dontrun{
#' define_stocks("SPY")
#' getContract("SPY") # == getInstrument("SPY")$IB
#' rmContract("SPY")
#' instrument_attr("SPY", "src", list(src="IBrokers", Contract=getContract("SPY")))
#' getContract("SPY") # == getSymbolLookup()[["SPY"]]$Contract
#' rmContract("SPY")
#' getContract("SPY") #has to look it up with reqContractDetails
#' }
#' @export
rmContract <- function(x, env=.GlobalEnv) {
    if (!is.character(x)) x <- deparse(substitute(x))
    if (is.xts(get(x, pos=env))){
        xxts <- get(x, pos=env)
        attr(xxts, 'Contract') <- NULL
        attr(xxts, 'twsContract') <- NULL        
    }    
    suppressWarnings(try(instrument_attr(x, "IB", NULL)))
    SL <- getSymbolLookup()[[x]]
    if (!is.null(SL) && SL$src == 'IBrokers') {
        all.symbols <- getOption("getSymbols.sources")
        all.symbols[[x]] <- NULL
        options(getSymbols.sources = all.symbols)
    }
}


