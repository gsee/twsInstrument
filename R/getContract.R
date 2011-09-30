#TODO: UseMethods -- twsInstrument, numeric/character, instrument
#TODO: make verbose/silent better
getContract <- function(x, verbose=TRUE, silent=FALSE, ...) {
    #if (is.xts(x)) x <- deparse(substitute(x))
    instr <- if (is.instrument(x)) {x} else try(getInstrument(x, silent=TRUE))
    if (is.twsInstrument(instr)) return(instr$IB)
    tmpnum <- try(suppressWarnings(as.numeric(x)), silent=TRUE)
    if (any(is.na(tmpnum))) return(Contr_From_Instr(x, assign_c=FALSE, verbose=verbose, silent=silent, ...))
    else {
        tryCatch({
            tryCatch(
            {
                if ( (hasArg(tws) && is.twsConnection(tws) && !isConnected(tws))
                    || !hasArg(tws)) {tws <- try(twsConnect(110),silent=TRUE)}
                if (inherits(tws,'try-error')) tws <- try(twsConnect(111), silent=TRUE)        
                if (inherits(tws,'try-error')) tws <- try(twsConnect(112), silent=TRUE)
                if (inherits(tws,'try-error')) tws <- twsConnect(150) #last attempt
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


