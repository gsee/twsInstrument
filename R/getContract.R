#TODO: UseMethods -- twsInstrument, numeric/character, instrument
getContract <- function(x, ...) {
    #verbose <- if (hasArg(silent)) {!silent} else TRUE    
    #if (is.xts(x)) x <- deparse(substitute(x))
    instr <- if (is.instrument(x)) {x} else try(getInstrument(x, silent=TRUE, ...))
    if (is.twsInstrument(instr)) return(instr$IB)
    tmpnum <- try(suppressWarnings(as.numeric(x)), silent=TRUE)
    if (any(is.na(tmpnum))) return(Contr_From_Instr(x, ...))
    else {
        tryCatch({
            tryCatch(
            {
                if ( (hasArg(tws) && is.twsConnection(tws) && !isConnected(tws))
                    || !hasArg(tws)) {tws <- try(twsConnect(2301),silent=TRUE)}
                if (inherits(tws,'try-error')) tws <- try(twsConnect(2302), silent=TRUE)        
                if (inherits(tws,'try-error')) tws <- try(twsConnect(2303), silent=TRUE)
                if (inherits(tws,'try-error')) tws <- twsConnect(50) #last attempt
            }, finally={
                if (isConnected(tws)) {                
                    #if (verbose) 
                    cat(paste('Connected with clientId ', tws$clientId, '.\n',sep=""))    
                    if (tws$clientId == 50) warning("IB Trader Workstation should be restarted.")                    
                    contract <- twsContract()
                    contract$conId <- paste(x)            
                    #request that IB fill in missing info.
                    details <- try(reqContractDetails(tws,contract),silent=TRUE)[[1]]                
                } else cat('Could not connect to tws.')
            }) #end nested tryCatch  
        },finally=twsDisconnect(tws)) #End outer tryCatch 
        return(details$contract)
    }
}


