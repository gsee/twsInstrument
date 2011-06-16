
#symbol can be the name of an instrument (e.g. "XOM")
#OR
#it can be a twsContract
#OR
#it can be a twsInstrument
#OR
#it can be an instrument

getBAT <- reqTBBO <-
function(symbol, endDateTime, tws=NULL, barSize='1 min', 
	duration='5 D', useRTH="1", auto.assign=TRUE, env=.GlobalEnv) {
	#TODO: use dots. check for tws, symbol, contract, endDateTime, barSize, duration, clientId
	
	contract <- Contr_From_Instr(symbol,verbose=FALSE)
    
	#if endDateTime is provided, does it need to be converted to an IB time?    
    if (!missing(endDateTime)) {
        if(is.character(endDateTime)) {
            endDateTime <- paste(format(as.POSIXct(endDateTime),"%Y%m%d %H:%M:%S")) #format for IB
        }
    } else if (contract$include_expired == "1") {
        endDateTime <- paste(contract$expiry, "23:59:00")
    }
    if (missing(tws) || is.null(tws) || (is.twsConnection(tws) && !isConnected(tws)) ) 
        tws <- try(twsConnect())
    if (inherits(tws,'try-error')) tws <- try(twsConnect(2)) #try another clientId
    if (inherits(tws,'try-error')) tws <- twsConnect(50) #a last attempt for an available clientId
    if (isConnected(tws)) cat(paste('Connected with clientId ', tws$clientId, '.\n',sep=""))    
    
    fields <- c("BID","ASK","TRADES")
    BID <- ASK <- TRADES <- NULL
    tryCatch(
    {
        for (field in fields) {
            assign(field, reqHistoricalData(tws,contract,endDateTime=endDateTime,
                        barSize=barSize,duration=duration,useRTH=useRTH, whatToShow=field))
            if (!is.null(get(field))) {
                cat("Pausing 10 seconds between requests ...\n")
                Sys.sleep(10) #to avoid IB pacing violation.
            }
        }
        cat("Disconnecting ... \n")
    }, finally=twsDisconnect(tws) )

    if (!is.null(BID) && !is.null(ASK) && !is.null(TRADES)) {
	    bat <- merge(Cl(BID),Cl(ASK),all=FALSE)
        bat <- na.omit(bat)
        bat <- merge(bat,Cl(TRADES),all=FALSE)
        bat <- na.locf(bat,na.rm=TRUE)
	    bat$Mid.Price <- (bat[,1] + bat[,2])/2
        if (NCOL(bat)==4)
        	colnames(bat) <- paste(contract$symbol,c('Bid.Price','Ask.Price','Trade.Price','Mid.Price'),sep='.')        
	    if (auto.assign) { 
		    assign(symbol, bat, envir=env)
		    return(symbol)
	    } else { return(bat) }
    } else NULL
}


#################################################################
#Old Version
#symbol can either be the symbol of an instrument (e.g. "XOM")
#OR
#it can be a twsContract
#getBAT <-
#function(symbol, endDateTime, tws=NULL, barSize='1 min', 
#duration='5 D',auto.assign=TRUE, env=.GlobalEnv) {
##TODO: use dots. check for tws, symbol, contract, endDateTime, barSize, duration, clientId
##TODO: if endDateTime is character string, convert to IB format time    
#    if (is.twsContract(symbol)){ 
#        contract <- symbol
#        symbol <- symbol$symbol    
#    } else {
#        if (length(symbol) > 1) 
#            stop('symbol must be either the name of an instrument, or a twsContract object.')
#        instr <- try(getInstrument(symbol))

#        if (inherits(instr,'try-error') || !is.instrument(instr)) {
#            warning(paste("No instrument defined for ", symbol, 
#                ". Tried using twsSTK to create contract.1",  sep=""))
#            contract <- twsSTK(symbol,'SMART')
#        } else if (is.null(instr$IB) ) {
#            warning(paste('Cannot find twsContract definition in IB slot of, ',
#                         symbol, '. Tried using twsSTK to create contract.2',sep=""))
#            contract <- twsSTK(symbol, 'SMART')
#        } else contract <- instr$IB
#        #done getting twsContract object
#    }
#    #if endDateTime is provided, does it need to be converted to an IB time?    
#    if (!missing(endDateTime)) {
#        if(is.character(endDateTime)) {
#            endDateTime <- paste(format(as.POSIXct(endDateTime),"%Y%m%d %H:%M:%S")) #format for IB
#        }
#    }
#    if (is.null(tws) || !isConnected(tws)) tws <- twsConnect()
#
#	bid <- reqHistoricalData(tws,contract,endDateTime=endDateTime,
#                barSize=barSize,duration=duration,whatToShow='BID')
#	cat("Pausing 10 seconds between requests ...")
#	Sys.sleep(10) #to avoid IB pacing violation.
#	cat(" Request sent ... ")
#	ask <- reqHistoricalData(tws,contract,endDateTime=endDateTime,
#               barSize=barSize,duration=duration,whatToShow='ASK')
#	cat("Pausing 10 seconds between requests ...")
#	Sys.sleep(10) #to avoid IB pacing violation.
#	cat(" Request sent ... ")
#	trd <- reqHistoricalData(tws,contract,endDateTime=endDateTime,
#                barSize=barSize,duration=duration,whatToShow='TRADES')	
#    twsDisconnect(tws)
#
#	bat <- merge(Cl(bid),Cl(ask),all=FALSE)
#    bat <- na.omit(bat)
#    bat <- merge(bat,Cl(trd),all=FALSE)
#    bat <- na.locf(bat,na.rm=TRUE)
#	bat$Mid.Price <- (bat[,1] + bat[,2])/2
#    if (NCOL(bat)==4)
#    	colnames(bat) <- paste(symbol,c('Bid.Price','Ask.Price','Trade.Price','Mid.Price'),sep='.')
#	if (auto.assign) { 
#		assign(symbol, bat, envir=env)
#		return(symbol)
#	} else { return(bat) }
#}


##########################################################################################################
#Older version
#getBAT <-
#function(tws, symbol, endDateTime, barSize='1 min', duration='5 D',auto.assign=TRUE, env=.GlobalEnv) {
##TODO: if endDateTime is character string, convert to IB format time
#	instr <- getInstrument(symbol)
#	bid <- reqHistoricalData(tws,getInstrument(symbol)$IB,endDateTime=endDateTime,barSize=barSize,duration=duration,whatToShow='BID')
#	cat("Pausing 10 seconds between requests ...")
#	Sys.sleep(10) #to avoid IB pacing violation.
#	cat(" Request sent ... ")
#	ask <- reqHistoricalData(tws,getInstrument(symbol)$IB,endDateTime=endDateTime,barSize=barSize,duration=duration,whatToShow='ASK')
#	cat("Pausing 10 seconds between requests ...")
#	Sys.sleep(10) #to avoid IB pacing violation.
#	cat(" Request sent ... ")
#	trd <- reqHistoricalData(tws,getInstrument(symbol)$IB,endDateTime=endDateTime,barSize=barSize,duration=duration,whatToShow='TRADES')	
#	Sys.sleep(10) #to avoid IB pacing violation.
#	cat(" Request sent ... ")
	
#	fut.bat <- cbind(Cl(bid),Cl(ask),Cl(trd))
#	fut.bat <- na.locf(fut.bat,na.rm=TRUE)
#	fut.bat$Mid.Price <- (fut.bat[,1] + fut.bat[,2])/2
#	colnames(fut.bat) <- paste(symbol,c('Bid','Ask','Trade.Price','Mid.Price'),sep='.')
#	if (auto.assign) { 
#		assign(symbol, fut.bat, envir=env)
#		return(symbol)
#	} else { return(fut.bat) }
#}

