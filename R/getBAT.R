#symbol can be the name of an instrument (e.g. "XOM")
#OR
#it can be a twsContract
#OR
#it can be a twsInstrument
#OR
#it can be an instrument



#' download Bid Ask Trade data and merge into BAT object
#' 
#' getBAT downloads and merges data for Bid, Ask, and Trade. reqTBBO is an
#' alias for getBAT. %getBAThistory downloads the max allowed by
#' InteractiveBrokers.
#' 
#' %for getBAThistory, the symbol argument must be the name %of a
#' FinancialInstrument instrument.
#' 
#' reqTBBO is an alias for getBAT.
#' 
#' symbol can be one of: an instrument, the name of an instrument, the name of
#' an undefined stock, or a twsContract object.
#' 
#' Before making a request for historical data, symbol will be passed through
#' \code{Contr_From_Instr} (which is a wrapper for \code{buildIBcontract})
#' which will convert symbol into an updated, complete twsContract.
#' 
#' If you want to get data for something other than a stock, then \code{symbol}
#' should be an instrument object, a twsContract object or the name of a
#' previosly defined instrument
#' 
#' Unlike reqHistoricalData, The \code{endDateTime} argument must be of the
#' form 'CCYY-MM-DD HH:MM:SS'
#' 
#' If the contract is expired, the minimum (i.e. oldest) of \code{endDateTime},
#' if given, and 1 minute before midnight on the expiration date, will be used
#' for endDateTime in the call to reqHistoricalData.
#' 
#' !!! Ignore the rest of this details section until I upload getBAThistory
#' function !!! (Below is mostly copied from reqHistoricalData help) If
#' endDateTime is not specified the current time as returned from the TWS
#' server will be used. This is the preferred method for backfilling data. The
#' \sQuote{TZ} portion of the string is optional.
#' 
#' Legal \code{barSize} settings are technically \sQuote{1 secs},\sQuote{5
#' secs},\sQuote{15 secs}, \sQuote{30 mins},\sQuote{1 min},\sQuote{2 mins},
#' \sQuote{3 mins},\sQuote{5 mins},\sQuote{15 mins}, \sQuote{30 mins},\sQuote{1
#' hour},\sQuote{1 day}, \sQuote{1 week},\sQuote{1 month},\sQuote{3 months},
#' and \sQuote{1 year}. They must be specified exactly and there is no
#' guarantee from the API that all will work for all securities or durations.
#' 
#' The duration string must be of the form \sQuote{n S} where the last
#' character may be any one of \sQuote{S} (seconds), \sQuote{D} (days),
#' \sQuote{W} (weeks), \sQuote{M} (months), and \sQuote{Y} (year). At present
#' the limit for years is 1.
#' 
#' %getBAThistory needs to be updated; %getBAThistory requests the maximal
#' history from IB.
#' 
#' @aliases getBAT reqTBBO
#' @param Symbols can be a twsInstrument, a twsContract, an instrumnet or the
#' name of an instrument. (you can give it a character vector to get multiple
#' Symbols)
#' @param endDateTime end date/time for request. See details.
#' @param tws a twsConnection object. optional.
#' @param barSize bar size to retrieve. default='1 min'
#' @param duration time span the request will cover. default='5 D'
#' @param useRTH only include regular trading hours?
#' @param auto.assign if TRUE, data will be assigned to symbols in the env or
#' .GlobalEnv if no env given.
#' @param env environment in which to save results
#' @return Returns (or assigns) an xts object containing Bid.Price, Ask.Price,
#' Trade.Price, Mid.Price, along with additional information stored in the
#' objects xtsAttributes,
#' @author Garrett See
#' @seealso Contr_From_Instr, IBrokers, reqHistoricalData, reqHistory,
#' getIBequities
#' @references InteractiveBrokers \url{www.interactivebrokers.com}
#' 
#' IB API \url{http://interactivebrokers.com/php/apiUsersGuide/apiguide.htm}
#' 
#' IB Historic Data Limitations
#' \url{http://individuals.interactivebrokers.com/php/apiUsersGuide/apiguide/api/historical_data_limitations.htm#XREF_93621_Historical_Data}
#' @examples
#' 
#' \dontrun{
#' #If no instrument is defined for symbol, 
#' #it will assume you are trying to get a stock
#' getBAT("XOM")
#' 
#' #Alternatively, pass a twsContract
#' contract <- twsSTK("XOM","SMART","NYSE")
#' getBAT(contract)
#' 
#' #or, if you use FinancialInstrument, add an IB slot to hold
#' #the twsContract object
#' currency('USD')
#' stock('XOM', 'USD', 1, IB=twsSTK('XOM'))
#' getBAT('XOM') #gets from .instrument
#' }
#' @export 
getBAT <- reqTBBO <-
function(Symbols, endDateTime, tws=NULL, 
        barSize='1 min', duration='5 D', 
        useRTH="1", auto.assign=TRUE, 
        env=.GlobalEnv) {
    #TODO: use dots. check for tws, symbol, contract, endDateTime, barSize, duration, clientId
    if (length(Symbols) > 1 && !auto.assign) stop('auto.assign must be TRUE if using multiple Symbols')
    symout <- NULL	
    for (symbol in Symbols) {
	    contract <- Contr_From_Instr(symbol,verbose=FALSE)
        
        if (missing(endDateTime)) endDateTime <- Sys.time()
        if (!is.null(contract$expiry) && contract$expiry != "") {
            endDate <- min(as.Date(contract$expiry,format="%Y%m%d"), Sys.Date())
            endDateTime <- as.POSIXct(paste(endDate, "23:59:59"))    
        }
        endDateTime <- paste(format(as.POSIXct(endDateTime),"%Y%m%d %H:%M:%S")) #format for IB
        
        if (missing(tws) || is.null(tws) || (is.twsConnection(tws) && !isConnected(tws)) ) 
            tws <- try(twsConnect(120))
        if (inherits(tws,'try-error')) tws <- try(twsConnect(121)) #try another clientId
        if (inherits(tws,'try-error')) tws <- twsConnect(150) #a last attempt for an available clientId
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

        if (!is.null(BID) && !is.null(ASK)) { # && !is.null(TRADES)) {
	        bat <- merge(Cl(BID),Cl(ASK),all=FALSE)
            bat <- na.omit(bat)
            if (!is.null(TRADES)) {        
                bat <- merge(bat,Cl(TRADES),all=FALSE)
                bat <- na.locf(bat,na.rm=TRUE)	    
            }
            bat$Mid.Price <- (bat[,1] + bat[,2])/2
            bat <- na.omit(bat)
            if (!is.null(TRADES)) {
                bat <- merge(bat, Vo(TRADES))
            	colnames(bat) <- paste(contract$symbol,c('Bid.Price','Ask.Price','Trade.Price','Mid.Price','Volume'),sep='.')
            } else colnames(bat) <- paste(contract$symbol,c('Bid.Price','Ask.Price','Mid.Price'),sep='.')
                    
	        if (auto.assign) { 
                assign(symbol, bat, envir=env)
	    	    symout <- c(symout, symbol)
	        } else { return(bat) }
        } else NULL
    }
    symout
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

