#' Download and save to disk historical Bid, Ask, and Trades data
#' 
#' This will make several requests for historical data from IBrokers
#' repecting the historical data request limitations.  
#' 
#' Experimental code -- see Note section
#'
#' Important: You should have a base directory that contains these sub-directories: \sQuote{BID}, \sQuote{ASK}, 
#' \sQuote{TRADES}, and \sQuote{BAT}.  If you will be requesting data for foreign exchange rates, you should also
#' have a sub-directory called \sQuote{BAM}.
#'
#' The default \code{base_dir} is \dQuote{/mnt/W}.  If you do not have a directory there, you should either create
#' it, or not use the default value for \code{base_dir}.
#'
#' \code{reqTBBOhistory} will make requests for 5 days of bid, ask, and trade data and then pause for 30 seconds.
#' In between requests, all BID, ASK, and TRADES data are saved into the respective subdirectories. Data are saved in rda files named 
#' with the format YYYY.MM.DD.Symbol.rda.  After all requests have been made (approx. 37 minutes), 
#' the data are re-read and merged together into BATMV format and (if \code{save=TRUE}) stored in the BAT directory 
#' (or BAM directory for forex instruments, see below).
#'
#' IBrokers does not disseminate TRADES data for FX (CASH).  Therefore, \code{reqTBBOhistory} will make 2 requests 
#' every 20 seconds (BID and ASK data) instead of 3 requests every 30 seconds.
#'
#' For best results, you should define your instruments before calling this function.
#'
#' This has only been tested on a debian-based linux system.
#'
#' IB limits historical data requests to 6 every 60 seconds
#'
#' @param Symbols names of instruments for which to request data
#' @param base_dir base_dir that contains sub-directories \sQuote{BID}, \sQuote{ASK}, \sQuote{TRADES}, \sQuote{BAT}, and/or \sQuote{BAM}
#' @param ndays total number of days to retrieve. Default is 95. Max is 365
#' @param endDateTime \sQuote{YYYYMMDD H:M:S}
#' @param tws twsConnection object (Not yet implemented)
#' @param barSize barSize to retrieve. see \code{\link[IBrokers]{reqHistoricalData}}
#' @param useRTH Use regular trading hours? 1 for TRUE, 0 for FALSE
#' @param env where to put the final BATVM or BAM xts object
#' @param save logical. Save to disk the BATVM or BAM object?
#' @param chronological should the requests be made in chronological order.  
#' Default is FALSE because, at least for futures and options, requests for old data are more 
#' likely to fail than requests for recent data.  For stocks, it might make sense
#' to use TRUE so that if a request is interrupted, you will not have gaps in your data.
#' \code{update.data} uses \code{chronological=TRUE}
#' @return called for side-effect. Returns the names of Symbols.
#' @author Garrett See
#' @note Warning: Interactive Brokers *back adjusts* their data for stock splits (but not for dividends).  
#' If you are storing stock data, you should probably unadjust the last year's worth of data, then, 
#' make sure you download the most recent data each day, being careful not to overwrite any data already
#' stored on disk.  This way you could have all unadjusted data that you could adjust (with 
#' qmao:::adjustIntraday.yahoo, for example).  Otherwise, you'll have some data that is 
#' split-adjusted (or partially split-adjusted) and some that isn't.
#' @seealso \code{twsInstrument:::update.data} (unexported due to possibility of name change, and current lack of documentation), 
#' 
#' \code{\link{getBAT}}, \code{\link{makeBATs}}, \code{\link[IBrokers]{reqHistoricalData}}, \code{\link[IBrokers]{reqHistory}}
#' @references InteractiveBrokers \url{www.interactivebrokers.com}
#' 
#' IB API \url{http://interactivebrokers.com/php/apiUsersGuide/apiguide.htm}
#' 
#' IB Historic Data Limitations
#' \url{http://individuals.interactivebrokers.com/php/apiUsersGuide/apiguide/api/historical_data_limitations.htm#XREF_93621_Historical_Data}
#'
#' @examples
#' \dontrun{
#' library(twsInstrument)
#' dir.create("tmpdata/BID", recursive=TRUE)
#' dir.create("tmpdata/ASK")
#' dir.create("tmpdata/TRADES")
#' dir.create("tmpdata/BAT")
#' dir.create("tmpdata/BAM")
#' define_stocks("SPY")
#' # get the last 20 days of minutely data 
#' # (this will take ~2 minutes)
#' reqTBBOhistory("SPY", 
#'                 base_dir="tmpdata", 
#'                 ndays=20, 
#'                 save=TRUE, 
#'                 chronological=TRUE)
#' # Now to prove it's on disk, delete from workspace
#' # and load it.
#' rm("SPY")
#' getSymbols("SPY", src='FI', dir='tmpdata/BAT', 
#'             from=Sys.Date() - 20, verbose=TRUE)
#' 
#' # If you had called reqTBBOhistory with save=FALSE (the default)
#' # or if your "BAT" directory were wiped out, you can use
#' # the unexported makeBATs function to get the BID, ASK, TRADES 
#' # data from disk and rebuild the BAT data.
#' unlink("tmpdata/BAT")
#' twsInstrument:::makeBATs("SPY", base_dir='tmpdata', ndays=20)
#' rm("SPY")
#' getSymbols("SPY", src='FI', dir='tmpdata/BAT', 
#'             from=Sys.Date() - 20, verbose=TRUE)
#' 
#' # Clean up -- Delete everything
#' unlink("tmpdata/BID")
#' unlink("tmpdata/ASK")
#' unlink("tmpdata/TRADES")
#' unlink("tmpdata/BAT")
#' unlink("tmpdata/BAM", recursive=TRUE)
#' }
#' @export
reqTBBOhistory <-
function(Symbols, base_dir='/mnt/W', ndays=95, 
        endDateTime, tws, barSize='1 min', useRTH='1', 
        env=.GlobalEnv,save=FALSE, chronological=FALSE) {
    if (missing(endDateTime) || is.null(endDateTime)) {
        autoDate <- TRUE
    } else {
        autoDate <- FALSE
        if (is.character(endDateTime)) {
            endDate <- as.Date(endDateTime, format="%Y%m%d")
        }
    }
    symout <- NULL
    for (Symbol in Symbols) {	        
        #contract <- Contr_From_Instr(Symbol, verbose = FALSE)
        #instr <- Instr_From_Contr(Symbol, verbose = FALSE)
        instr <- twsInstrument(Symbol,output="instrument",assign_i=FALSE,verbose=FALSE)
        contract <- instr$IB
        symout <- c(symout, instr$primary_id)
        if (autoDate) {
            if (contract$expiry == "" || is.null(contract$expiry)) {
                endDate <- Sys.Date()
            } else if (as.Date(contract$expiry, format="%Y%m%d") < Sys.Date()) {
                endDate <- as.Date(contract$expiry, format="%Y%m%d")
            } else endDate <- Sys.Date()
        } 
        if (contract$expiry != "") 
            endDate <- min(as.Date(contract$expiry, format="%Y%m%d"), endDate) 
        Ndays <- max(0,ndays[1] - 5 )
        if (!is.null(instr$first_traded)) {
            tmpfirst <- instr$first_traded        
            if (is.character(tmpfirst)) tmpfirst <- as.Date(tmpfirst, format="%Y-%m-%d") 
            Ndays <- min(Ndays,as.numeric(endDate-tmpfirst))
        }        
        if (barSize == "1 min") {
            if (chronological) {
                endDateTime <- endDate - seq(Ndays, 0, -5)
            } else endDateTime <- endDate - seq(0, Ndays, 5)
            duration <- "5 D"
        }
        else if (barSize == "15 mins") {
            if (chronological) {
                endDateTime <- endDate - seq(Ndays, 0, -10)
            } else endDateTime <- endDate - seq(0, Ndays, 10)
            duration <- "10 D"
        }

        if (missing(tws) || is.null(tws) || 
		        (is.twsConnection(tws) && !isConnected(tws))) 
            tws <- try(ConnectIB(c(120:122, 150)))
            if (isConnected(tws)) {
                cat(paste("Connected with clientId ", tws$clientId, 
                            ".\n Requesting ", Symbol, "\n", sep = ""))
    	        if (tws$clientId == 150) warning("IB TWS should be restarted.")
            } else stop("Could not connect to IB.") 

        tryCatch( {        
	        if (substr(base_dir,nchar(base_dir),nchar(base_dir)) != "/") base_dir <- paste(base_dir, "/",sep="")

	        bidenv <- new.env()
            askenv <- new.env()
            trdenv <- new.env()
        
            if (contract$exch == "IDEALPRO") 
                hastrades <- FALSE            
            else hastrades <- TRUE

            if (contract$sectype == "IND")
                hasbidask <- FALSE
            else hasbidask <- TRUE

            domakeBATs <- TRUE
            slptime <- 0.1     
            for (i in 1:length(endDateTime)) {
                st <- strptime(paste(endDateTime[i],"23:59:59"),"%Y-%m-%d %H:%M:%S")
                ceDT <- paste(format(st,"%Y%m%d %H:%M:%S"))
                cat(paste("Request ",i, " of ", length(endDateTime), ": request end Date: ", format(st,"%Y-%m-%d"),"\n",sep="")) #debug
                if (hasbidask) {                
                    assign(Symbol, reqHistoricalData(tws, contract, barSize=barSize, 
                                                    duration=duration, endDateTime=ceDT, 
                                                    useRTH=useRTH,whatToShow='BID'),pos=bidenv)
                    assign(Symbol, reqHistoricalData(tws, contract, barSize=barSize, 
                                                    duration=duration, endDateTime=ceDT, 
                                                    useRTH=useRTH,whatToShow='ASK'),pos=askenv)
                    slptime <- slptime + 20
                }
                if (hastrades) {
                    assign(Symbol, reqHistoricalData(tws, contract, barSize=barSize, 
                                                        duration=duration, endDateTime=ceDT, 
                                                        useRTH=useRTH,whatToShow='TRADES'),pos=trdenv)
                    
                    slptime <- slptime + 10
                } 
                if (slptime < 20) domakeBATs <- FALSE #if there is only Trade data (IND/synthetic)
                if (!is.null(bidenv[[Symbol]])) saveSymbols.days(Symbols=Symbol,base_dir=paste(base_dir,'BID/',sep=""), env=bidenv)
                if (!is.null(askenv[[Symbol]])) saveSymbols.days(Symbols=Symbol,base_dir=paste(base_dir,'ASK/',sep=""), env=askenv)
                if (!is.null(trdenv[[Symbol]])) saveSymbols.days(Symbols=Symbol,base_dir=paste(base_dir,'TRADES/',sep=""), env=trdenv)
                #uncomment if you don't want to sleep after last request. 
                #(if you don't sleep, then repeated calls to this function will result in pacing violation)                
                #if ((ndays > 5 && i < length(endDateTime)) || (Symbol != Symbols[length(Symbols)])) {
                    cat("Pausing for", round(slptime,0), "seconds to avoid pacing violation...\n")
                    Sys.sleep(slptime)
                    slptime <- 0.1
                #}
            }
        },finally={twsDisconnect(tws)} )
    }
#	tmp <- reqHistoricalData(tws, contract, barSize = barSize, duration = duration, 
#        endDateTime = endDateTime, useRTH=useRTH)
#	assign(Symbol, tmp, pos=tmpenv)
#	saveSymbols.days(Symbol=Symbol, base_dir=base_dir,env=tmpenv)
    if (domakeBATs)	makeBATs(symout,base_dir,env=env,ndays=ndays)
    if (save) saveSymbols.days(symout,base_dir=paste(base_dir,"BAT",sep=""),env=env)
    symout
}

