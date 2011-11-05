#' Build Bid Ask Trade object
#'
#' Reads from disk and merges closing Bid, Ask, Trade data
#' 
#' @param Symbols chr name of instrument
#' @param base_dir base_dir that holds TRADES, BID, and ASK directories 
#' (each of those directories should hold directories named for instruments.)
#' @param env environment to put BATs in
#' @param ndays optional. If given and numeric, \sQuote{from} will become \sQuote{to}-\code{ndays}
#' @return called for side-effect.  Objects that are stored will have Bid, Ask, Trade, Mid, and Volume columns unless
#' there are no TRADES data in which case they will have Bid, Ask, and Mid columns.
#' @author Garrett See
#' @note used by \code{\link{reqTBBOhistory}}
#' @seealso \code{\link{getBAT}}, \code{\link{reqTBBOhistory}}
#' @examples
#' \dontrun{
#' #reqTBBOhistory("RIMM", ndays=5)
#' makeBATs("RIMM")
#' }
makeBATs <-
function(Symbols, base_dir='/mnt/W',env=.GlobalEnv, ndays) {
    if (substr(base_dir,nchar(base_dir),nchar(base_dir)) != "/") base_dir <- paste(base_dir, "/",sep="")
	out <- NULL
	for (Symbol in Symbols) {    
		if (!Symbol %in% list.files(paste(base_dir,"BID",sep=""), all.files=TRUE)) {
			warning(paste("Cannot find directory ", Symbol, " in ",base_dir,'BID/. ..skipping ', Symbol, sep=""))
			next		
		}
        if (!is.instrument(try(getInstrument(Symbol,silent=TRUE),silent=TRUE))) Symbol = twsInstrument(Symbol,output='Symbol')        

        #if it has TRADES data, then use that to get 'from' and 'to'        
        if (!file.exists(paste(base_dir,"TRADES/",Symbol,sep=""))) {
            if (file.exists(paste(base_dir,"BID/",Symbol,sep="")) &&
               file.exists(paste(base_dir,"ASK/",Symbol,sep="")) ) {
                warning("No trade data for ", Symbol, ". Making BAMs instead of BATs.")
                hastrades <- FALSE                
                from <- max(get_from(Symbol,paste(base_dir,"BID",sep="")),
                            get_from(Symbol,paste(base_dir,"ASK",sep="")) )
                to <- min(get_to(Symbol,paste(base_dir,"BID",sep="")),
                            get_to(Symbol,paste(base_dir,"ASK",sep="")) )      
            } else {
                warning("No data for ", Symbol, "...skipping.")
                next
            }
        } else {
     		from <- get_from(Symbol,paste(base_dir,'TRADES',sep=""))
    		to <- get_to(Symbol,paste(base_dir,'TRADES',sep=""))
            hastrades <- TRUE
		}
        if (!missing(ndays) && is.numeric(ndays)) from <- paste(as.Date(to)-ndays)
		#setSymbolLookup.FI(base_dir=paste(base_dir,'BID',sep=""))
		bid <- getSymbols(Symbol,auto.assign=FALSE,from=from,to=to,
                    src='FI',dir=paste(base_dir,'BID',sep=""),
                        split_method='days',storage_method='rda')
        #setSymbolLookup.FI(base_dir=paste(base_dir,'ASK',sep=""))
		ask <- getSymbols(Symbol,auto.assign=FALSE,from=from,to=to,
                    src='FI',dir=paste(base_dir,'ASK',sep=""),
                        split_method='days',storage_method='rda')
		#setSymbolLookup.FI(base_dir=paste(base_dir,'TRADES',sep=""))		
		bat <- merge(Cl(bid),Cl(ask))
		bat <- na.omit(bat)
        if (hastrades) {
            trade <- getSymbols(Symbol,auto.assign=FALSE,from=from,to=to,
                        src='FI',dir=paste(base_dir,'TRADES',sep=""),
                                split_method='days',storage_method='rda')
            bat <- merge(bat, Cl(trade), all=FALSE)
		    bat <- na.locf(bat,na.rm=TRUE)
        }		
        bat$Mid.Price <- (bat[,1] + bat[,2]) / 2
		bat <- na.omit(bat)
        if (hastrades) {		
            bat <- merge(bat, Vo(trade), all=FALSE)
		    colnames(bat) <- paste(Symbol, c("Bid.Price","Ask.Price","Trade.Price", "Mid.Price", "Volume"),sep=".")		
		} else colnames(bat) <- paste(Symbol, c("Bid.Price","Ask.Price", "Mid.Price"),sep=".")		
        assign(Symbol, bat, pos=env)
		out <- c(out, Symbol)
	}
	out
}

