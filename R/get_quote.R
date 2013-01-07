#getQuote.IB <- function(Symbols,src='yahoo', ...

#' @export
#' @rdname get_quote.IB
get_quote <- function(Symbols, src='IB', ...) {
#    if (src != 'IB' && src != 'yahoo') stop("\"IB\" and \"yahoo\" are the only valid values for src.")
#    if (src == 'yahoo') getQuote(Symbols)
#    else 
    do.call(paste('get_quote', src, sep='.'), list(Symbols=Symbols, ...))
}



#' Download current instrument quote using IBrokers...
#' 
#' Download current instrument quote using IBrokers
#' 
#' \code{get_quote} can be called with \code{src="yahoo"} or \code{src="IB"}.
#' 
#' \code{get_quote.IB} is an adaptation of code that Jeff sent to the
#' r-sig-finance mailing list. This function will connect to IBrokers and
#' download recent market data for one or many instruments. It will try to
#' connect with clientId 1000. If unsuccessful, it will try again with clientId
#' 1001, and finally, clientId 9999.  (the clientIds are arbitrary.) Once
#' connected, it will request market data and then disconnect.  IB does not
#' give values for \sQuote{Last}, \sQuote{LastSize}, or \sQuote{Volume} for
#' exchange_rates (twsCASH), so those columns will not be included if any of
#' the instruments for which you are requesting a quote are exchange_rates.
#' 
#' All instruments should be defined before requesting quotes with
#' \code{src="IB"}, but if any of the requested symbols are not names of
#' previously defined instruments, they will be treated as stocks denominated
#' in \sQuote{USD}. Internally, the stock instrument will be temporarily
#' created to make the request, and subsequently removed.
#' 
#' @aliases get_quote get_quote.IB
#' @param Symbols Can be a vector of instrument names, or a character string of
#' symbols, separated by semi-colons.
#' @param verbose boolean. If TRUE, user will be informed that connection is
#' established, and what is being downloaded.
#' @param tws Currently not implemented.
#' @param \dots other arguments such as \sQuote{eWrapper}
#' @param src method to use to get the quote. Only "IB" and "yahoo" supported
#' @return data.frame with columns: \sQuote{BidSize}, \sQuote{BidPrice},
#' \sQuote{AskPrice}, \sQuote{AskSize}.  If none of the quotes are for
#' exchange_rates, it will also contain the columns \sQuote{Last},
#' \sQuote{LastSize}, and \sQuote{Volume}
#' @author Garrett See, but the bulk of this comes from Jeff Ryan. See
#' references.
#' @seealso \code{\link{get_quote.yahoo}}
#' @references
#' \url{http://www.mail-archive.com/r-sig-finance@@stat.math.ethz.ch/msg00927.html}
#' @examples
#' \dontrun{
#' define_stocks(c("SPY","DIA"))
#' fut <- front_future(c("ES","YM"))
#' get_quote(ls_twsInstruments())  
#' get_quote(ls_stocks(),src='yahoo')
#' define_FX('EURUSD')
#' get_quote(ls_twsInstruments()) #will not include trade related data because EURUSD doesn't have it
#' }
#' @export
#' @rdname get_quote.IB
get_quote.IB <- function(Symbols, verbose=FALSE, tws=NULL, ...) {
    if (length(Symbols) == 1) Symbols <- strsplit(Symbols,";")[[1]]
    if (!hasArg(silent)) {
        silent <- !isTRUE(verbose)
    } else silent <- list(...)[["silent"]]
    snapShot <- function (twsCon, eWrapper, timestamp, file, playback = 1, ...)
    {
        if (missing(eWrapper))
            eWrapper <- eWrapper()
        names(eWrapper$.Data$data) <- eWrapper$.Data$symbols
        con <- twsCon[[1]]
        if (inherits(twsCon, "twsPlayback")) {
            sys.time <- NULL
            while (TRUE) {
                if (!is.null(timestamp)) {
                    last.time <- sys.time
                    sys.time <- as.POSIXct(strptime(paste(readBin(con,
                      character(), 2), collapse = " "), timestamp))
                    if (!is.null(last.time)) {
                      Sys.sleep((sys.time - last.time) * playback)
                    }
                    curMsg <- readBin(con, "character", 1L)
                    if (length(curMsg) < 1)
                      next
                    processMsg(curMsg, con, eWrapper, format(sys.time,
                      timestamp), file, ...)
                }
                else {
                    curMsg <- readBin(con, character(), 1)
                    if (length(curMsg) < 1)
                      next
                    processMsg(curMsg, con, eWrapper, timestamp,
                      file, ...)
                    if (curMsg == .twsIncomingMSG$REAL_TIME_BARS)
                      Sys.sleep(5 * playback)
                }
            }
        }
        else {
            while (TRUE) {
                socketSelect(list(con), FALSE, NULL)
                curMsg <- readBin(con, "character", 1L)
                if (!is.null(timestamp)) {
                    processMsg(curMsg, con, eWrapper, format(Sys.time(),
                      timestamp), file, ...)
                }
                else {
                    processMsg(curMsg, con, eWrapper, timestamp,
                      file, ...)
                }
                if (!any(sapply(eWrapper$.Data$data, is.na)))
                    return(do.call(rbind, lapply(eWrapper$.Data$data,
                      as.data.frame)))
            }
        }
    }
    length.of.symbols <- length(Symbols)
    if (length.of.symbols > 50) {
        Symbols <- unlist(strsplit(Symbols, ";"))
        all.symbols <- lapply(seq(1, length.of.symbols, 50), 
            function(x) na.omit(Symbols[x:(x + 49)]))
        df <- NULL
        cat("downloading set: ")
        for (i in 1:length(all.symbols)) {
            Sys.sleep(0.5)
            cat(i, ", ")
            df <- rbind(df, get_quote.IB(all.symbols[[i]],verbose,tws))
        }
        cat("...done\n")
        return(df)
    }

    if (missing(tws) || 
        is.null(tws) ||
        !inherits(tws, 'twsconn') || 
        (is.twsConnection(tws) && !isConnected(tws))) 
        tws <- ConnectIB(c(130:134,150))
    tryCatch( 
    {
        if (suppressWarnings(isConnected(tws)) && isTRUE(verbose)) 
            cat(paste("Connected with clientId ", tws$clientId, 
                        ".\n Requesting ", Symbols, "\n", sep = ""))
        if (tws$clientId == 150) warning("IB TWS should be restarted.")
        contracts <- lapply(Symbols, getContract, silent=TRUE)
        if (hasArg(eWrapper)) {
            eW <- list(...)$eWrapper
            ew <- eW(length(Symbols))
        } else ew <- if(any(lapply(contracts, "[[", "sectype") == "CASH"))
                    eWrapper.FXdata(length(Symbols))  
              else eWrapper.data(length(Symbols))      
        qt <- reqMktData(tws, contracts, 
                         eventWrapper=ew, CALLBACK=snapShot)
                  
    },finally={try(twsDisconnect(tws), silent=TRUE)} )
    qt
}

#' @export
#' @rdname get_quote.IB
getQuote.IB <- get_quote.IB


#' Download current instrument quote from yahoo...
#' 
#' Download current instrument quote from yahoo
#' 
#' This \code{get_quote.yahoo} method is the same as Jeff Ryan's code for
#' getQuote.yahoo (see also) except for two differences.  First, the quote 
#' requests are wrapped in a while loop.  If the timestamp of the receied
#' quote has a year that is different than the current year (as reported by
#' \code{Sys.time()}), it will keep trying until either the year in the quote
#' is the same as the current year, or \code{waitTime} has passed. Thanks to
#' Zachary Mayar for suggesting the change, and Samo Pahor for providing the
#' specific patch for this code.  Second, if the \code{Symbols} are defined 
#' \code{instrument}s that have a \sQuote{yahoo} identifier.
#' 
#' @param Symbols Can be a vector of instrument names, or a character string of
#' symbols, separated by semi-colons.
#' @param what what should be retrieved
#' @param waitTime time in seconds that is the longest you're willing to wait
#' to get back a quote with a valid timestamp.
#' @param \dots args to pass to \code{\link[FinancialInstrument]{getSymbols.FI}}
#' @return a data frame with rows matching the number of Symbols requested, and
#' the columns matching the requested columns.
#' @seealso \code{\link[quantmod]{getQuote}},
#' \code{\link{get_quote.IB}}
#' @references
#' \url{http://r.789695.n4.nabble.com/getQuote-problem-tt3689746.html}
#' @examples
#' \dontrun{
#' ibak <- as.list(FinancialInstrument:::.instrument) #backup instruments
#' rm_instruments()
#' # create some instruments and give them 'yahoo' identifiers
#' synthetic("SPX", currency("USD"), identifiers=list(yahoo="^GSPC"))
#' future("ES", currency("USD"), multiplier=50, underlying_id="SPX")
#' ## figure out front month contract for ES
#' (fid <- future_id("ES", format(Sys.Date(), "%b"), format(Sys.Date(), "%y")))
#' (yahooid <- paste(format_id(fid, sep=""), "CME", sep="."))
#' ## define future_series, adding yahoo identifier
#' future_series(fid, identifiers=list(yahoo=yahooid))
#' s <- c("SPX", "SPY", "ES_M12")
#' get_quote.yahoo(s)
#' get_quote(s, src='yahoo') #same
#' ## restore previous instrument environment
#' reloadInstruments(ibak)
#' }
#' @export
get_quote.yahoo<-function(Symbols,what=standardQuote(),waitTime=30,...) { 
	tmp <- tempfile()
	if(length(Symbols) > 1 && is.character(Symbols)) 
		Symbols <- paste(Symbols,collapse=";") 
    s <- unlist(strsplit(Symbols, ";"))
    get_identifier <- function(x) {
        instr <- getInstrument(x, silent=TRUE, ...)
        if (is.instrument(instr)) {
            idents <- instr[["identifiers"]]
            names(idents) <- tolower(names(idents))
            if ("yahoo" %in% names(idents)) {
                return(idents[["yahoo"]])
            } 
        }
        return(x)
    }
	Symbols <- paste(unname(sapply(s, get_identifier)), collapse=";")
	length.of.symbols <- length(s) 
	if(length.of.symbols > 200) { 
		# yahoo only works with 200 symbols or less per call 
		# we will recursively call getQuote.yahoo to handle each block of 200 
		Symbols <- unlist(strsplit(Symbols,";")) 
		all.symbols <- lapply(seq(1,length.of.symbols,200), function(x) na.omit(Symbols[x:(x+199)])) 
		df <- NULL 
		cat("downloading set: ") 
		for(i in 1:length(all.symbols)) { 
			Sys.sleep(0.5) 
			cat(i,", ") 
			df <- rbind(df, get_quote.yahoo(all.symbols[[i]],what)) 
		}
		cat("...done\n") 
		return(df)
	}
	Symbols <- paste(strsplit(Symbols,';')[[1]],collapse="+") 
	if(inherits(what, 'quoteFormat')) { 
		QF <- what[[1]] 
		QF.names <- what[[2]] 
	} else { 
		QF <- what
		QF.names <- NULL 
	}
	QF <- paste('d1t1',QF,sep='') 
	download.file(paste("http://finance.yahoo.com/d/quotes.csv?s=", 
        Symbols,"&f=",QF,sep=""), destfile=tmp,quiet=TRUE) 
	Year <- 1970 
	currentYear <- as.numeric(format(Sys.time(),'%Y')) 
	start_time <- Sys.time()
	while (Year != currentYear) { 
    #loop until we get a valid quote, or it's been longer than 'waitTime'
    #Thanks to Zachary Mayer for the suggestion and Samo Pahor for patching this code.
		sq <- read.csv(file=tmp,sep=',',stringsAsFactors=FALSE,header=FALSE) 
		unlink(tmp)
		Qposix <- strptime(paste(sq[,1],sq[,2]),format='%m/%d/%Y %H:%M') 
		Symbols <- unlist(strsplit(Symbols,'\\+')) 
		df <- data.frame(Qposix,sq[,3:NCOL(sq)]) 
		rownames(df) <- Symbols
		if(!is.null(QF.names)) { 
			colnames(df) <- c('Trade Time',QF.names) 
		}
		Year <- as.numeric(format(df[1, 'Trade Time'],'%Y')) 
		if ((Sys.time() - start_time) >= waitTime) break
		Sys.sleep(0.5) 
	}
	df
}

