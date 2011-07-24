
#getQuote.IB <- function(Symbols,src='yahoo', ...

#' @export
get_quote <- function(Symbols, verbose=FALSE, src='IB', ...) {
    if (src != 'IB' && src != 'yahoo') stop("\"IB\" and \"yahoo\" are the only valid values for src.")
    if (src == 'yahoo') getQuote(Symbols)
    else if (src =='IB') get_quote.IB(Symbols,verbose)
}

#' Download current instrument quote from IBrokers
#'
#' \code{get_quote} can be called with \code{src="yahoo"} or \code{src="IB"}
#' If called with \code{src="yahoo"}, \code{\link{getQuote}} from the quantmod package will be called. 
#'
#' \code{get_quote.IB} is an adaptation of code that Jeff sent to the r-sig-finance mailing list.
#' This function will connect to IBrokers and download recent market data for one or many instruments.
#' It will try to connect with clientId 1000. If unsuccessful, it will try again with clientId 1001, and finally, clientId 9999. 
#' (the clientIds are arbitrary.)
#' Once connected, it will request market data and then disconnect. 
#' IB does not give values for \sQuote{Last}, \sQuote{LastSize}, or \sQuote{Volume} for
#' exchange_rates (twsCASH), so those columns will not be included if any of the instruments for which you
#' are requesting a quote are exchange_rates.
#'
#' All instruments should be defined before requesting quotes with \code{src="IB"}, but if any of the requested symbols are not 
#' names of previously defined instruments, they will be treated as stocks denominated in \sQuote{USD}.
#' Internally, the stock instrument will be temporarily created to make the request, and subsequently removed.
#' @param Symbols Can be a vector of instrument names, or a character string of symbols, separated by semi-colons.
#' @param verbose boolean. If TRUE, user will be informed that connection is established, and what is being downloaded.
#' @param tws Currently not implemented.
#' @return data.frame with columns: \sQuote{BidSize}, \sQuote{BidPrice}, \sQuote{AskPrice}, \sQuote{AskSize}.  If none of the quotes are for
#' exchange_rates, it will also contain the columns \sQuote{Last}, \sQuote{LastSize}, and \sQuote{Volume} 
#' @aliases get_quote get_quote.IB
#' @references \url{http://www.mail-archive.com/r-sig-finance@@stat.math.ethz.ch/msg00927.html}
#' @author Garrett See, but the bulk of this comes from Jeff Ryan. See references.
#' @seealso \code{\link{getQuote}}
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
get_quote.IB <- function(Symbols, verbose=FALSE, tws=NULL) {
    if (length(Symbols) == 1) Symbols <- strsplit(Symbols,";")[[1]]
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
                    curMsg <- .Internal(readBin(con, "character",
                      1L, NA_integer_, TRUE, FALSE))
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
                curMsg <- .Internal(readBin(con, "character", 1L,
                    NA_integer_, TRUE, FALSE))
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

    #if any Symbols are not instruments, we'll define them as stocks,
    #try to get the quote, and then remove those instruments from .instrument
    non_instr <- Symbols[which(!Symbols %in% ls_instruments())]
    if (any(!Symbols %in% ls_instruments())) {
        if (!exists("USD")) currency("USD")
        invisible(lapply(non_instr, FUN=function(x) twsInstrument(stock(x,"USD"))))
    }

    tryCatch( {
        if (missing(tws) || 
            is.null(tws) ||
            !inherits(tws, 'twsconn') || 
	        (is.twsConnection(tws) && !isConnected(tws))) 
            tws <- try(twsConnect(1000))
        if (inherits(tws, "try-error")) 
            tws <- try(twsConnect(1001))
        if (inherits(tws, "try-error")) 
            tws <- twsConnect(9999)
        if (isConnected(tws) && verbose) 
            cat(paste("Connected with clientId ", tws$clientId, 
                        ".\n Requesting ", Symbols, "\n", sep = ""))
        if (tws$clientId == 9999) warning("IB TWS should be restarted.")

        ew <- if(is.null(ls_FX(Symbols))) #if any are FX, don't include Last or LastSize columns
                    eWrapper.data(length(Symbols))
              else eWrapper.FXdata(length(Symbols))
      
        qt <- reqMktData(tws, lapply(Symbols,getContract), eventWrapper=ew,CALLBACK=snapShot)
                     
        },finally={twsDisconnect(tws)} )

    #Clean-up if we had to make any instruments
    if (length(non_instr)) rm_instruments(non_instr)
    qt
}

