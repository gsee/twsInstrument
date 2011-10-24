#' requests historical data for stocks from IB
#' 
#' A wrapper to request historical data for several symbols at once
#' 
#' gets Open, High, Low, Close, Volume, etc. for TRADES
#' 
#' The below is taken from reqHistoricalData help page, see that help for more.
#' 
#' Legal \code{barSize} settings are \sQuote{1 secs},\sQuote{5 secs},\sQuote{15
#' secs}, \sQuote{30 mins},\sQuote{1 min},\sQuote{2 mins}, \sQuote{3
#' mins},\sQuote{5 mins},\sQuote{15 mins}, \sQuote{30 mins},\sQuote{1
#' hour},\sQuote{1 day}, \sQuote{1 week},\sQuote{1 month},\sQuote{3 months},
#' and \sQuote{1 year}.
#' 
#' The duration string must be of the form \sQuote{n S} where the last
#' character may be any one of \sQuote{S} (seconds), \sQuote{D} (days),
#' \sQuote{W} (weeks), \sQuote{M} (months), and \sQuote{Y} (year). At present
#' the limit for years is 1.
#' 
#' @param symbols character vector
#' @param tws twsConnection object
#' @param barSize see details for valid values
#' @param duration timespan request covers
#' @param env environment to put data in; also the environment to look for
#' twsConnection object if not supplied
#' @return called for its side effect
#' @author Garrett See
#' @seealso reqHistoricalData, reqHistory, getBAT, getBAThistory
#' @examples
#' 
#' \dontrun{
#' getIBEquities(c('SPY','DIA'))
#' }
#' @export
getIBEquities <-
function(symbols, tws ,barSize='1 min', duration='5 D', env=.GlobalEnv) {
#Gets Trades for all symbols given  
    if (missing(tws) || is.null(tws)) {	
	tws <- try(get('tws',pos=env),silent=TRUE)
        if (inherits(tws,'try-error'))        
            tws <- try(get('tws',pos=.GlobalEnv),silent=TRUE)    
	#isConnected will take care of case where tws wasn't found
    }
    iscon <- suppressWarnings(try(isConnected(tws),silent=TRUE))
    if(inherits(iscon,"try-error") || iscon==FALSE) tws <- ConnectIB(c(125:129, 150))
    if (tws$clientId == 150) warning("Interactive Brokers should be restarted.")
    if (is.environment(env) || length(symbols) > 1) {
        assign('tws',tws,pos=env)
        use.env=TRUE
    } else use.env=FALSE
	#TODO: check to see if tscale, barSize, or duration exist and !is.null
    for (i in 1:length(symbols)) {
	contract <- twsEquity(symbols[i],'SMART')
	if (i > 1) {
            cat("Pausing 10 seconds between requests ...")
            Sys.sleep(10)
            cat(" Request sent ... \n")
	}
        if (use.env) {
	    assign(symbols[i], reqHistoricalData(tws,contract, 
                  barSize = barSize, duration=duration), pos=env) 
        } else out <- reqHistoricalData(tws,contract, 
                    barSize = barSize, duration=duration)
    }
    twsDisconnect(tws)
    if (use.env) out <- symbols
    out
}

