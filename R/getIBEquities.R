getIBEquities <-
function(symbols, tws ,barSize='1 min', duration='5 D', env=.GlobalEnv) {
#Gets Trades for all symbols given  
    if (missing(tws) || is.null(tws)) {	
	tws <- try(get('tws',pos=env),silent=TRUE)
        if (inherits(tws,'try-error'))        
            tws <- try(get('tws',pos=.GlobalEnv),silent=TRUE)    
	#isConnected will take care of case where tws wasn't found
    }
    iscon <- try(isConnected(tws),silent=TRUE)
    if(inherits(iscon,"try-error") || iscon==FALSE) tws <- try(twsConnect(),silent=TRUE)	
    #Still can't connect. Try with up to 10 different clientIds.        
    #i <- 1	
    #while(inherits(tws,"try-error")) #try with next clientid
    #{
    #	tws <- try(twsConnect(i),silent=TRUE)
    #	i <- i + 1
    #	if (i >= 10) stop('Too many clientIds in use.')
    #}
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

