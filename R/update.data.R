
update.data <- function(Symbols, base_dir='/mnt/W', maxDays=365, useRTH=0) {
    for (s in Symbols) {
        nDays=maxDays
        instr <- getInstrument(s)
        tmpexpires <- instr$expires
        expired <- if (is.null(tmpexpires)) { 
            FALSE       
        } else {
            if (nchar(tmpexpires) == 8) {
                expires <- as.Date(tmpexpires, format='%Y%m%d')
            } else if (nchar(tmpexpires) == 10) {
                expires <- as.Date(tmpexpires, format='%Y-%m-%d')
            } else if (nchar(tmpexpires) == 7) {
                expires <- as.Date(paste(tmpexpires,"01",sep="-"), format='%Y-%m-%d') + 30
            } else stop("write something to handle expires of nchar ", nchar(tmpexpires))
            if (expires < Sys.Date()) {TRUE} else FALSE
        }
        if (expired) {
            if (s %in% list.files(paste(base_dir, "BID/", sep="/"), all.files=TRUE)) {
                nDays <- as.numeric(expires - as.Date(get_to(s, base_dir=paste(base_dir, 'BID/', sep="/"))))-1
            }
        } else {
            if (s %in% list.files(paste(base_dir, "BID/",sep="/"), all.files=TRUE)) {
                nDays <- Sys.Date() - as.Date(get_to(s, base_dir=paste(base_dir, "BID/", sep="/")))
            } else if (s %in% list.files(paste(base_dir, "TRADES/",sep="/"), all.files=TRUE)) {
                nDays <- Sys.Date() - as.Date(get_to(s, base_dir=paste(base_dir, "TRADES/", sep="/")))
            } 
        }

        # if there is no directory for this stock, we'll use 365 for nDays to get max data allowed
        if (nDays > 0) {
            nDays <- ceiling(nDays/5) * 5 #round up to the nearest 5
            reqTBBOhistory(s, base_dir=base_dir, ndays=nDays, save=TRUE, useRTH=useRTH, chronological=TRUE)
            rm(list=s, pos=.GlobalEnv)
        }

    }
}

update.data.all <- function(base_dir = "/mnt/W/") {
    sep = if (substr(base_dir,nchar(base_dir),nchar(base_dir)) == "/") {""} else "/"
    batdir <- paste(base_dir, "BAT", sep=sep)
    out <- lf <- list.files(batdir)
    out <- out[!out %in% out[grep("\\.",out)]]
    out2 <- do.call(c, lapply(out, function(x) {
        xx <- try(twsInstrument(x)); 
        if(!inherits(xx, 'try-error')) xx
    }))
    out3 <- update.data(out2)
    list(updated=out3, could.not.define=out[!out %in% out2], not.attempted=out[grep("\\.",out)])
}


update.stocks <- function(base_dir='/mnt/W', useRTH=1) {
    loadInstruments("MyStocks.RData", base_dir)
    update.data(ls_stocks(), base_dir, useRTH=1)
}

update.FX <- function(base_dir='/mnt/W', useRTH=1) {
    loadInstruments("MyFX.RData", base_dir)
    update.data(ls_exchange_rates(), base_dir, useRTH=0)
}

#FXfuts, CLfuts, 
