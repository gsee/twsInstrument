saveInstruments <- function(file_name="MyInstruments", dir="", extension="RData") {
	if (!is.null(dir) && !dir == "" && substr(dir,nchar(dir),nchar(dir)) != "/")
		dir <- paste(dir,"/",sep="")
	.instrument <- get('.instrument', pos=.GlobalEnv)
	save(.instrument,file=paste(dir,file_name,".",extension,sep=""))	
}

loadInstruments <-function(file_name="MyInstruments", dir="", extension="RData", env=.GlobalEnv) {
	if (!is.null(dir) && !dir == "" && substr(dir,nchar(dir),nchar(dir)) != "/")
		dir <- paste(dir,"/",sep="")
    tmpenv <- new.env()
	load(paste(dir,file_name,".",extension,sep=""),envir=tmpenv)
    .instrument <- get(".instrument",pos=env)
    il <- ls(tmpenv$.instrument,all.names=TRUE)
    for (i in il) {
        .instrument[[i]] <- tmpenv$.instrument[[i]]
    }
    assign(".instrument",.instrument, pos=env)
}

