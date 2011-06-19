saveInstruments <- function(file_name="MyInstruments", dir="", extension="RData") {
	if (!is.null(dir) && !dir == "" && substr(dir,nchar(dir),nchar(dir)) != "/")
		dir <- paste(dir,"/",sep="")
	.instrument <- get('.instrument', pos=.GlobalEnv)
	save(.instrument,file=paste(dir,file_name,".",extension,sep=""))	
}

loadInstruments <-function(file_name="MyInstruments", dir="", extension="RData", env=parent.frame()) {
	if (!is.null(dir) && !dir == "" && substr(dir,nchar(dir),nchar(dir)) != "/")
		dir <- paste(dir,"/",sep="")
	load(paste(dir,file_name,".",extension,sep=""),envir=env)
}

