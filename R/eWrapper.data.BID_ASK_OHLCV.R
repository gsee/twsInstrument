#' eWrapper for getting BID, ASK, LAST and OHLCV values from IB...
#' 
#' eWrapper for getting BID, ASK, LAST and OHLCV values from IB
#' 
#' This is \code{eWrapper.data} modified to include the Open, High, Low, and
#' Close columns.
#' 
#' @param n number of contracts being watched
#' @return list of functions
#' @author Samo Pahor
#' @seealso \code{eWrapper.data}
#' @export
eWrapper.data.BID_ASK_OHLCV <- function(n) {
	# internally updated data
	eW <- eWrapper(NULL)  # use basic template
	eW$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_,11),ncol=11),0),
									.Dimnames=list(NULL,
											c("BidSize","BidPrice",
													"AskPrice","AskSize",
													"Last",
													"Open","High","Low","Close",
													"LastSize","Volume")))),n))
	
	eW$tickPrice <- function(curMsg, msg, timestamp, file, ...) 
	{
		tickType = msg[3]
		msg <- as.numeric(msg)
		id <- msg[2] #as.numeric(msg[2])
		data <- eW$get.Data("data") #[[1]]  # list position of symbol (by id == msg[2])
		attr(data[[id]],"index") <- as.numeric(Sys.time())
#    data[[1]] <- rbind(data[[1]],.xts(matrix(rep(NA_real_,7),ncol=7), Sys.time()))
		nr.data <- NROW(data[[id]])
		#data[[id]][1] <- as.numeric(Sys.time()) #timestamp
		if(tickType == .twsTickType$BID) {
			data[[id]][nr.data,1:2] <- msg[5:4]
		} else
		if(tickType == .twsTickType$ASK) {
			data[[id]][nr.data,3:4] <- msg[4:5]
		} else
		if(tickType == .twsTickType$LAST) {
			data[[id]][nr.data,5] <- msg[4]
		} else
		if(tickType == .twsTickType$OPEN) {
			data[[id]][nr.data,6] <- msg[4]
		} else
		if(tickType == .twsTickType$HIGH) {
			data[[id]][nr.data,7] <- msg[4]
		} else
		if(tickType == .twsTickType$LOW) {
			data[[id]][nr.data,8] <- msg[4]
		} else
		if(tickType == .twsTickType$CLOSE) {
			data[[id]][nr.data,9] <- msg[4]
		}
		eW$assign.Data("data", data)
		c(curMsg, msg)
	}
	eW$tickSize  <- function(curMsg, msg, timestamp, file, ...) 
	{ 
		data <- eW$get.Data("data")
		tickType = msg[3]
		msg <- as.numeric(msg)
		id <- as.numeric(msg[2])
#    data[[1]] <- rbind(data[[1]],.xts(matrix(rep(NA_real_,7),ncol=7), Sys.time()))
		attr(data[[id]],"index") <- as.numeric(Sys.time())
		nr.data <- NROW(data[[id]])
		#data[[id]][1] <- as.numeric(Sys.time()) #timestamp
		if(tickType == .twsTickType$BID_SIZE) {
			data[[id]][nr.data,1] <- msg[4]
		} else
		if(tickType == .twsTickType$ASK_SIZE) {
			data[[id]][nr.data,4] <- msg[4]
		} else 
		if(tickType == .twsTickType$LAST_SIZE) {
			data[[id]][nr.data,10] <- msg[4]
		} else
		if(tickType == .twsTickType$VOLUME) {
			data[[id]][nr.data,11] <- msg[4]
		}
		eW$assign.Data("data", data)
		c(curMsg, msg)
	}
	return(eW)
}
