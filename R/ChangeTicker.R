#' Change the Symbol of data on disk
#' 
#' When a stock changes its ticker symbol, this function can be used to change
#' the name of the ticker associated with the data stored on disk.
#'
#' This will load all data stored for \code{old.ticker} and change the 
#' column names to use the new ticker.  Then it will save a copy of the data
#' under the new ticker.  For now, nothing is deleted, but in the future
#' an argument may be added to allow for that.
#'
#' @param old.ticker character string. The ticker the stock used to have
#' @param new.ticker character string. The ticker the stock has now
#' @param base_dir the base directory that contains at least some of the 
#'   following subdirectories: BID, ASK, TRADES, BAT, BAM.
#' @return called for side-effect
#' @author Garrett See
#' @seealso \code{\link{reqTBBOhistory}}, 
#'   \code{\link[FinancialInstrument]{instrument_attr}} and
#'   \code{\link[FinancialInstrument]{add.identifier}} for updating the 
#'   \code{.instrument} environment (see note section)
#' @note This does not do anything with the \code{.instrument} environment.
#'   You should update your instruments separately (e.g. change the primary_id
#'   to the \code{new.ticker} and add \code{old.ticker} as an identifier of the 
#'   new instrument with \code{\link[FinancialInstrument]{add.identifier}}
#' @examples
#' \dontrun{
#' # If you have data stored on disk for ERTS,
#' # this will save a copy of it in the EA directories
#' ChangeTicker("ERTS", "EA", "/mnt/W/")
#' }
ChangeTicker <- function(old.ticker, new.ticker, base_dir="/mnt/W/") {
    if (substr(base_dir, nchar(base_dir), nchar(base_dir)) != "/") {
        base_dir <- paste0(base_dir, "/")
    }
    for(d in c("BID/", "ASK/", "TRADES/", "BAM/", "BAT/")) {
        of <- list.files(paste0(base_dir, d, old.ticker)) #old files
        dir.create(paste0(base_dir, d, new.ticker), showWarnings=FALSE)
        if (substr(old.ticker, nchar(old.ticker), nchar(old.ticker)) == "/") {
            old.ticker <- substr(old.ticker, 1, nchar(old.ticker) - 1)
        }
        #foreach(i = seq_along(of)) %dopar% {
        for(i in seq_along(of)) {
            nf <- gsub(old.ticker, new.ticker, of[i]) # new file
            s <- load(paste0(base_dir, d, old.ticker, "/", of[i]))
            x <- get(s)
            colnames(x) <- gsub(old.ticker, new.ticker, colnames(x), fixed=TRUE)
            xtsAttributes(x)$updated <- Sys.time()
            assign(new.ticker, x, pos=.GlobalEnv)
            save(list=new.ticker, file=paste0(base_dir, d, new.ticker, "/", nf), 
                 envir=.GlobalEnv)
            rm(x, s)
            rm(list=new.ticker, pos=.GlobalEnv)
        }
    }
}




