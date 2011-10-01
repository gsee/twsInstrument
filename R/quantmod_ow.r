#' @export
getQuote.yahoo <-
function (Symbols, what = standardQuote(), ...) 
{
    tmp <- tempfile()
    if (length(Symbols) > 1 && is.character(Symbols)) 
        Symbols <- paste(Symbols, collapse = ";")
    length.of.symbols <- length(unlist(strsplit(Symbols, ";")))
    if (length.of.symbols > 200) {
        Symbols <- unlist(strsplit(Symbols, ";"))
        all.symbols <- lapply(seq(1, length.of.symbols, 200), 
            function(x) na.omit(Symbols[x:(x + 199)]))
        df <- NULL
        cat("downloading set: ")
        for (i in 1:length(all.symbols)) {
            Sys.sleep(0.5)
            cat(i, ", ")
            df <- rbind(df, getQuote.yahoo(all.symbols[[i]],what))
        }
        cat("...done\n")
        return(df)
    }
    Symbols <- paste(strsplit(Symbols, ";")[[1]], collapse = "+")
    if (inherits(what, "quoteFormat")) {
        QF <- what[[1]]
        QF.names <- what[[2]]
    }
    else {
        QF <- what
        QF.names <- NULL
    }
    QF <- paste("d1t1", QF, sep = "")
    download.file(paste("http://finance.yahoo.com/d/quotes.csv?s=", 
        Symbols, "&f=", QF, sep = ""), dest = tmp, quiet = TRUE)
    sq <- read.csv(file = tmp, sep = ",", stringsAsFactors = FALSE, 
        header = FALSE)
    unlink(tmp)
    Qposix <- strptime(paste(sq[, 1], sq[, 2]), format = "%m/%d/%Y %H:%M")
    Symbols <- unlist(strsplit(Symbols, "\\+"))
    df <- data.frame(Qposix, sq[, 3:NCOL(sq)])
    rownames(df) <- Symbols
    if (!is.null(QF.names)) {
        colnames(df) <- c("Trade Time", QF.names)
    }
    df
}

