
has.Mid <- function (x, which = FALSE) {
    loc <- grep("Mid", colnames(x), ignore.case = TRUE)
    if (!identical(loc, integer(0))) 
        return(ifelse(which, loc, TRUE))
    ifelse(which, loc, FALSE)
}

Mid <- function (x) {
    if (has.Mid(x)) 
        return(x[, grep("Mid", colnames(x), ignore.case = TRUE)])
    stop("subscript out of bounds: no column name containing \"Mid\"")
}

