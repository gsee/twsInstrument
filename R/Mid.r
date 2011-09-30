#' @export
#' @rdname Mid
has.Mid <- function (x, which = FALSE) {
    loc <- grep("Mid", colnames(x), ignore.case = TRUE)
    if (!identical(loc, integer(0))) 
        return(ifelse(which, loc, TRUE))
    ifelse(which, loc, FALSE)
}



#' Return Mid column / check to see if data contains Mid column
#' 
#' \code{Mid} returns the Mid column of an xts object \code{has.Mid} checks to
#' see if an xts object has a Mid column
#' 
#' @aliases Mid has.Mid
#' @param x xts object
#' @param which display position of match
#' @return \code{Mid} returns a univariate xts object \code{has.Mid} returns
#' logical (boolean)
#' @note These should be exported from quantmod.
#' @author gsee
#' @seealso Op, Hi, Lo, Cl, Bid, Ask, has.Op, has.Cl
#' @export
#' @rdname Mid
Mid <- function (x) {
    if (has.Mid(x)) 
        return(x[, grep("Mid", colnames(x), ignore.case = TRUE)])
    stop("subscript out of bounds: no column name containing \"Mid\"")
}

