#' Establish (force) a connection to TWS or IBG
#' 
#' As long as you are logged-in to either IB TWS or IB Gateway, and at least
#' one of the \code{clientIds} is not in use, a connection will be established.
#' If the first attempt to connect fails, several more connection attempts will
#' be made until a connection is established, or the \code{clientIds} are used up.
#'
#' This will first try to connect using the first value of \code{clientIds} in
#' a call to \code{\link[IBrokers]{twsConnect}}.  If that fails because you are 
#' not connected to TWS, it will try to connect using \code{\link[IBrokers]{ibgConnect}}.  
#' If it fails because the clientId is already in use, it will continue
#' trying each of the values in \code{clientIds} until a connection is established
#' or there are no more to try.
#' @param clientIds numeric vector. These will be tried one at a time in order until a connection is established.
#' @return a connected 'twsconn' object
#' @author Garrett See
#' @seealso \code{\link[IBrokers]{twsConnect}}, \code{\link[IBrokers]{ibgConnect}}
#' @examples
#' \dontrun{
#' con1 <- ConnectIB(851:852)
#' con2 <- ConnectIB(851:852)
#' con3 <- ConnectIB(851:852) #out of ids
#' lapply(list(con1, con2), twsDisconnect)
#' }
#'
ConnectIB <- function(clientIds=1:10) {
    con <- suppressWarnings(try(twsConnect(clientIds[[1]]), silent=TRUE))
    #if (!inherits(con, 'try-error')) return(con)
    if (suppressWarnings(isConnected(con))) return(con)
    if (grep('cannot open the connection', con[[1]]) == 1)
        con <- suppressWarnings(try(ibgConnect(clientIds[[1]]), silent=TRUE))
    #if (!inherits(con, 'try-error')) return(con)
    if (suppressWarnings(isConnected(con))) return(con)
    if (!identical(integer(0), grep('id is already in use', con[[1]]))) {
        if (length(clientIds) > 1) return(ConnectIB(clientIds[-1]))
        else stop(sub("Error : ", "", con[[1]]))
    }
    stop('Please log in to IB TWS or IB Gateway')
}

