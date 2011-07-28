\name{get_quote.IB}
\title{Download current instrument quote using IBrokers...}
\usage{get_quote.IB(Symbols, verbose=FALSE, tws, ...)}
\description{Download current instrument quote using IBrokers}
\details{\code{get_quote} can be called with \code{src="yahoo"} or \code{src="IB"}.  

\code{get_quote.IB} is an adaptation of code that Jeff sent to the r-sig-finance mailing list.
This function will connect to IBrokers and download recent market data for one or many instruments.
It will try to connect with clientId 1000. If unsuccessful, it will try again with clientId 1001, and finally, clientId 9999. 
(the clientIds are arbitrary.)
Once connected, it will request market data and then disconnect. 
IB does not give values for \sQuote{Last}, \sQuote{LastSize}, or \sQuote{Volume} for
exchange_rates (twsCASH), so those columns will not be included if any of the instruments for which you
are requesting a quote are exchange_rates.

All instruments should be defined before requesting quotes with \code{src="IB"}, but if any of the requested symbols are not 
names of previously defined instruments, they will be treated as stocks denominated in \sQuote{USD}.
Internally, the stock instrument will be temporarily created to make the request, and subsequently removed.}
\value{data.frame with columns: \sQuote{BidSize}, \sQuote{BidPrice}, \sQuote{AskPrice}, \sQuote{AskSize}.  If none of the quotes are for
exchange_rates, it will also contain the columns \sQuote{Last}, \sQuote{LastSize}, and \sQuote{Volume}}
\alias{get_quote}
\alias{get_quote.IB}
\references{\url{http://www.mail-archive.com/r-sig-finance@stat.math.ethz.ch/msg00927.html}}
\author{Garrett See, but the bulk of this comes from Jeff Ryan. See references.}
\seealso{\code{\link{get_quote.yahoo}}}
\arguments{\item{Symbols}{Can be a vector of instrument names, or a character string of symbols, separated by semi-colons.}
\item{verbose}{boolean. If TRUE, user will be informed that connection is established, and what is being downloaded.}
\item{tws}{Currently not implemented.}
\item{\dots}{other arguments such as \sQuote{eWrapper}}}
\examples{\dontrun{
define_stocks(c("SPY","DIA"))
fut <- front_future(c("ES","YM"))
get_quote(ls_twsInstruments())  
get_quote(ls_stocks(),src='yahoo')
define_FX('EURUSD')
get_quote(ls_twsInstruments()) #will not include trade related data because EURUSD doesn't have it
}}
