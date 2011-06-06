\name{getQuote}
\alias{getQuote}
\alias{getQuote.yahoo}
\title{ Download Current Stock Quote }
\description{
THIS IS A PATCHED VERSION
Fetch current stock quote(s) from specified source.  At present this only
handle sourcing quotes from Yahoo Finance, but it will be extended to
additional sources over time.
}
\usage{
getQuote.yahoo(Symbols, what = standardQuote(), ...) 
}
\arguments{
  \item{Symbols}{ character string of symbols, seperated by semi-colons }
  \item{what}{ what should be retrieved }
  \item{\dots}{ currently unused }
}
\value{
\code{define_stocks} depends on this patch. When Jeff updates the quantmod
version, define_stocks should be changed to use that version and this can be deleted.


A maximum of 200 symbols may be requested per call to Yahoo!, and 
all requested will be returned in one data.frame object. 
If more that 200 symbols are requested, multiple 200 symbol calls will be made
and the results will be returned as one data object.

\code{getQuote} returns a data frame with rows matching the number of Symbols
requested, and the columns matching the requested columns.

The \code{what} argument allows for specific data to be requested
from yahoo, using Yahoo!'s formatting string.  A list and interactive
selection tool can be seen with \code{yahooQF}.

}
\references{ 
Yahoo! Finance \url{finance.yahoo.com} 
gummy-stuff.org \url{www.gummy-stuff.org/Yahoo-data.htm}
}
\author{ Jeffrey A. Ryan, patched by Garrett See}
\seealso{ \code{\link{getSymbols}}}
\examples{
yahooQuote.EOD
\dontrun{
getQuote("AAPL")
getQuote("QQQQ;SPY;^VXN",what=yahooQF(c("Bid","Ask")))
standardQuote()
yahooQF()
}
}
\keyword{ IO }
\keyword{ data }

