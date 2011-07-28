\name{get_quote.yahoo}
\alias{get_quote.yahoo}
\title{Download current instrument quote from yahoo...}
\usage{get_quote.yahoo(Symbols, what=standardQuote(), waitTime=30, ...)}
\description{Download current instrument quote from yahoo}
\details{This \code{get_quote.yahoo} method is the same as Jeff Ryan's code for getQuote.yahoo (see also) except for 2 changes. 
The first is a patch to allow custom quote formats for requests for 200 symbols or more. (put the \sQuote{what} arg in the rbind)  
The second is that in this version, the quote requests are wrapped in a while loop.  If the timestamp
of the receied quote has a year that is different than the current year (as reported by \code{Sys.time()}), 
it will keep trying until either the year in the quote is the same as the current year, or \code{waitTime} has passed.
Thanks to Zachary Mayar for suggesting the change, and Samo Pahor for providing the specific patch for this code.}
\value{a data frame with rows matching the number of Symbols requested, and the columns matching the requested columns.}
\references{\url{http://r.789695.n4.nabble.com/getQuote-problem-tt3689746.html}}
\seealso{quantmod:::getQuote.yahoo, \code{\link{get_quote}}, \code{\link{get_quote.IB}}}
\arguments{\item{Symbols}{Can be a vector of instrument names, or a character string of symbols, separated by semi-colons.}
\item{what}{what should be retrieved}
\item{waitTime}{time in seconds that is the longest you're willing to wait to get back a quote with a valid timestamp.}
\item{\dots}{other args}}
