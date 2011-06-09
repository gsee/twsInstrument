\name{ATMQuote}
\alias{ATMQuote}

\title{
At-the-money quote
}
\description{
Returns quotes for at-the-money options on a given stock
}
\usage{
ATMQuote(symbol, Exp)
}
\arguments{
  \item{symbol}{
yahoo stock symbol
}
  \item{Exp}{
Expiration dates; YYYY-MM
}
}
\details{
Makes a call to getOptionChain and getQuote.  Returns quotes for options struck at the price that is closest to the last quoted price of the underlying.
Does not support multiple expiration months.  ALPHA code.
}
\value{
xts object
}
\author{
Garrett See
}
\seealso{
ATM_k, getOptionChain, getQuote
}
\examples{
\dontrun{
ATMQuote('SPY')
}
}

