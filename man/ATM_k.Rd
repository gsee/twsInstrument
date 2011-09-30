\name{ATM_k}
\alias{ATM_k}
\title{At-the-money strike}
\usage{
  ATM_k(options, verbose = TRUE)
}
\arguments{
  \item{options}{vector of option names that have been
  previously defined}

  \item{verbose}{be verbose?}
}
\value{
  numeric
}
\description{
  Returns the At-The-Money strike price of an option chain
}
\details{
  Very preliminary version. Currently, you give it a bunch
  of names of options, it gets the most recent price of the
  underlying using \code{getQuote} and returns the strike
  price that is closest to the last price. See notes.
}
\note{
  TODO: I would like to extend this so that (1)you only
  need to give it an underlying (2)if you give it several
  underlyings, it will give you list or named vector of
  strikes (3)include an argument for expiration date
}
\examples{
\dontrun{
option_series.yahoo('SPY')
ATM_k(ls_options())
ATM_k(ls_options('SPY', match=FALSE))
ATM_k(ls_options(ls_instruments_by('underlying_id','SPY')))

Exp <- ls_expiries(underlying_id='SPY', type='option')[1] #1st expiry of SPY options
ATM_k(ls_by_expiry(Exp))
}
}
\author{
  Garrett See
}
\seealso{
  ATMQuote ls_options, ls_underlyings, ls_strikes,
  ls_instruments_by
}

