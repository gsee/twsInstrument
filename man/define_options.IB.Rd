\name{define_options.IB}
\alias{define_options.IB}
\title{define option_series with IB...}
\usage{
  define_options.IB(underlying_id, strike, expires, right =
  c("C", "P"), currency = "USD", multiplier = 100,
  include_expired = "1", ...)
}
\arguments{
  \item{underlying_id}{vector of underlying_ids}

  \item{strike}{vector of strike prices}

  \item{expires}{vector of expiration dates of format
  YYYYMM}

  \item{right}{\code{"C"}, \code{"P"}, or
  \code{c("C","P")}}

  \item{currency}{name of currency}

  \item{multiplier}{contract multiplier (usually 100 for
  equity options)}

  \item{include_expired}{\dQuote{0} if you do not want to
  define expired contracts}

  \item{\dots}{other arguments to pass through to
  \code{\link{twsInstrument}}}
}
\value{
  called for side-effect
}
\description{
  define option_series with IB
}
\details{
  define option_series instruments using IBrokers

  a wrapper for twsInstrument to define multiple options
  contracts.
}
\note{
  the date in the primary_id will correspond to Expiration
  Saturday, however, the Date in the $expires slot (and in
  $IB$expiry) will correspond to the Friday prior, which is
  the last day trading occurs.
}
\examples{
\dontrun{
define_options.IB('SPY',stike=125)
define_options.IB(c("SPY","DIA"),strike=seq(120,130,5))
define_options('GOOG',600,c('201109','201112'),'C',src='IB')
}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{define_options}},
  \code{\link{option_series.yahoo}}
}

