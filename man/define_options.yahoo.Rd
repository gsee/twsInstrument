\name{define_options.yahoo}
\alias{define_options.yahoo}
\title{define option_series instruments using yahoo...}
\usage{
  define_options.yahoo(symbol, currency = "USD",
    multiplier = 100, tick_size = NULL)
}
\arguments{
  \item{symbol}{chr name of underlying instrument}

  \item{currency}{chr name of currency}

  \item{multiplier}{contract multiplier (usually 100 for
  equity options)}

  \item{tick_size}{minimum price change}
}
\value{
  called for side-effect
}
\description{
  define option_series instruments using yahoo
}
\examples{
\dontrun{
define_options("SPY",src='yahoo')
}
}
\author{
  Garrett See
}
\seealso{
  option_series.yahoo, \code{\link{define_options}},
  \code{\link{define_options.IB}}
}

