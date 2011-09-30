\name{option_id}
\alias{option_id}
\title{make primary_ids for options...}
\usage{
  option_id(underlying_id, strike, month, year, right =
  c("C", "P"))
}
\arguments{
  \item{underlying_id}{chr vector of names of underlyings.}

  \item{strike}{vector of strike prices}

  \item{month}{numeric vector of months (partial matches to
  \code{month.name} will also work)}

  \item{year}{numeric vector of years (can be 1 digit, 2
  digit, or 4 digit years)}

  \item{right}{\sQuote{C} or \sQuote{P} (\sQuote{call} and
  \sQuote{put} will also work)}
}
\value{
  chr vector of primary_ids (or suffix_ids if underlying_id
  is missing, NULL, or \dQuote{}) for option_series
}
\description{
  make primary_ids for options
}
\details{
  create a primary_id for options. The only difference
  between the ids this generates, and those of the Option
  Symbology Initiative (OSI) is that these have an
  underscore separating the primary_id from the suffix_id.
  The format for a 125 strike call on SPY expiring in
  December of 2011 would be \sQuote{SPY_111217C120}.  The
  first 6 digits of the suffix correspond to the expiration
  YYMMDD where DD is calculated as the Saturday following
  the 3rd Friday of the month.
}
\note{
  does not support weekly or EOM options
}
\examples{
option_id("SPY",125,'Sep',2011,'C')
option_id("SPY",130,1:12,11,'C')
option_id(c("SPY","DIA"),seq(120,130,5))
}
\author{
  Garrett See
}
\seealso{
  future_id, build_series_symbols, build_spread_symbols
}

