\name{define_FX}
\alias{define_FX}
\alias{define_exchange_rates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
exchange_rate class constructor
}
\description{
Quickly define several exhange rate instruments.
}
\usage{
define_FX(pairs, counter_currencies = NULL, second_currencies = NULL, use.IB = TRUE)
define_exchange_rates(pairs, counter_currencies = NULL, second_currencies = NULL, use.IB = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pairs}{
chr names of currency pairs to be defined. Each currency pair name should be the base currency and base currency, separated by a period e.g. EUR.USD
Optional.
}
  \item{counter_currencies}{
chr names of first currencies of pairs to define. Optional
}
  \item{second_currencies}{
chr names of second currencies of pairs to define. Optional
}
  \item{use.IB}{
call update_instruments.IB ?
}
}
\details{
If nothing is given, 9 currency pairs will be defined: "AUD.USD" "EUR.JPY" "EUR.USD" "GBP.JPY" "GBP.USD" "NZD.USD" "USD.CAD" "USD.CHF" "USD.JPY"
If \code{pairs} is given, it should be a vector of pair names formatted in the same manner as above.
If \code{pairs} is not given, but \code{base_currencies} and \code{quote_currencies} are, then it will make all base/quote pairs possible using those.
If only \code{counter_currencies} are given, then if only one currency exists in the .instrument environment, it will be used as the counter_currency. Otherwise, "USD" will be used.
}
\value{
Called for side-effect
}
\author{
Garrett See
}
\seealso{
exchange_rate, ls_exchange_rates(), twsInstrument
}
\examples{
\dontrun{
#make EUR.USD, EUR.JPY, GBP.USD, GBP.JPY
define_FX(counter_currencies=c("EUR","GBP"), second_currencies=c("USD","JPY")) 

define_FX(c("USD.CHF","NZD.USD))

define_FX()
}
}
