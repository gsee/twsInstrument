\name{define_options}
\alias{define_options}
\alias{define_options.yahoo}
\title{
create twsInstruments for options on a given underlying
}
\description{
Get option chains for a given underlying and create instruments (or twsInstruments)
}
\usage{
define_options(symbol, currency = "USD", multiplier = 100, tick_size = NULL, use.IB = TRUE, src = "yahoo")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{symbol}{
ticker symbol. character string
}
  \item{currency}{
base currency. character string
}
  \item{multiplier}{
contract multiplier (usually 100 for stocks)
}
  \item{tick_size}{
minimum price change
}
%  \item{first_traded}{
%first date it is tradeable 
%}
  \item{use.IB}{
Should IBrokers be used to get twsContracts?
}
  \item{src}{
where to get the options_chain. Currently only yahoo is implemented
}
}
\details{
will automatically create currency if it doesn't exist.

only works for stock options
}
\value{
called for side-effect
}
\author{
Garrett See
}
\note{
at some point, define_options will operate similarly to getSymbols by implementing src and Defaults.
}
\seealso{
option, option_series, twsInstrument
}
\examples{
\dontrun{
define_options("SPY")
}
}

