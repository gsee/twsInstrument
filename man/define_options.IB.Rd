\name{define_options.IB}
\alias{define_options.IB}
\title{define option_series with IB...}
\usage{define_options.IB(underlying_id, strike, expiries, right=c("C", "P"),
    currency="USD", multiplier=100, include_expired="1", ...)}
\description{define option_series with IB}
\details{define option_series instruments using IBrokers

a wrapper for twsInstrument to define multiple options contracts.}
\value{called for side-effect}
\author{Garrett See}
\seealso{\code{\link{define_options}}, \code{\link{option_series.yahoo}}}
\arguments{\item{underlying_id}{vector of underlying_ids}
\item{strike}{vector of strike prices}
\item{expiries}{vector of expiration dates of format YYYYMM}
\item{right}{\code{"C"}, \code{"P"}, or \code{c("C","P")}}
\item{currency}{name of currency}
\item{multiplier}{contract multiplier (usually 100 for equity options)}
\item{include_expired}{\dQuote{0} if you do not want to define expired contracts}
\item{\dots}{other arguments to pass through to \code{\link{twsInstrument}}}}
\examples{\dontrun{
define_options.IB('SPY',125)
define_options.IB(c("SPY","DIA"),seq(120,130,5))
define_options('GOOG',600,c('201109','201112'),'C',src='IB')
}}
