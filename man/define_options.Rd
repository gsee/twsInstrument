\name{define_options}
\alias{define_options}
\title{create twsInstruments for options on a given underlying...}
\usage{define_options(..., src="IB")}
\description{create twsInstruments for options on a given underlying}
\details{define option_series instruments.

wrapper to call either \code{\link{define_options.IB}} or \code{\link{define_options.yahoo}}}
\value{called for side-effect}
\author{Garrett See}
\seealso{\code{\link{define_options.IB}}, \code{\link{define_options.yahoo}}
option_series.yahoo,
option, option_series, twsInstrument}
\arguments{\item{\dots}{arguments to pass to other methods}
\item{src}{where to get option_series metadata; currently only \dQuote{IB} and \dQuote{yahoo} are implemented}}
\examples{\dontrun{
define_options("SPY",strike=125, src='IB')
define_options("SPY",src='yahoo')
}}
