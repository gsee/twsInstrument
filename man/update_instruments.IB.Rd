\name{update_instruments.all}
\alias{update_instruments.all}
\alias{update_instruments.IB}
\title{updates instrument metadata with data from IB}
\usage{
  update_instruments.all(Symbols = "all", ...)

  update_instruments.IB(Symbols = c("all", "stocks",
  "futures", "options", "currencies"), addIBslot = TRUE,
  updateInstrument = TRUE, include_expired = "1", assign_i
  = TRUE, assign_c = TRUE)
}
\arguments{
  \item{Symbols}{can be a vector of instrument names, or,
  can be \sQuote{all} or \sQuote{stocks} or, can also be
  \sQuote{futures}, \sQuote{options}, \sQuote{currencies}}

  \item{addIBslot}{Boolean. should an IB slot be added to
  the instrument, making it a twsInstrument?}

  \item{updateInstrument}{Boolean. Should data outside the
  IB slot also be updated?}

  \item{include_expired}{Should expired contracts be
  included in reqContractDetails call? "0" for no, "1" for
  yes (default).}

  \item{assign_i}{should the instrument be stored in
  .instrument environment.}

  \item{assign_c}{If a new currency is discovered, should
  it be created}
}
\value{
  called for side-effect
}
\description{
  Adds/updates information in instrument with data
  downloaded from IB
}
\details{
  These are basically wrappers for buildIBcontract. With
  these functions you can update some or all instruments'
  information from IB.

  if you call update_instruments.IB with one of
  \sQuote{all} or \sQuote{stocks}, \sQuote{futures},
  \sQuote{options}, \sQuote{currencies}, it is the same as
  calling it with the relevant ls_ function (e.g.
  ls_stocks()). Therefore, functionality can be extended by
  using ls_ functions instead of a descriptive string.
}
\examples{
\dontrun{
stock('GS',currency('USD'))
update_instruments.IB('GS')
getInstrument("GS")
}
}
\author{
  Garrett See
}
\seealso{
  FinancialInstrument:::update_instruments.yahoo,
  twsInstrument, define_stocks
  \url{www.interactivebrokers.com} IB API
  \url{http://interactivebrokers.com/php/apiUsersGuide/apiguide.htm}?
}

