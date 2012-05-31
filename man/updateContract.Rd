\name{updateContract}
\alias{updateContract}
\title{update the IB attribute of a \code{\link{twsInstrument}}}
\usage{
  updateContract(x, add.identifier = TRUE, ...)
}
\arguments{
  \item{x}{character primary_id of an instrument}

  \item{add.identifier}{logical. If the conId has changed,
  should a \dQuote{old.conId} identifier be added to the
  instrument?}

  \item{...}{arguments to pass to
  \code{\link{update_instruments.IB}}.}
}
\value{
  character name of instrument
}
\description{
  If a Contract is no longer valid, this will request the
  new contract
}
\details{
  Interactive Brokers occationally changes the conId that
  is associated with a given instrument (for example, after
  a split, they change the conId). This function will check
  to make sure that the Contract that
  \code{\link{getContract}} finds is still valid.  If not,
  it will request the new Contract and store it in the IB
  attribute of the instrument.

  This is a very simple function
}
\examples{
\dontrun{
ibak <- as.list(FinancialInstrument:::.instrument) #backup instrument envir
rm_instruments()
define_stocks("SPY")
contr <- getContract("SPY")
contr$conId <- "0000000"
instrument_attr("SPY", "IB", contr)
getContract("SPY") #bad conId
updateContract("SPY")
getInstrument("SPY") #updated
## Restore previous instrument environment
rm_instruments(keep=FALSE)
loadInstruments(ibak)
}
}
\author{
  Garrett See
}

