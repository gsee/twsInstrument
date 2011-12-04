\name{addIBslot}
\alias{addIBslot}
\title{Create a twsContract and store it in a slot of an instrument}
\usage{
  addIBslot(symbols, sectype = "STK", exch = "SMART",
    primary = "", expiry = "", strike = "0.0",
    currency = "USD", right = "", local = "",
    multiplier = "", include_expired = "0", conId = 0)
}
\arguments{
  \item{symbols}{vector of character names of
  FinancialInstrument instruments}

  \item{sectype}{type of security}

  \item{exch}{exchange}

  \item{primary}{primary exchange of security}

  \item{expiry}{expiry}

  \item{strike}{strike price}

  \item{currency}{requested currency}

  \item{right}{right}

  \item{local}{local security name}

  \item{multiplier}{contract multiplier}

  \item{include_expired}{should expired contracts be
  included}

  \item{conId}{IB contract ID}
}
\value{
  Called for it's side effect
}
\description{
  Creates a twsContract object, and slot in the
  corresponding instrument, and places the object in the
  slot.
}
\details{
  This is an old function that is not terribly useful
  anymore.  It should probably be deprecated It is
  essentially a wrapper for twsContract, but it doesn't
  check to make sure the twsContract is valid.

  The instrument must be defined before calling this
  function.

  adds twsInstrument class to the instrument
}
\note{
  TODO: create instrument if it doesn't already exist
}
\examples{
\dontrun{
currency('USD')
stock('XOM','USD',1)
addIBslot('XOM')

# This is BETTER:
stock("CVX","USD")
instrument_attr("CVX","IB",getContract("CVX"))
# the above will add the twsInstrument class
# if you have FinancialInstrument version 0.7.2 or greater

# So is this:
twsInstrument(stock("BP","USD")) #will update other details of instrument by default
}
}
\author{
  Garrett See
}
\seealso{
  see instead: \code{\link{twsInstrument}} twsContract,
  instrument
}

