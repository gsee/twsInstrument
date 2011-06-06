\name{addIBslot}
\alias{addIBslot}
\title{
Create a twsContract and store it in a slot of an instrument
}
\description{
Creates a twsContract object, and slot in the corresponding instrument, and places the object in the slot. 
}
\usage{
addIBslot(symbols, 
          sectype = "STK",
          exch = "SMART", 
          primary = "",
          expiry = "", 
          strike = "0.0", 
          currency = "USD", 
          right = "", 
          local = "", 
          multiplier = "", 
          include_expired = "0", 
          conId = 0)
}
\arguments{
  \item{symbols}{ vector of character names of FinancialInstrument instruments }
  \item{sectype}{type of security}
  \item{exch}{exchange}
  \item{primary}{primary exchange of security}
  \item{expiry}{expiry}
  \item{strike}{strike price}
  \item{currency}{requested currency}
  \item{right}{right}
  \item{local}{local security name}
  \item{multiplier}{contract multiplier}
  \item{include_expired}{should expired contracts be included}
  \item{conId}{IB contract ID}
}

\details{
It's essentially a wrapper for twsContract.  Currently, the instrument must be defined before a slot can be
added, although in the future this function may create the instrument if it does not already exist.
A currency should be defined before defining any other instrument.

adds twsInstrument class to the instrument
}
\value{Called for it's side effect}
\author{Garrett See}
\note{
TODO: create instrument if it doesn't already exist
}

\seealso{
twsContract, instrument
}
\examples{
\dontrun{
currency('USD')
stock('XOM','USD',1)
addIBslot('XOM')
}
}
