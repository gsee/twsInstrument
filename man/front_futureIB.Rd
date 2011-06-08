\name{front_future.IB}
\alias{front_future.IB}
\title{
instrument class constructor for futures
}
\description{
Create a future instrument for the front-month contract as well as specs for the root.
Looks up contract details of the front-month contract from IB and creates a futures instrument. 
The contract root will also be defined if it hasn't been defined yet.
}
\usage{
front_future.IB(root, currency = "USD", underlying_id = NULL, addIBslot=TRUE)
}
\arguments{
  \item{root}{
chr string; contract root.
}
  \item{currency}{
name of currency
}
  \item{underlying_id}{
name of underlying
}
  \item{addIBslot}{
should an IB slot be added to the instrument; i.e. do you want to create a twsInstrument.
}
}
\details{
using \code{root} and \code{currency} it will get the contract details of the front month future with a call
to reqContractDetails. Using that info, it will create a future_series instrument, and a future instrument if it doesn't exist yet.
}
\value{
called for side-effect.
Returns the name of any instruments that were created.
}
\author{
Garrett See
}
\seealso{
future, future_series, update_instruments.IB, Instr_From_Contr, option_series.yahoo
}
\examples{
\dontrun{
front_future.IB('YM')
front_future.IB('ES')
front_future.IB('SPY')
}
}

