\name{front_future}
\alias{front_future}
\alias{front_future.IB}
\title{instrument class constructor for front futures contract}
\usage{
  front_future(roots, currency = "USD", exchange = NULL,
  underlying_id = NULL, addIBslot = TRUE, src = "IB")

  front_future.IB(roots, currency = "USD", exchange = NULL,
  underlying_id = NULL, addIBslot = TRUE)
}
\arguments{
  \item{roots}{vector of chr strings; contract roots.}

  \item{currency}{name of currency; if more than one root,
  they all must have the same currency}

  \item{exchange}{optional character string name of
  exchange ("GLOBEX", "NYMEX", etc.)}

  \item{underlying_id}{name of underlying}

  \item{addIBslot}{should an IB slot be added to the
  instrument; i.e. do you want to create a twsInstrument.}

  \item{src}{currently only dQuoteIB is supported}
}
\value{
  called for side-effect. Returns the name of any
  instruments that were created.
}
\description{
  Create a future instrument for the front-month contract
  as well as specs for the root. Looks up contract details
  of a futures contract from IB and creates a futures
  instrument. The contract root will also be defined if it
  hasn't been defined yet.
}
\details{
  The contract details that IB gives are usually for the
  front month contract, but there is no guarantee that the
  contract will actually be the front contract.  (perhaps
  default_future would be a better name for this function?)

  This is not a very robust function.
  \code{define_futures} is probably a better alternative,
  but requires more user input.

  If you find that \code{front_future.IB} is defining
  something different than what you wanted (e.g. futures on
  Colgate Palmolive instead of Light Sweet Crude) try
  providing the \code{underlying_id}

  Currently, \code{front_future} is just a wrapper for
  \code{front_future.IB}. I.e. \dQuote{IB} is the only
  functional value for \code{src}

  using \code{root} and \code{currency} it will get the
  contract details of the front month future with a call to
  reqContractDetails. Using that info, it will create a
  future_series instrument, and a future instrument if it
  doesn't exist yet.
}
\examples{
\dontrun{
front_future('YM')
front_future('ES')
front_future('SPY')

#alternatively
#front_future(c("YM","ES","SPY"))
#or,
#unlist(lapply(c("YM","ES","SPY"), front_future))
}
}
\author{
  Garrett See
}
\seealso{
  define_futures, future, future_series,
  update_instruments.IB, Instr_From_Contr,
  option_series.yahoo
}

