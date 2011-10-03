\name{is.twsInstrument}
\alias{buildIBcontract}
\alias{Contr_From_Instr}
\alias{Instr_From_Contr}
\alias{is.twsInstrument}
\alias{twsInstrument}
\title{buildIBcontract and wrappers}
\usage{
  is.twsInstrument(x)

  Contr_From_Instr(instrument, tws = NULL, addIBslot =
  FALSE, updateInstrument = FALSE, output = c("contract",
  "symbol", "nothing", "instrument"), include_expired =
  "0", assign_i = FALSE, assign_c = TRUE, verbose = TRUE,
  silent = FALSE)

  Instr_From_Contr(contract, tws = NULL, addIBslot = FALSE,
  updateInstrument = TRUE, output = c("instrument",
  "symbol", "nothing", "contract"), include_expired = "0",
  assign_i = FALSE, assign_c = TRUE, verbose = TRUE, silent
  = FALSE)

  twsInstrument(symbol, tws = NULL, addIBslot = TRUE,
  updateInstrument = TRUE, output = c("symbol", "nothing",
  "instrument", "contract"), include_expired = "0",
  assign_i = TRUE, assign_c = TRUE, verbose = TRUE, silent
  = FALSE)

  buildIBcontract(symbol, tws = NULL, addIBslot = FALSE,
  updateInstrument = FALSE, output = c("contract",
  "instrument", "symbol", "nothing"), include_expired =
  "0", assign_i = FALSE, assign_c = TRUE, verbose = TRUE,
  silent = FALSE)
}
\arguments{
  \item{symbol}{An instrument, The name of an instrument, a
  twsContract, or a conId.}

  \item{tws}{twsconn object. Not required.}

  \item{addIBslot}{Boolean. Should an IB slot be created in
  the instrument object?)}

  \item{updateInstrument}{Should all the attributes of the
  instrument be updated with the information retrieved from
  IB?}

  \item{output}{what should be returned. one of
  'nothing','symbol','instrument','contract'}

  \item{include_expired}{is the requested contract
  expired?}

  \item{assign_i}{boolean. Should the instrument be stored
  in the instrument environment?}

  \item{assign_c}{boolean. If a currency isn't defined,
  should it be?}

  \item{verbose}{be verbose?}

  \item{silent}{silence warnings?}

  \item{instrument}{for wrapper, alias for symbol}

  \item{contract}{for wrapper, alias for symbol}

  \item{x}{what to test for is.twsInstrument}
}
\value{
  Usually called for its side-effect. It will return what
  is defined by the output argument. See details.
}
\description{
  create twsInstrument, or create twsContracts using
  previously defined FinancialInstruments, or create
  FinancialInstruments from previously defined twsContract
  see
}
\details{
  ?\sQuote{twsInstrument-package} for package help page.

  Using metadata that has already been defined for an
  instrument, create a twsContract object and fill in any
  missing information. Can either add an IB slot to the
  instrument, or update the entire instrument creating
  slots as needed.

  buildIBcontract is the main function; the rest are
  wrappers.

  symbol can be the name of an instrument, an instrument, a
  twsContract, twsInstrument or a conId (unique numeric
  contract identifier used by Interactive Brokers).  Using
  the information given, it will create an instrument and a
  twsContract.  It will then make a call to
  reqContractDetails to fill in any missing information.
  (If you give it a string and there is no instrument by
  that name, then: (a) If the string ends in a period, it
  will treat it as a currency pair using "USD" as the base
  currency. (b) if the string can be coerced to numeric, it
  will be used as the conId in a call to
  \code{\link{getContract}} (which in turn calls
  \code{\link{reqContractDetails}} to get a twsContract
  object.  (c) Otherwise, it will be passed to
  \code{\link{instrument.auto}} which will try to create an
  instrument that can be updated.  If the string is
  something other than the name of a \code{FUT},
  \code{OPT}, or \code{CASH} -- for example, if it is the
  name of a \code{STK} or \code{IND} -- it will be assumed
  that it is a \code{STK}.  If the request for contract
  details fails, it will be tried again as an \code{IND}
  (Note that if you want an index, but request a stock, it
  is more likely that you will get a stock of a different
  currency than that the request will fail. Therefore, you
  should always define your instruments first.  Wrapping
  the symbol with something like
  \code{synthetic('SPX','USD')} or
  \code{stock('SPY','USD')} will ensure that you get the
  \code{sectype} of \code{twsContract} you are after).

  if addIBslot is TRUE it will store the contract in the IB
  slot of the instrument (creating the slot if necessary.)
  If updateInstrument is TRUE it will add/replace
  information directly in the instrument object.

  It is recommended that you do not pass it a twsconn
  object, in which case it will create a temporary one. If
  you pass it a twsconn object you are more likely to
  encounter errors.  If you pass a connected twsconn object
  it will be disconnected after the request.

  twsInstrument is a wrapper that will create a
  twsInstrument classed object. By default, It creates a
  twsContract and an instrument (if necessary) and places
  the twsContract in the IB slot of the instrument. The
  twsInstrument class is automatically added to any
  instrument that has an IB slot.

  Instr_From_Contr is a wrapper to create an instrument
  using a twsContract. It does not create an IB slot or
  store the twsContract in the instrument by default, and
  therefore does not add the twsInstrument class to the
  instrument.

  Contr_From_Instr is a wrapper to create a twsContract
  using an instrument. By default, it does not make any
  changes to the instrument, and therefore does not add the
  twsInstrument class to it.

  Instr_From_Contr and Contr_From_Instr are essentially the
  same functions, but with different default outputs.

  \code{ouput} should be a character string describing what
  to return. Valid values are c(\sQuote{"nothing},
  \sQuote{symbol}, \sQuote{instrument}, \sQuote{contract}
}
\examples{
\dontrun{

twsInstrument('SPY') #assumes it's a stock

#Now something that isn't denominated in USD
twsInstrument(twsFUT(symbol='NIY',exch='GLOBEX',expiry='201109',
currency='JPY'))

#As a wrapper for instrument wrappers
Contr_From_Instr(synthetic('SPX','USD'))
Instr_From_Contr(twsFUT(symbol='ES',exch='GLOBEX',expiry='201112', currency='USD'))

buildIBcontract(twsSTK("GOOG"),updateInstrument=TRUE, addIBslot=FALSE, output='nothing')

stock('AAPL','USD')
buildIBcontract('AAPL', updateInstrument=FALSE) #uses instrument

ls(.instrument,all.names=TRUE)

}
}
\author{
  Garrett See
}
\seealso{
  instrument, twsContract, addIBslot,
}

