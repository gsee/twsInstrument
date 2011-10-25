\name{getContract}
\alias{getContract}
\title{get a twsContract object}
\usage{
  getContract(x, verbose = TRUE, silent = FALSE, ...)
}
\arguments{
  \item{x}{can be an instrument, twsInstrument, name of an
  instrument or twsInstrument, or a numeric or character
  \sQuote{conId}}

  \item{verbose}{be verbose?}

  \item{silent}{silence warnings?}

  \item{...}{any additional arguments to pass to
  \code{\link{Contr_From_Instr}}}
}
\value{
  an object of class twsContract
}
\description{
  get a twsContract object
}
\details{
  If \code{x} is a twsInstrument, or if there is a
  twsInstrument with the same name as \code{x} in the
  .instrument environment, it will return the twsContract
  found in the $IB field of that object.  If not, then if
  \code{x} is the name of a Symbol defined in the Symbol
  Lookup table (see
  \code{\link[quantmod]{setSymbolLookup}}) that has an
  "IBrokers" src, and a twsContract object stored under
  "Contract", that twsContract will be returned. Otherwise,
  \code{x} will be passed to \code{\link{Contr_From_Instr}}
  which will use what information it can to create a
  twsContract shell that can/will be updated with
  \code{\link{reqContractDetails}}

  If you want to force a request for contract details from
  IB (and ensure you are not getting a local copy of the
  twsContract), use \code{\link{Contr_From_Instr}} which
  will return the twsContract that
  \code{\link[IBrokers]{reqContractDetails}} returns
  without altering any local copies.  Alternatively, if you
  call \code{\link{rmContract}} before calling
  \code{getContract}, it will be forced to look it up (by
  calling \code{\link{Contr_From_Instr}} for you).
}
\note{
  To ensure you get the contract you're after, you should
  define your instruments before using this function.
}
\examples{
\dontrun{
getContract("4082282") #uses reqContractDetails
getContract(4082282) #same
twsInstrument('SEE')
getContract("SEE") # == .instrument$SEE$IB
getContract("DIA") # will use reqContractDetails if "DIA" is not already defined
instr <- getInstrument("SEE")
getContract(instr)
getContract(instr$conId) # will connect to IB and reqContractDetails
}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{conId}}, \code{\link{rmContract}},
  \code{\link{define_stocks}},
  \code{\link{define_options}},
  \code{\link{define_futures}},
  \code{\link{Contr_From_Instr}},
  \code{\link{twsInstrument}}, \code{\link{getInstrument}}
}

