\name{conId}
\alias{conId}
\title{methods to get conId}
\usage{
  conId(x, ...)
}
\arguments{
  \item{x}{The name of a twsInstrument, or an object of
  type twsInstrument, or twsContract.}

  \item{...}{other arguments}
}
\value{
  character conId
}
\description{
  currently defined methods for default, character,
  twsInstrument, and twsContract
}
\details{
  conId.twsContract
}
\note{
  unless you call \code{conId} with a \code{twsContract} or
  \code{twsInstrument}, it will call getContract which
  will, as a last resort, download the contract details
  from IBrokers.
}
\examples{
\dontrun{
stk <- twsInstrument("GOOG")
conId(stk)
conId(getInstrument(stk))
conId(getContract(stk))

conId(stock("AAPL","USD")) #uses reqContractDetails
}
}
\author{
  gsee
}
\seealso{
  \code{\link{twsInstrument}}, \code{\link{twsContract}}
}

