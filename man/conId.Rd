\name{conId}
\alias{conId}
\alias{conId.default}
\alias{conId.character}
\alias{conId.twsInstrument}
\alias{conId.twsContract}
\title{
methods to get conId
}
\description{
currently defined methods for default, character, twsInstrument, and twsContract
}
\usage{
conId(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The name of a twsInstrument, or an object of type twsInstrument, or twsContract.
}
}
\value{
character conId
}
\author{
gsee
}
\note{
unless you call \code{conId} with a \code{twsContract} or \code{twsInstrument}, it will call
getContract which will, as a last resort, download the contract details from IBrokers. 
}
\seealso{
\code{\link{twsInstrument}}, \code{\link{twsContract}}
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

