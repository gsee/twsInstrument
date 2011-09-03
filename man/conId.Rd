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
\seealso{
\code{\link{twsInstrument}}, \code{\link{twsContract}}
}
\examples{
\dontrun{
stk <- twsInstrument("GOOG")
conId(stk)
conId(getInstrument(stk))
conId(getContract(stk))
}
}

