\name{getContract}
\alias{getContract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get a twsContract from twsInstrument
}
\description{
get twsContract
}
\usage{
getContract(primary_id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{primary_id}{name of twsInstrument}
}
\details{
returns the twsContract stored in the IB slot of an instrument
}
\value{
an object of class twsContract
}
\author{
Garrett See
}
\note{
Very simple function. In the future, I will update this to have an option to allow a call to Contr_From_Instr (which, is essentially an alias for buildIBcontract), if the contract can't be found locally.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Contr_From_Instr, getInstrument, twsInstrument
}
\examples{
\dontrun{
twsInstrument('SEE') #have to have a twsInstrument
getContract('SEE')
}
}