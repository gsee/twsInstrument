\name{getContract}
\alias{getContract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get a twsContract object
}
\description{
get a twsContract object
}
\usage{
getContract(x, verbose=TRUE, silent=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
can be an instrument, twsInstrument, name of an instrument or twsInstrument, or a
numeric or character \sQuote{conId}
}
  \item{verbose}{
be verbose?
}
  \item{silent}{
silence warnings?
}
  \item{...}{
any additional arguments to pass to \code{\link{Contr_From_Instr}}
}
}
\details{
If \code{x} is a twsInstrument, or if it can find a twsInstrument with the same name as \code{x} in the .instrument 
environment, it will return the twsContract found in the $IB slot of that object.  If not, then if \code{x}
is numeric, or if it is a character string of numbers, it will treat that as the conId in a newly created 
twsContract shell.  Then it will call \code{\link{reqContractDetails}} to get the contract object.  Otherwise, 
\code{x} will be passed to \code{\link{Contr_From_Instr}} which will use what information it can to create a
twsContract shell that can/will be updated with \code{\link{reqContractDetails}} 
}
\value{
an object of class twsContract
}
\author{
Garrett See
}
\note{
To ensure you get the contract you're after, you should define your instruments before using this function.
}
\seealso{
\code{\link{conId}},
\code{\link{define_stocks}}, \code{\link{define_options}}, \code{\link{define_futures}},
\code{\link{Contr_From_Instr}}, \code{\link{twsInstrument}},
\code{\link{getInstrument}}
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
}
}
