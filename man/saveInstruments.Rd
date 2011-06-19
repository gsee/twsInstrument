\name{saveInstruments}
\alias{saveInstruments}
\alias{loadInstruments}
\title{
Save and Load the all instrument definitions
}
\description{
Saves (loads) the .instrument environment to (from) disk.
\code{loadInstruments} will overwrite your .instrument environment with the newly loaded one!
}
\usage{
saveInstruments(file_name = "MyInstruments", dir = "", extension = "RData")
loadInstruments(file_name = "MyInstruments", dir = "", extension = "RData", env = parent.frame())
}

\arguments{
  \item{file_name}{
What to name the file (name of file that holds a .instrument enviroment)
Does not include file extension.
}
  \item{dir}{
Directory of file (defaults to current working directory. ie. sQuote{})
}
  \item{extension}{
File extension of file. default is RData
}
  \item{env}{
What environment to put the newly loaded .instrument environment in. Default is parent.frame()
}
}
\details{
After you have defined a bunch of instruments, you can use \code{saveInstruments} to save the entire .instrument environment to disk.
}
\value{
Called for side-effect
}
\author{
Garrett See
}
\seealso{
save, load
load.instrument
define_stocks, define_futures, define_options (option_series.yahoo)
}
\examples{
\dontrun{
stock("SPY","USD",1)
saveInstruments()
loadInstruments()
}
}
