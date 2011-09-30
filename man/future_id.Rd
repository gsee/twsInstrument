\name{future_id}
\alias{future_id}
\title{Create future_series ids...}
\usage{
  future_id(root, month, year)
}
\arguments{
  \item{root}{character vector of names of contract roots}

  \item{month}{numeric vector of months (3 letter
  abbreviations also work)}

  \item{year}{numeric vector of years}
}
\value{
  character vector of primary_ids for future_series
  instruments, OR, if \code{root} is missing, a character
  vector of suffix_ids.
}
\description{
  Create future_series ids...
}
\examples{
future_id(root=c("ES","YM"), month=c(3,6,9,12), year=2010:2011)
#getSymbols(future_id("VX",1:12,10:11),src='cfe')
future_id('VX',M2C(),10)
#future_id('VX',paste(M2C(),collapse=','),10)
}
\author{
  Garrett See
}

