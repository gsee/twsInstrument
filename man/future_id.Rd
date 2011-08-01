\name{future_id}
\alias{future_id}
\title{Create future_series ids...}
\usage{future_id(root, month, year)}
\description{Create future_series ids}
\value{character vector of primary_ids for future_series instruments.}
\author{Garrett See}
\arguments{\item{root}{character vector of names of contract roots}
\item{month}{numeric vector of months (3 letter abbreviations also work)}
\item{year}{numeric vector of years}}
\examples{future_id(root=c("ES","YM"), month=c(3,6,9,12), year=2010:2011)
#getSymbols(future_id("VX",1:12,10:11),src='cfe')}
