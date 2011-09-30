\name{eWrapper.FXdata}
\alias{eWrapper.FXdata}
\title{eWrapper for exchange_rate instruments...}
\usage{
  eWrapper.FXdata(n)
}
\arguments{
  \item{n}{number of contracts being watched}
}
\value{
  list of functions
}
\description{
  eWrapper for exchange_rate instruments
}
\details{
  This is \code{eWrapper.data} modified to exclude the
  Last, LastSize, and Volume columns because IBrokers does
  not disseminate FX trade data.
}
\seealso{
  \code{eWrapper.data}
}

