\name{has.Mid}
\alias{has.Mid}
\alias{Mid}
\title{Return Mid column / check to see if data contains Mid column}
\usage{
  has.Mid(x, which = FALSE)

  Mid(x)
}
\arguments{
  \item{x}{xts object}

  \item{which}{display position of match}
}
\value{
  \code{Mid} returns a univariate xts object \code{has.Mid}
  returns logical (boolean)
}
\description{
  \code{Mid} returns the Mid column of an xts object
  \code{has.Mid} checks to see if an xts object has a Mid
  column
}
\note{
  These should be exported from quantmod.
}
\author{
  gsee
}
\seealso{
  Op, Hi, Lo, Cl, Bid, Ask, has.Op, has.Cl
}

