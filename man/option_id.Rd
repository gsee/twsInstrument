\name{option_id}
\alias{option_id}

\title{
make an option_id
}
\description{
create a primary_id for an option
}
\usage{
option_id(underlying_id, expiry, right, strike)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{underlying_id}{
ticker symbol of underying. character string.
}
  \item{expiry}{
exiry
}
  \item{right}{
call or put
}
  \item{strike}{
  strike price
}
}
\details{
puts the inputs into a string formatted like the primary_id of an option
}
\value{
primary_id of a \code{right} option on \code{underying_id} expiring on \code{expiry} struck at \code{strike} 
}
\author{
Garrett See
}
\examples{
option_id('SPY','20110630','C',130)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
