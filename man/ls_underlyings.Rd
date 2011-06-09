\name{ls_underlyings}
\alias{ls_underlyings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
show names of underlyings
}
\description{
shows names that are stored in the \code{underlying_id} slot of derivative instruments
}
\usage{
ls_underlyings(pattern = NULL, match=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pattern}{an optional regular expression.  Only names matching \sQuote{pattern} are returned.}
  \item{match}{require exact match?}
}
\details{
first calls \code{ls_derivatives}, then looks for unique \code{underlying_id}s. If no derivatives have been defined, nothing will be returned.
}
\value{
chr vector of names of unique \code{underlying_id}s
}
\author{
Garrett See
}
\seealso{
ls_instruments_by, ls_derivatives, ls_options, ls_futures
}
\examples{
\dontrun{
ls_underlyings()
}
}
