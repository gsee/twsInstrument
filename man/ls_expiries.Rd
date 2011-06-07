\name{ls_expiries}
\alias{ls_expiries}
\alias{ls_expires}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
show unique expiration dates of instruments
}
\description{
show unique expiration dates of instruments
}
\usage{
ls_expiries(underlying_id = NULL, type = "derivative")
ls_expires(underlying_id = NULL, type = "derivative")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{underlying_id}{
chr name of underlying. If NULL, all underlyings will be used
}
  \item{type}{
chr string name of class that instruments to be returned must inherit.
}
}
\details{
\code{ls_expires} is an alias. (plural of expires?)

type is currently only implemented for sQuote{derivative}, sQuote{future}, sQuote{option}, sQuote{call} and sQuote{put}
internally, a call is made to the appropriate ls_ function.
}
\value{
named chr vector with length of unique expiration dates of derivatives of class \code{type} and having an underlying_id of \code{underlying_id} if given. 
}
\author{
Garrett
}
\note{
This should be updated to deal with dates instead of character strings
}
\seealso{
ls_instruments_by for things like e.g. ls_instruments_by('expires','20110916')
ls_instruments, ls_derivatives, ls_options, ls_calls
buildHierarchy, instrument.table
}
\examples{
\dontrun{
option_series.yahoo('SPY')
option_series.yahoo('DIA',NULL)
ls_expiries()
}
}

