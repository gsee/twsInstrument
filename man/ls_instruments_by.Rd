\name{ls_instruments_by}
\alias{ls_instruments_by}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Subset names of instruments
}
\description{
list names of instruments that have an attribute that matches some value
}
\usage{
ls_instruments_by(what, value, pattern = NULL, match = TRUE, in.IBslot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{what}{
what attribute (e.g. currency, type, strike, etc.)
}
  \item{value}{
what value must the attribute have (e.g. \sQuote{EUR}, \sQuote{option}, 100, etc.)
}
  \item{pattern}{
only return instruments with \code{pattern} in the name
}
  \item{match}{
should pattern match names exactly?
}
  \item{in.IBslot}{
Is the attribute you're looking for stored inside the IBslot?
}

}
\details{
list instruments that have a given attribute level with a given value.
}
\value{
chr vector of instrument names
}
\author{
Garrett See
}
\seealso{
buildHierarchy, instrument.table, ls_instruments
}
\examples{
\dontrun{
ls_instruments_by('type','stock')
}
}
