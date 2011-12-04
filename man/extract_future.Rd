\name{extract_future}
\alias{extract_future}
\title{Extract root \code{future} from \code{future_series}}
\usage{
  extract_future(primary_id, assign_i = FALSE, ...)
}
\arguments{
  \item{primary_id}{of future_series object}

  \item{assign_i}{should the future instrument be stored in
  the \code{.instrument} environment?}

  \item{...}{any other parameters to pass through to
  \code{instrument}}
}
\value{
  a \code{\link[FinancialInstrument]{future}} object unless
  called with \code{assign_i=TRUE} in which case the
  \code{future} will be stored and only the
  \code{primary_id} will be returned.
}
\description{
  Construct a \code{future} from a \code{future_series}
}
\details{
  Use this if you have defined a future_series object using
  \code{define_futures}, or deleted the root \code{future}
  and you would like to define the root.
}
\examples{
\dontrun{
s <- front_future("ES")
rm_futures("ES") #delete the root that front_future automatically created
extract_future(s)
extract_future(s, extra_field="custom") # can add any extra arbitrary fields
}
}
\author{
  Garrett See
}

