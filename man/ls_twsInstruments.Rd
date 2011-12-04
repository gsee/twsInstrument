\name{ls_twsInstruments}
\alias{ls_defined.by}
\alias{ls_IB}
\alias{ls_non_twsInstruments}
\alias{ls_twsInstruments}
\alias{ls_yahoo}
\alias{rm_non_twsInstruments}
\alias{rm_twsInstruments}
\title{display the names of or delete twsInstruments.}
\usage{
  ls_twsInstruments(pattern = NULL, match = TRUE)

  ls_non_twsInstruments(pattern = NULL)

  ls_yahoo(pattern = NULL)

  ls_IB(pattern = NULL)

  ls_defined.by(x, pattern = NULL)

  rm_twsInstruments(x, keep.currencies = TRUE)

  rm_non_twsInstruments(x, keep.currencies = TRUE)
}
\arguments{
  \item{pattern}{an optional regular expression.  Only
  names matching \code{pattern} are returned.}

  \item{match}{return only exact matches?}

  \item{x}{For the rm_ functions: x is what to remove. if
  not supplied all instruments of relevent class will be
  removed For \code{ls_defined.by} x is the string
  describing how the instrument was defined.}

  \item{keep.currencies}{If TRUE, currencies will not be
  deleted.}
}
\value{
  ls functions return vector of character strings
  corresponding to instruments of requested type rm
  functions are called for side-effect
}
\description{
  ls functions return the names of all the instruments of
  the class implied by the function name. rm functions
  remove the instruments of the class implied by the
  function name
}
\details{
  \code{rm_twsInstruments}, and
  \code{rm_non_twsInstruments} will not delete currencies
  unless the keep.currencies argument is FALSE.

  For the rm functions, x can be a vector of instrument
  names, or nothing.  If x is missing, all instruments of
  the relevant type will be removed.

  ls_yahoo and ls_IB look for instruments with
  \sQuote{yahoo} or \sQuote{IB} in the defined.by slot,
  respectively Equivalently, \code{ls_defined.by("yahoo")}
  or \code{ls_defined.by("IB")} could be used. Note that
  being defined.by IB does does not necessarily mean that
  the instrument is a twsInstrument (i.e. the details may
  have been updated by IB, without an IB slot being added.)

  Often is is useful to nest these functions.
}
\examples{
\dontrun{
rm_instruments(keep.currencies=FALSE) #remove everything from .instrument

# First, create some instruments
currency('USD')
currency('EUR')
currency('JPY')
#stocks
stock("S","USD")
stock('SPY','USD',1)
twsInstrument('XOM') #requires Interactive Brokers
twsInstrument(option(primary_id='XOM', currency='USD', multiplier=100,
    tick_size=0.01, expiry='201106', right='P', strike=80, underlying_id='XOM'))

# Now, the examples
ls_twsInstruments() #all instruments containing IB slot with twsContract object
ls_non_twsInstruments()

ls_defined.by('IB')
ls_IB() #same as above
ls_defined.by('IB', ls_options())

ls_yahoo()

rm_twsInstruments()
rm_yahoo()
rm_instruments() #remove all but currencies
rm_currencies()
}
}
\author{
  Garrett See
}
\seealso{
  ls_instruments, ls_currencies, ls_instruments_by, ls, rm,
  twsInstrument, instrument, stock, future, option,
  currency
}

