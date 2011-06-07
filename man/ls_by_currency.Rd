\name{ls_by_currency}
\alias{ls_by_currency}
\alias{rm_by_currency}
\alias{ls_USD}
\alias{ls_AUD}
\alias{ls_GBP}
\alias{ls_CAD}
\alias{ls_EUR}
\alias{ls_JPY}
\alias{ls_CHF}
\alias{ls_HKD}
\alias{ls_SEK}
\alias{ls_NZD}

%AUD GBP CAD EUR JPY CHF HKD SEK NZD
\title{
shows or removes instruments of given currency denomination(s)
}
\description{
ls_ functions get names of instruments denominated in a given currency (or currencies) 
rm_ functions remove instruments of a given currency
}
\usage{
ls_by_currency(currency, pattern = NULL, match = TRUE, show.currencies = FALSE)
rm_by_currency(x,currency,keep.currencies=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{currency}{
chr vector of names of currency
}
  \item{pattern}{
an optional regular expression.  Only names matching \sQuote{pattern} are returned.
}
  \item{match}{
exact match?
}
  \item{show.currencies}{
include names of currency instruments in the returned names? 
}
  \item{keep.currencies}{
Do not delete currency instruments when deleting multiple instruments. 
}
  \item{x}{what to remove. chr vector.
}
}
\value{
ls_ functions return vector of instrument names
rm_ functions return invisible / called for side-effect.
}
\author{
Garrett See
}

\seealso{
ls_instruments, ls_currencies, rm_instruments, rm_currencies, twsInstrument, instrument
}
\examples{
\dontrun{
#First create instruments
currency('USD')
currency('CAD')
currency('GBP')
stock('CM','CAD')
stock('CNQ','CAD')
stock('BET','GBP')
stock('BARC','GBP')
stock('SPY','USD')
stock('DIA','USD')

#now the examples
ls_by_currency(c('CAD','GBP'))

ls_USD()
ls_CAD()

#2 ways to remove all instruments of a currency
rm_instruments(ls_USD()) 
#rm_instruments(ls_GBP(),keep.currencies=FALSE)
rm_by_currency( ,'CAD') 
#rm_by_currency( ,'CAD', keep.currencies=FALSE)
}
}
