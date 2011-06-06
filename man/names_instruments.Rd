\name{ls_instruments}
\alias{ls_instruments}
\alias{ls_twsInstruments}
\alias{ls_non_twsInstruments}
\alias{ls_stocks}
\alias{ls_options}
\alias{ls_futures}
\alias{ls_currencies}
\alias{ls_non_currencies}
\alias{ls_exchange_rates}
\alias{ls_bonds}
\alias{ls_funds}
\alias{ls_spreads}
\alias{ls_guaranteed_spreads}
\alias{ls_synthetics}
\alias{ls_defined.by}
\alias{ls_hand}
\alias{ls_yahoo}
\alias{ls_IB}
\alias{ls_derivatives}
\alias{ls_non_derivatives}
\alias{ls_calls}
\alias{ls_puts}
\alias{rm_instruments}
\alias{rm_twsInstruments}
\alias{rm_stocks}
\alias{rm_options}
\alias{rm_futures}
\alias{rm_currencies}
\alias{rm_exchange_rates}
\alias{rm_bonds}
\alias{rm_funds}
\alias{rm_spreads}
\alias{rm_synthetics}
\alias{rm_derivatives}
\alias{rm_non_derivatives}
\alias{rm_non_twsInstruments}

\title{
display the names of or delete instruments, twsInstruments, 
stocks, options, futures, currencies, bonds, funds, spreads,
guaranteed_spreads, synthetics, derivatives, or non-derivatives.
}
\description{
ls functions return the names of all the instruments of the class implied by the function name.
rm functions remove the instruments of the class implied by the function name
}
\usage{
ls_stocks(pattern=NULL)
ls_options(pattern=NULL)
ls_futures(pattern=NULL)
ls_currencies(pattern=NULL)
ls_non_currencies(pattern=NULL, includeFX=TRUE)
ls_exchange_rates(pattern=NULL)
ls_bonds(pattern=NULL)
ls_funds(pattern=NULL)
ls_spreads(pattern=NULL)
ls_guaranteed_spreads(pattern=NULL)
ls_synthetics(pattern=NULL)
ls_derivatives(pattern=NULL)
ls_non_derivatives(pattern=NULL)
ls_instruments(pattern=NULL, match=TRUE)
ls_twsInstruments(pattern=NULL)
ls_non_twsInstruments(pattern=NULL)
ls_defined.by(x,pattern=NULL)
ls_yahoo(pattern=NULL)
ls_IB(pattern=NULL)
ls_hand(pattern=NULL)

rm_instruments(x, keep.currencies=TRUE)
rm_twsInstruments(x, keep.currencies=TRUE)
rm_non_twsInstruments(x, keep.currencies=TRUE)
rm_stocks(x)
rm_options(x)
rm_futures(x)
rm_currencies(x)
rm_exchange_rates(x)
rm_bonds(x)
rm_funds(x)
rm_spreads(x)
rm_synthetics(x)
rm_derivatives(x)
rm_non_derivatives(x, keep.currencies=TRUE)
}
\arguments{
  \item{pattern}{an optional regular expression.  Only names matching sQuote{pattern} are returned.}
  \item{match}{return only exact matches?}
  
  \item{x}{
For the rm_ functions: x is what to remove. if not supplied all instruments of relevent class will be removed
For \code{ls_defined.by} x is the string describing how the instrument was defined.
}  
  \item{keep.currencies}{If TRUE, currencies will not be deleted.}
  
  \item{includeFX}{should exchange_rates be included in ls_non_currencies results}
}
\value{
ls functions return vector of character strings corresponding to instruments of requested type
rm functions are called for side-effect
}
\details{
ls_instruments is the only one that has a match argument. The rest will be updated soon.

rm_instruments, rm_twsInstruments, and rm_non_derivatives will not delete currencies 
unless the keep.currencies argument is FALSE.

For the rm functions, x can be a vector of instrument names, or nothing. 
If x is missing, all instruments of the relevant type will be removed.  

ls_yahoo and ls_IB look for instruments with \sQuote{yahoo} or \sQuote{IB} in the defined.by slot, respectively 
Equivalently, \code{ls_defined.by("yahoo")} or \code{ls_defined.by("IB")} could be used. Note that being 
defined.by IB does does not necessarily mean that the instrument is a twsInstrument (i.e. the details may have
been updated by IB, without an IB slot being added.)

Often is is useful to nest these functions.
}
\author{
Garrett See
}
\seealso{
ls, rm, twsInstrument, instrument, stock, future, option, currency
}

\examples{
\dontrun{
rm_instruments(keep.currencies=FALSE) #remove everything from .instrument

# First, create some instruments
currency('USD')
currency('EUR')
currency('JPY')
#stocks
stock("SEE","USD")
stock("SE","USD")
stock("S","USD")
twsInstrument(twsSTK("GSE",exch="",currency="AUD")) 
define_stocks('TT','CAD')
stock('SPY','USD',1)
#derivatives
option('.SPY','USD',multiplier=100,expiry='201106', strike=130, right='P', underlying_id='SPY')
future('ES', 'USD', multiplier=50, expiry='201106', underlying_id='ES')
option('.ES','USD',multiplier=1, expiry='201106',strike=1350, right='C', underlying_id='ES')
#twsInstruments
twsInstrument('XOM') #requires Interactive Brokers
twsInstrument(option(primary_id='.XOM', currency='USD', multiplier=100,
    tick_size=0.01, expiry='201106', right='P', strike=80, underlying_id='XOM'))


# Now, the examples
ls_instruments() #all instruments
ls_instruments("SE") #only the one stock
ls_instruments("S", match=FALSE) #anything with "S" in name

ls_twsInstruments() #all instruments containing IB slot with twsContract object
ls_currencies()
ls_stocks() 
ls_options() 
ls_futures() 
ls_derivatives()
ls_non_derivatives()


ls_USD() #USD denominated instruments
#Nesting:
ls_USD(ls_derivatives()) #USD denominated derivatives
ls_derivatives(ls_USD()) #Same thing

ls_defined.by('IB', ls_options())

rm_options('.SPY')
rm_futures()
ls_instruments()
#rm_instruments('EUR') #Incorrect
rm_instruments('EUR', keep.currencies=FALSE) #remove the currency
rm_currencies('JPY') #or remove currency like this
ls_currencies()
rm_twsInstruments()
ls_instruments()

rm_instruments() #remove all but currencies
rm_currencies()

}
}

